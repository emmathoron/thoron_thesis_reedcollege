
# Emma Thoron 
# with help from Josie Griffin '12 and Josh Yamamoto '23
# Sept. - Dec. 2022
# Thesis Data Collection: Labor Productivity



## Libraries ===================================================================

library(rjson)
library(blsAPI)
library(dplyr)
library(tidyverse)
library(tidyr)
library(readr)
library(writexl)
library(ggthemes)



# BLS SECTION FUNCTION =========================================================

## Function and Process results ------------------------------------------------

apiDF <- function(data){
  df <- data.frame(
    year = character(),
    period = character(),
    periodName = character(),
    value = character(),
    stringsAsFactors = FALSE
  )
  i <- 0
  for(d in data){
    i <- i + 1
    df[i,] <- unlist(d)
  }
  return(df)
}



# BLS SECTION EMPLOYMENT =======================================================

## Function for renaming multiple data frames ----------------------------------

# vector of industry names specifically ordered so that industry_names[i] corresponds to json$Results$series[[i]]
industry_names <- c("total", 
                    "mining", "construction", "manufacturing", "durable_goods", 
                    "nondurable_goods", "wholesale", "retail", "transportation", 
                    "utilities", "information", "financial", "professional", 
                    "education_health", "leisure", "other")

## Pull the data via the API ---------------------------------------------------

payload_employees1 <- list(
  'seriesid'=c('CES0500000001',
               'CES1000000001', 'CES2000000001', 'CES3000000001', 'CES3100000001',
               'CES3200000001', 'CES4142000001', 'CES4200000001', 'CES4300000001', 
               'CES4422000001', 'CES5000000001', 'CES5500000001', 'CES6000000001',
               'CES6500000001', 'CES7000000001', 'CES8000000001'),
  'startyear'=2006,
  'endyear'=2022,
  'catalog'=FALSE,
  'calculations'=TRUE,
  'annualaverage'=TRUE,
  'registrationKey'='4c48322612414050a0db4c1a31a340f3')
response_employees1 <- blsAPI(payload_employees1, 2)
json_employees1 <- fromJSON(response_employees1)

## Create data list ------------------------------------------------------------

# 2006 - 2022
df_list_employees1 <- list()
for (i in 1:length(industry_names)) {
  
  # use apiDF to read in the ith data frame 
  temp_employment <- apiDF(json_employees1$Results$series[[i]]$data)
  
  # rename the industry column after pivoting to make the coding easier
  temp_pivot_employment <- temp_employment %>% 
    pivot_longer(cols = "value",
                 names_to = "industry",
                 values_to = "employment") %>% 
    mutate(employment = as.numeric(employment)) %>% 
    mutate(industry = industry_names[i])
  
  # set temp to be the ith element in the list
  df_list_employees1[[i]] <- temp_pivot_employment
}
all_employees <- bind_rows(df_list_employees1) %>%
  drop_na(employment) %>%
  arrange(industry, year, period) 



# BLS SECTION: AVERAGE WEEKLY HOURS (awh) ======================================

## Pull the data via the API ---------------------------------------------------

payload_awh1 <- list(
  'seriesid'=c('CES0500000002', 
               'CES1000000002','CES2000000002', 'CES3000000002', 'CES3100000002', 
               'CES3200000002', 'CES4142000002', 'CES4200000002', 'CES4300000002', 
               'CES4422000002', 'CES5000000002', 'CES5500000002', 'CES6000000002',
               'CES6500000002', 'CES7000000002', 'CES8000000002'),
  'startyear'=2006,
  'endyear'=2022,
  'catalog'=FALSE,
  'calculations'=TRUE,
  'annualaverage'=TRUE,
  'registrationKey'='4c48322612414050a0db4c1a31a340f3')
response_awh1 <- blsAPI(payload_awh1, 2)
json_awh1 <- fromJSON(response_awh1)

## Create data list ------------------------------------------------------------

# 2006 - 2022
df_list_awh1 <- list()
for (i in 1:length(industry_names)) {
  
  # use apiDF to read in the ith data frame 
  temp_awh <- apiDF(json_awh1$Results$series[[i]]$data)
  
  # rename the industry column after pivoting to make the coding easier
  temp_pivot_awh <- temp_awh %>% 
    pivot_longer(cols = "value",
                 names_to = "industry",
                 values_to = "awh") %>% 
    mutate(awh = as.numeric(awh)) %>% 
    mutate(industry = industry_names[i])
  
  # set temp to be the ith element in the list
  df_list_awh1[[i]] <- temp_pivot_awh
}
all_awh <- bind_rows(df_list_awh1) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(industry, year, period)



# MERGE ========================================================================

all_both <- all_employees %>%
  mutate(year = as.numeric(year)) %>%
  left_join(all_awh, by = c("year", "period", "periodName", "industry"))

# WRANGLING FOR LABOR PRODUCTIVITY =============================================

employment_data <- all_both %>%
  mutate(month = case_when(period == "M01" ~ "Jan", 
                           period == "M02" ~ "Feb",
                           period == "M03" ~ "Mar",
                           period == "M04" ~ "Apr",
                           period == "M05" ~ "May",
                           period == "M06" ~ "Jun",
                           period == "M07" ~ "Jul",
                           period == "M08" ~ "Aug",
                           period == "M09" ~ "Sep",
                           period == "M10" ~ "Oct",
                           period == "M11" ~ "Nov",
                           period == "M12" ~ "Dec")) %>%
  select(!period) %>%
  select(!periodName)

employment_data$quarter <- character(length = NROW(employment_data))
employment_data$quarter[employment_data$month %in% month.abb[c(1:3)]] <- "Q1"
employment_data$quarter[employment_data$month %in% month.abb[c(4:6)]] <- "Q2"
employment_data$quarter[employment_data$month %in% month.abb[c(7:9)]] <- "Q3"
employment_data$quarter[employment_data$month %in% month.abb[c(10:12)]] <- "Q4"
employment_data$quarter <- factor(employment_data$quarter, levels = c("Q1","Q2","Q3","Q4"))
with(employment_data, aggregate(c(employment), list(quarter = quarter, year =
                                                      year), FUN = mean))

employment_data <- employment_data %>%
  group_by(industry, year, quarter) %>%
  summarise(employment = mean(employment),
            awh = mean(awh)) %>%
  ungroup() %>%
  mutate(input = awh*employment)
#weekly rate (annual rate = multiply by 52)



# BEA SECTION ==================================================================

# For this section I pull data from BEA on Real Gross Output by Industry
# [Billions of 2012 chain dollars]
# and I wrangle it to get Percent Change in Real Gross Output by Industry

## Upload CSV ------------------------------------------------------------------

RVA <- read_csv("RVA_2.csv")

## Wrangling into Tidy Format --------------------------------------------------

# RVA_2 wrangling
rva <- RVA %>%
  select(-1) %>%
  filter(!row_number() %in% c(99, 102:107))

colnames(rva) = paste(sep ="", rva[1,], colnames(rva))

rva <- rva %>%
  rename("industry" = "NA...2") %>%
  slice(-1) %>%
  pivot_longer(cols = -1,                        #the -1 tells it to skip the first column
               names_to = c("quarter", "year"),  #this tells it where we want the data to go
               names_pattern = c("(..)(....)"),  #this says to split columns based on a pattern of the first 4 characters
               values_to = "gdp")                #then the next 2 characters, so the date and the quarter, ex: (2002)(Q1)

industies <- rva %>%
  count(industry)

years <- rva %>%
  count(year)


## Wrangling -------------------------------------------------------------------

rva_select <- rva %>%
  filter(industry %in% c('Gross domestic product',
                         'Mining',
                         'Construction', 
                         'Manufacturing', 
                         'Durable goods',
                         'Nondurable goods',
                         'Wholesale trade',
                         'Retail trade',
                         'Transportation and warehousing', 
                         'Utilities',
                         'Information',
                         'Finance, insurance, real estate, rental, and leasing',
                         'Professional and business services',
                         'Educational services, health care, and social assistance',
                         'Arts, entertainment, recreation, accommodation, and food services',
                         'Other services, except government')) %>%
  mutate(industry = case_when(industry == "Gross domestic product" ~ "total", 
                              industry == "Mining" ~ "mining",
                              industry == "Construction" ~ "construction",
                              industry == "Manufacturing" ~ "manufacturing",
                              industry == "Durable goods" ~ "durable_goods",
                              industry == "Nondurable goods" ~ "nondurable_goods",
                              industry == "Wholesale trade" ~ "wholesale",
                              industry == "Retail trade" ~ "retail",
                              industry == "Transportation and warehousing" ~ "transportation",
                              industry == "Utilities" ~ "utilities",
                              industry == "Information" ~ "information",
                              industry == "Finance, insurance, real estate, rental, and leasing" ~ "financial",
                              industry == "Professional and business services" ~ "professional",
                              industry == "Educational services, health care, and social assistance" ~ "education_health",
                              industry == "Arts, entertainment, recreation, accommodation, and food services" ~ "leisure",
                              industry == "Other services, except government" ~ "other")) %>%
  mutate(year = as.numeric(year), 
         gdp = as.numeric(gdp))


# LABOR PRODUCTIVITY ===========================================================

labor_productivity_data <- employment_data %>% # data starts in 2006!
  left_join(rva_select, by = c("year", "industry", "quarter")) %>%
  mutate(productivity = gdp/input) %>%
  drop_na()
#write_xlsx(labor_productivity_data_2,"~/thoron_thesis_reedcollege/laborproductivitydata.xlsx") 



# INDEX ========================================================================

df_list_pp <- list()
# iterate through the industry names
for (i in 1:length(industry_names)) { # industry_names should still work
  
  base <- labor_productivity_data[labor_productivity_data$industry==industry_names[i],] %>%
    filter(year == 2006, quarter == "Q2" ) %>%
    #since it's seasonally adjusted, it's okay to use a quarter
    select(productivity)
  
  base_productivity <- base$productivity[]
  
  labor_productivity_data_temp <- labor_productivity_data[labor_productivity_data$industry==industry_names[i],] %>%
    select(industry, year, quarter, productivity) %>%
    unique() %>%
    drop_na(productivity) %>%
    mutate(pp_index = (productivity/base_productivity)*100)
  
  # set temp to be the ith element in the list
  df_list_pp[[i]] <- labor_productivity_data_temp
}

# row bind all of the elements in the list
indexed_data <- bind_rows(df_list_pp)

indexed_data_test <- indexed_data %>%
  filter(year == 2006, quarter == "Q2" )



# GGPLOT DATA ==================================================================

indexed_data_2 <- indexed_data %>%
  filter(industry == "total") %>%
  group_by(industry, year) %>%
  summarise(productivity = mean(productivity),
            pp_index = mean(pp_index)) %>%
  ungroup() 

indexed_data %>%
  count(industry)


# GGPLOTS ======================================================================
library(RColorBrewer)
library(lattice)

yearly_labor_productivity_data <- indexed_data %>%
  group_by(industry, year) %>%
  summarise(yearly_lp = mean(pp_index))

ggplot(yearly_labor_productivity_data, aes(x = year, y = yearly_lp)) +
  geom_line() +
  facet_wrap(~industry) +
  labs(x = "Years", y = "Labor Productivity Index") +
  theme_stata(scheme = "s2color") +
  scale_colour_stata("s1color")

colVec<-c(brewer.pal(10,"Set3"),brewer.pal(6,"Set3"))
ltyVec<-rep(c("solid","dashed"),c(10,6))

ggplot(yearly_labor_productivity_data, aes(x = year, y = yearly_lp, 
                                           color = industry)) +
  geom_line() +
  labs(x = "Years", y = "Labor Productivity Index") +
  theme_stata(scheme = "s2color") +
  scale_colour_stata("s1color") +
  scale_linetype_manual(values = c(rep("solid", 8), rep("dashed", 8))) +
  scale_color_manual(values = c(brewer.pal(8, "Paired"), brewer.pal(8, "Dark2")))




