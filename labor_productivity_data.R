
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
industry_names <- c("total", "goods_producing",
                    "mining", "construction", "manufacturing", "durable_goods", 
                    "nondurable_goods", "wholesale", "retail", "transportation", 
                    "utilities", "information", "finance", "professional", 
                    "education_health", "leisure", "other")

## Pull the data via the API ---------------------------------------------------

payload_employees1 <- list(
  'seriesid'=c('CES0500000001', 'CES0600000001', 
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
all_employees <- bind_rows(df_list_employees1) 



# BLS SECTION: AVERAGE WEEKLY HOURS (awh) ======================================

## Pull the data via the API ---------------------------------------------------

payload_awh1 <- list(
  'seriesid'=c('CES0500000002', 'CES0600000002', 
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
  mutate(year = as.numeric(year))

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

df_list2<- list()
# iterate through the industry names
for (i in 1:length(industry_names)) {
  
  # The formula for gdp rate I'm using is: 
  # Real GDP growth rate = (most recent years real GDP - the last years real GDP) / the previous years real GDP
  employment_temp <- employment_data[employment_data$industry==industry_names[i],] %>%
    mutate(year = as.numeric(year),
           gdp = as.numeric(employment),
           numerator = employment - lag(employment), # Difference in route between years
           employ_rate = numerator/lag(employment) * 100)
  
  # set temp to be the ith element in the list
  df_list2[[i]] <- employment_temp
}

# row bind all of the elements in the list
employment_rate_data <- bind_rows(df_list2) %>%
  group_by(industry, year, quarter) %>%
  summarise(employment_rate = mean(employ_rate), awh = awh) %>%
  ungroup()

#employment_data <- employment_data %>%
  #left_join(employment_rate_data, by = c("year", "period", "periodName", "industry"))
  
  



# BEA SECTION ==================================================================

# For this section I pull data from BEA on Real Value Added by Industry
# [Billions of 2012 chain dollars]
# and I wrangle it to get Percent Change in Real Value Added by Industry

## Upload CSV ------------------------------------------------------------------

RGO <- read_csv("RGO.csv")

## Wrangling into Tidy Format --------------------------------------------------

# RGO_2 wrangling
rgo <- RGO %>%
  filter(!row_number() %in% c(99, 102:107))

colnames(rgo) = paste(sep ="", rgo[1,], colnames(rgo))

rgo_full <- rgo %>%
  rename("industry" = "NA...1") %>%
  slice(-1) %>%
  pivot_longer(cols = -1,                        #the -1 tells it to skip the first column
               names_to = c("quarter", "year"),  #this tells it where we want the data to go
               names_pattern = c("(..)(....)"),  #this says to split columns based on a pattern of the first 4 characters
               values_to = "gdp")                #then the next 2 characters, so the date and the quarter, ex: (2002)(Q1)

industies <- rgo_full %>%
  count(industry)

## Wrangling -------------------------------------------------------------------

bea1 <- rgo_full %>%
  filter(industry %in% c('Private industries',
                         'Private goods-producing industries2',
                         'Private services-producing industries3',
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
  mutate(industry = case_when(industry == "Private industries" ~ "total",
                              industry == "Private goods-producing industries2" ~ "goods_producing",
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
                              industry == "Other services, except government" ~ "other"))



# LABOR PRODUCTIVITY ===========================================================

labor_productivity_data <- lp_input %>% # data starts in 2006!
  mutate(input = awh*employment) %>%
  group_by(industry, year, quarter) %>%
  mutate(input = mean(input)) %>%
  ungroup() %>%
  left_join(bea2, by = c("year", "industry", "quarter")) %>%
  drop_na(gdp) %>%
  mutate(productivity = gdp/input) %>%
  select(industry, year, quarter, productivity) %>%
  unique() %>%
  drop_na(productivity)

write_xlsx(labor_productivity_data,"~/Thesis/labor_productivity_data.xlsx") 



# GGPLOTS ======================================================================

lp_1 <- labor_productivity_data %>%
  filter(year >= 2006 & year <= 2016) %>%
  group_by(industry) %>%
  summarize(meanlp = mean(productivity)) %>%
  mutate(period = "2006 - 2016")

lp_2 <- labor_productivity_data %>%
  filter(year >= 2017 & year <= 2022) %>%
  drop_na() %>%
  group_by(industry) %>%
  summarize(meanlp = mean(productivity)) %>%
  mutate(period = "2017 - 2022")

labor_production <- rbind(lp_1, lp_2)

# Labor Productivity
ggplot(labor_production, aes(x = industry, y = meanlp, fill = period)) +
  geom_col(position = "dodge") +
  labs(x = "Periods", y = "Labor Productivity", 
       title = "Labor Productivity by for Two Periods") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6))

ggplot(labor_productivity_data, aes(x = year, y = productivity, color = industry)) +
  geom_line() +
  labs(x = "Years", y = "Labor Productivity", 
       title = "Labor Productivity by Industry",
       subtitle = "2006 to 2022") 