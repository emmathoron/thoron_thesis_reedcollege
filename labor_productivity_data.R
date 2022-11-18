
# Emma Thoron, Josie Griffin
# October 2022
# Thesis Data Collection



## Libraries ===================================================================

library(rjson)
library(blsAPI)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(tframePlus)
library(writexl)
library(ggplot2)
library(ggthemes)
library(kableExtra)
library(padr)



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
industry_names_employment <- c("nonfarm", "private", "goods_producing", "service_providing", "private_service_providing",
                               "mining", "construction", "manufacturing", "durable_goods", "nondurable_goods",
                               "trade_transport_utilities", "wholesale_trade", "retail_trade", "transportation_warehousing",
                               "utilities", "information", "financial_activities", "professional_business_services",
                               "education_health_services", "leisure_hospitality", "other_services", "government")

## Pull the data via the API ---------------------------------------------------

payload_employees1 <- list(
  'seriesid'=c('CES0000000001', 'CES0500000001', 'CES0600000001',
               'CES0700000001', 'CES0800000001', 'CES1000000001',
               'CES2000000001', 'CES3000000001', 'CES3100000001',
               'CES3200000001', 'CES4000000001', 'CES4142000001',
               'CES4200000001', 'CES4300000001', 'CES4422000001',
               'CES5000000001', 'CES5500000001', 'CES6000000001',
               'CES6500000001', 'CES7000000001', 'CES8000000001',
               'CES9000000001'),
  'startyear'=1981,
  'endyear'=2001,
  'catalog'=FALSE,
  'calculations'=TRUE,
  'annualaverage'=TRUE,
  'registrationKey'='4c48322612414050a0db4c1a31a340f3')
response_employees1 <- blsAPI(payload_employees1, 2)
json_employees1 <- fromJSON(response_employees1)

payload_employees2 <- list(
  'seriesid'=c('CES0000000001', 'CES0500000001', 'CES0600000001',
               'CES0700000001', 'CES0800000001', 'CES1000000001',
               'CES2000000001', 'CES3000000001', 'CES3100000001',
               'CES3200000001', 'CES4000000001', 'CES4142000001',
               'CES4200000001', 'CES4300000001', 'CES4422000001',
               'CES5000000001', 'CES5500000001', 'CES6000000001',
               'CES6500000001', 'CES7000000001', 'CES8000000001',
               'CES9000000001'),
  'startyear'=2002,
  'endyear'=2022,
  'catalog'=FALSE,
  'calculations'=TRUE,
  'annualaverage'=TRUE,
  'registrationKey'='4c48322612414050a0db4c1a31a340f3')
response_employees2 <- blsAPI(payload_employees2, 2)
json_employees2 <- fromJSON(response_employees2)

## Create data list ------------------------------------------------------------

# 1981 - 2001
df_list_employees1 <- list()
for (i in 1:length(industry_names_employment)) {
  
  # use apiDF to read in the ith data frame 
  temp_employment <- apiDF(json_employees1$Results$series[[i]]$data)
  
  # rename the industry column after pivoting to make the coding easier
  temp_pivot_employment <- temp_employment %>% 
    pivot_longer(cols = "value",
                 names_to = "industry",
                 values_to = "employment") %>% 
    mutate(employment = as.numeric(employment)) %>% 
    mutate(industry = industry_names_employment[i])
  
  # set temp to be the ith element in the list
  df_list_employees1[[i]] <- temp_pivot_employment
}
all_employees1 <- bind_rows(df_list_employees1) 

# 2002 - 2022
df_list_employees2 <- list()
for (i in 1:length(industry_names_employment)) {
  
  # use apiDF to read in the ith data frame 
  temp_employment <- apiDF(json_employees2$Results$series[[i]]$data)
  
  # rename the industry column after pivoting to make the coding easier
  temp_pivot_employment <- temp_employment %>% 
    pivot_longer(cols = "value",
                 names_to = "industry",
                 values_to = "employment") %>% 
    mutate(employment = as.numeric(employment)) %>% 
    mutate(industry = industry_names_employment[i])
  
  df_list_employees2[[i]] <- temp_pivot_employment # set temp to be the ith element in the list
}
all_employees2 <- bind_rows(df_list_employees2) 

all_employees <- rbind(all_employees1, all_employees2) # bind employees data together



# BLS SECTION: AVERAGE WEEKLY HOURS (awh) ======================================

## Function for renaming multiple data frames ----------------------------------

# vector of industry names specifically ordered so that industry_names[i] corresponds to json$Results$series[[i]]
industry_names_awh <- c("private", "goods_producing", "service_providing", "private_service_providing",
                        "mining", "construction", "manufacturing", "durable_goods", "nondurable_goods",
                        "trade_transport_utilities", "wholesale_trade", "retail_trade", "transportation_warehousing",
                        "utilities", "information", "financial_activities", "professional_business_services",
                        "education_health_services", "leisure_hospitality", "other_services")

## Pull the data via the API ---------------------------------------------------

payload_awh1 <- list(
  'seriesid'=c('CES0500000002', 'CES0600000002', 'CES0800000002', 'CES1000000002',
               'CES3000000002', 'CES3100000002', 'CES3200000002', 'CES4000000002', 
               'CES4142000002', 'CES4200000002', 'CES4300000002', 'CES4422000002',
               'CES5000000002', 'CES5500000002', 'CES6000000002', 'CES6500000002',
               'CES7000000002', 'CES8000000002'),
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
for (i in 1:length(industry_names_awh)) {
  
  # use apiDF to read in the ith data frame 
  temp_awh <- apiDF(json_awh1$Results$series[[i]]$data)
  
  # rename the industry column after pivoting to make the coding easier
  temp_pivot_awh <- temp_awh %>% 
    pivot_longer(cols = "value",
                 names_to = "industry",
                 values_to = "awh") %>% 
    mutate(awh = as.numeric(awh)) %>% 
    mutate(industry = industry_names_awh[i])
  
  # set temp to be the ith element in the list
  df_list_awh1[[i]] <- temp_pivot_awh
}
all_awh1 <- bind_rows(df_list_awh1) 

# bind employees data together
all_awh <- all_awh1

write_xlsx(all_awh,"~/Thesis/all_awh.xlsx")



# WRANGLING FOR LABOR PRODUCTIVITY =============================================

employment_data <- all_employees %>%
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
  select(year, month, industry, employment)
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
for (i in 1:length(industry_names_employment)) {
  
  # The formula for gdp rate I'm using is: 
  # Real GDP growth rate = (most recent years real GDP - the last years real GDP) / the previous years real GDP
  employment_temp <- employment_data[employment_data$industry==industry_names_employment[i],] %>%
    mutate(year = as.numeric(year),
           gdp = as.numeric(employment),
           numerator = employment - lag(employment), # Difference in route between years
           employment_rate = numerator/lag(employment) * 100)
  
  # set temp to be the ith element in the list
  df_list2[[i]] <- employment_temp
}

# row bind all of the elements in the list
employment_data <- bind_rows(df_list2) 

all_employees <- all_employees %>%
  filter(industry != "nonfarm", industry != "government") %>%
  mutate(year = as.numeric(year))

lp_input <- all_awh %>%
  mutate(year = as.numeric(year)) %>%
  left_join(all_employees, by = c("year", "period", "periodName", "industry")) 

lp_input <- lp_input %>%
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
  select(year, month, industry, awh, employment)

lp_input$quarter <- character(length = NROW(lp_input))
lp_input$quarter[lp_input$month %in% month.abb[c(1:3)]] <- "Q1"
lp_input$quarter[lp_input$month %in% month.abb[c(4:6)]] <- "Q2"
lp_input$quarter[lp_input$month %in% month.abb[c(7:9)]] <- "Q3"
lp_input$quarter[lp_input$month %in% month.abb[c(10:12)]] <- "Q4"
lp_input$quarter <- factor(lp_input$quarter, levels = c("Q1","Q2","Q3","Q4"))

with(lp_input, aggregate(c(employment, awh), list(quarter = quarter, year =
                                                    year), FUN = mean))


# BEA SECTION ==================================================================

# For this section I pull data from BEA on Real Value Added by Industry
# [Billions of 2012 chain dollars]
# and I wrangle it to get Percent Change in Real Value Added by Industry

## Upload CSV ------------------------------------------------------------------

RGO_1 <- read_csv("~/Thesis/bea9704.csv")
RGO_2 <- read_csv("~/Thesis/realgrossoutput.csv")

## Wrangling into Tidy Format --------------------------------------------------

# RGO_1 wrangling
rgo_1 <- RGO_1 %>%
  filter(!row_number() %in% c(99, 102:107)) %>%
  rename("industry" = "...1") %>%
  drop_na() %>%
  pivot_longer(cols = -1,                        #the -1 tells it to skip the first column
               names_to = c("year"),
               values_to = "gdp") %>%
  add_column(quarter = NA)

# RGO_2 wrangling
rgo_2 <- RGO_2 %>%
  filter(!row_number() %in% c(99, 102:107))

colnames(rgo_2) = paste(sep ="", rgo_2[1,], colnames(rgo_2))

rgo_2 <- rgo_2 %>%
  rename("industry" = "NA...1") %>%
  slice(-1) %>%
  pivot_longer(cols = -1,                        #the -1 tells it to skip the first column
               names_to = c("quarter", "year"),  #this tells it where we want the data to go
               names_pattern = c("(..)(....)"),  #this says to split columns based on a pattern of the first 4 characters
               values_to = "gdp")                #then the next 2 characters, so the date and the quarter, ex: (2002)(Q1)

rgo_full <- rbind(rgo_1, rgo_2)

## Wrangling -------------------------------------------------------------------

bea1 <- rgo_full %>%
  filter(industry %in% c('Agriculture, forestry, fishing, and hunting',
                         'All industries', 'Construction', 'Durable goods',
                         'Educational services, health care, and social assistance',
                         'Finance, insurance, real estate, rental, and leasing',
                         'Government', 'Information', 'Manufacturing', 
                         'Mining', 'Nondurable goods', 'Professional and business services',
                         'Transportation and warehousing', 
                         'Utilities', 'Wholesale trade')) %>%
  mutate(industry = case_when(industry == "Agriculture, forestry, fishing, and hunting" ~ "agriculture", 
                              industry == "Construction" ~ "construction",
                              industry == "Durable goods" ~ "durable_goods",
                              industry == "Educational services, health care, and social assistance" ~ "education_health_services",
                              industry == "Finance, insurance, real estate, rental, and leasing" ~ "financial_activities",
                              industry == "Government" ~ "government",
                              industry == "Information" ~ "information",
                              industry == "Manufacturing" ~ "manufacturing",
                              industry == "Mining" ~ "mining",
                              industry == "Nondurable goods" ~ "nondurable_goods",
                              industry == "Professional and business services" ~ "professional_business_services",
                              industry == "All industries" ~ "total", 
                              industry == "Transportation and warehousing" ~ "transport_tu",
                              industry == "Utilities" ~ "utilities_tu",
                              industry == "Wholesale trade" ~ "wholesale_trade"))



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