
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
library(seasonal)

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



# BLS SECTION: UNEMPLOYMENT ====================================================

industry_names_unemployment <- c("total", "nonfarm", "mining", "construction", "manufacturing", 
                                 "durable_goods", "nondurable_goods", "wholesale_trade", 
                                 "transportation_utilities", "information", "financial_activities", 
                                 "professional_business_services", "education_health_services", 
                                 "leisure_hospitality", "other_services", "agriculture",
                                 "government", "self_employed")

## Pull the data via the API ---------------------------------------------------

payload_unemployment1 <- list(
  'seriesid'=c('LNU04000000', 'LNU04032229', 'LNU04032230', 'LNU04032231', 'LNU04032232',
               'LNU04032233', 'LNU04032234', 'LNU04032235', 'LNU04032236', 'LNU04032237',
               'LNU04032238', 'LNU04032239', 'LNU04032240', 'LNU04032241', 'LNU04032242',
               'LNU04035109', 'LNU04028615', 'LNU04035181'),
  'startyear'=1997,
  'endyear'=2001,
  'catalog'=FALSE,
  'calculations'=TRUE,
  'annualaverage'=TRUE,
  'registrationKey'='4c48322612414050a0db4c1a31a340f3')
response_unemployment1 <- blsAPI(payload_unemployment1, 2)
json_unemployment1 <- fromJSON(response_unemployment1)

payload_unemployment2 <- list(
  'seriesid'=c('LNU04000000', 'LNU04032229', 'LNU04032230', 'LNU04032231', 'LNU04032232',
               'LNU04032233', 'LNU04032234', 'LNU04032235', 'LNU04032236', 'LNU04032237',
               'LNU04032238', 'LNU04032239', 'LNU04032240', 'LNU04032241', 'LNU04032242',
               'LNU04035109', 'LNU04028615', 'LNU04035181'),
  'startyear'=2002,
  'endyear'=2022,
  'catalog'=FALSE,
  'calculations'=TRUE,
  'annualaverage'=TRUE,
  'registrationKey'='4c48322612414050a0db4c1a31a340f3')
response_unemployment2 <- blsAPI(payload_unemployment2, 2)
json_unemployment2 <- fromJSON(response_unemployment2)


## Create data list ------------------------------------------------------------

# initialize landing list
df_list_unemployment1 <- list()
for (i in 1:length(industry_names_unemployment)) {
  
  # use apiDF to read in the ith data frame 
  temp_unemployment1 <- apiDF(json_unemployment1$Results$series[[i]]$data)
  
  # rename the industry column after pivoting to make the coding easier
  temp_pivot_unemployment1 <- temp_unemployment1 %>% 
    pivot_longer(cols = "value",
                 names_to = "industry",
                 values_to = "unemployment_rate") %>% 
    mutate(unemployment_rate = as.numeric(unemployment_rate)) %>% 
    mutate(industry = industry_names_unemployment[i])
  
  # set temp to be the ith element in the list
  df_list_unemployment1[[i]] <- temp_pivot_unemployment1
}
bls1997 <- bind_rows(df_list_unemployment1)

df_list_unemployment2 <- list()
for (i in 1:length(industry_names_unemployment)) {
  
  # use apiDF to read in the ith data frame 
  temp_unemployment2 <- apiDF(json_unemployment2$Results$series[[i]]$data)
  
  # rename the industry column after pivoting to make the coding easier
  temp_pivot_unemployment2 <- temp_unemployment2 %>% 
    pivot_longer(cols = "value",
                 names_to = "industry",
                 values_to = "unemployment_rate") %>% 
    mutate(unemployment_rate = as.numeric(unemployment_rate)) %>% 
    mutate(industry = industry_names_unemployment[i])
  
  # set temp to be the ith element in the list
  df_list_unemployment2[[i]] <- temp_pivot_unemployment2
}
bls2002 <- bind_rows(df_list_unemployment2)

bls <- rbind(bls1997, bls2002) 

bls %>%
  count(industry)

## BLS Wrangling -------------------------------------------------------------------

bls <- bls %>%
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
                           period == "M12" ~ "Dec", 
                           period == "M13" ~ "Annual")) %>%
  select(year, month, industry, unemployment_rate)

# Here's a page I used to help me out: https://stat.ethz.ch/pipermail/r-help/2008-August/171301.html
bls$quarter <- character(length = NROW(bls))
bls$quarter[bls$month %in% month.abb[c(1:3)]] <- "Q1"
bls$quarter[bls$month %in% month.abb[c(4:6)]] <- "Q2"
bls$quarter[bls$month %in% month.abb[c(7:9)]] <- "Q3"
bls$quarter[bls$month %in% month.abb[c(10:12)]] <- "Q4"
bls$quarter <- factor(bls$quarter, levels = c("Q1","Q2","Q3","Q4"))

with(bls, aggregate(unemployment_rate, list(quarter = quarter, year =
                                              year), FUN = mean))

bls%>%
  count(industry)
bls <- bls %>%
  filter(industry %in% c("agriculture", "construction", "durable_goods",
                         "education_health_services", "financial_activities",
                         "government", "information","manufacturing",
                         "mining", "nondurable_goods", "professional_business_services",
                         "total","transportation_utilities", "wholesale_trade")) %>% #"transportation_utilities"
  filter(month != "Annual") %>%
  group_by(industry, year, quarter) %>%
  summarise(unemployment_rate_qu = mean(unemployment_rate)) %>%
  ungroup()

bls %>%
  count(industry)
  
bls1 <- bls %>%
  filter(industry == "transportation_utilities") %>%
  separate_rows(industry, sep = "_") %>%
  mutate(industry = case_when(industry == "transportation" ~ "transport_tu",
                              industry == "utilities" ~ "utilities_tu"))

bls2 <- bls %>%
  filter(industry != "transportation_utilities")

bls3 <- rbind(bls1, bls2) #%>% # final BLS data set used in meta_data
  #filter(year >= 2000)

bls3 %>%
  count(year)

industry_names_unemployment_2 <- c("agriculture", "construction", "durable_goods",
                                   "education_health_services", "financial_activities",
                                   "government", "information","manufacturing",
                                   "mining", "nondurable_goods", "professional_business_services",
                                   "total", "wholesale_trade")

# function for seasonally adjusting 
df_seasonal <- list()
for (i in industry_names_unemployment_2) {
  
  bls_ud <- bls3[bls3$industry==i,] %>%
    select(unemployment_rate_qu)
  
  ts_bls = ts(bls_ud, frequency = 4, start = 2000, end = 2005)
  
  decompose_bls = decompose(ts_bls, "multiplicative")
  adjust_bls = ts_bls - decompose_bls$seasonal 
  plot(adjust_bls)
  
  df_seasonal[[i]] <- adjust_bls
}
seasonal_adjustments <- bind_rows(df_seasonal)
  




# BEA SECTION ==================================================================

# For this section I pull data from BEA on Real Value Added by Industry
# [Billions of 2012 chain dollars]
# and I wrangle it to get Percent Change in Real Value Added by Industry

## Upload CSV ------------------------------------------------------------------

RVA_1 <- read_csv("~/Thesis/RVA_1.csv")
RVA_2 <- read_csv("~/Thesis/RVA_2.csv")

## Wrangling into Tidy Format --------------------------------------------------

# RVA_1 wrangling
rva_1 <- RVA_1 %>%
  select(-1) %>%
  filter(!row_number() %in% c(99, 102:107)) %>%
  rename("industry" = "...2") %>%
  drop_na() %>%
  pivot_longer(cols = -1,                        #the -1 tells it to skip the first column
               names_to = c("year"),
               values_to = "gdp") %>%
  add_column(quarter = NA)
  
# RVA_2 wrangling
rva_2 <- RVA_2 %>%
  select(-1) %>%
  filter(!row_number() %in% c(99, 102:107))

colnames(rva_2) = paste(sep ="", rva_2[1,], colnames(rva_2))

rva_2 <- rva_2 %>%
  rename("industry" = "NA...2") %>%
  slice(-1) %>%
  pivot_longer(cols = -1,                        #the -1 tells it to skip the first column
               names_to = c("quarter", "year"),  #this tells it where we want the data to go
               names_pattern = c("(..)(....)"),  #this says to split columns based on a pattern of the first 4 characters
               values_to = "gdp")                #then the next 2 characters, so the date and the quarter, ex: (2002)(Q1)

rva_full <- rbind(rva_1, rva_2)

rva_full %>%
  count(year)

## Wrangling -------------------------------------------------------------------

bea1 <- rva_full %>%
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

industry_names_bea <- c("agriculture", "construction", "durable_goods",
                        "education_health_services", "financial_activities",
                        "government", "information","manufacturing",
                        "mining", "nondurable_goods", "professional_business_services",
                        "total", "transport_tu", "utilities_tu", "wholesale_trade")

df_list1 <- list()
# iterate through the industry names
for (i in 1:length(industry_names_bea)) {
  
  # The formula for gdp rate I'm using is: 
  # Real GDP growth rate = (most recent years real GDP - the last years real GDP) / the previous years real GDP
  bea_temp <- bea1[bea1$industry==industry_names_bea[i],] %>%
    mutate(year = as.numeric(year),
           gdp = as.numeric(gdp),
           numerator = gdp - lag(gdp), # Difference in route between years
           gdp_rate = numerator/lag(gdp) * 100)
  
  # set temp to be the ith element in the list
  df_list1[[i]] <- bea_temp
}

# row bind all of the elements in the list
bea2 <- bind_rows(df_list1) 

bea2 %>%
  count(industry)

# MEGA DATASET =================================================================

mega_correlations <- employment_data %>%
  mutate(year = as.numeric(year)) %>%
  select(year, industry, quarter, employment_rate) %>%
  left_join(bea2, by = c("industry", "year", "quarter"))

mega_data <- bls3 %>%
  mutate(year = as.numeric(year)) %>%
  left_join(bea2, by = c("industry", "year", "quarter")) %>%
  drop_na()
  #group_by(industry) %>%
  #mutate(lag_1 = lag(gdp_rate, 1),
         #lag_2 = lag(gdp_rate, 2)) %>%
  #ungroup()

write_xlsx(mega_data,"~/Thesis/mega_data.xlsx") 


m <- seas(AirPassengers)
m <- seas(AirPassengers, regression.variables = c("td", "ao1955.jan"))

view(AirPassengers)

library(seasonalview)
view(m)

library(fpp)
data(ausbeer)
ts_bls = ts(ausbeer, frequency = 4, start = 1956)

decompose_bls = decompose(ts_bls, "additive")
adjust_bls = ts_bls - decompose_bls$seasonal 
plot(adjust_bls)

bls3 <- bls3 %>%
  seas(
    x = unemployment_rate_qu,
    x11 = "",
    arima.model = "(0 1 1)"
  )
