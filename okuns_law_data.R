
# Emma Thoron 
# with help from Josie Griffin '12 and Josh Yamamoto '23
# Sept. - Dec. 2022
# Thesis Data Collection: Okun's Law



## Libraries ===================================================================

library(rjson)
library(blsAPI)
library(dplyr)
library(tidyverse)
library(tidyr)
library(readr)
library(writexl)
#library(padr)
#library(tframePlus)



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

industry_names_unemployment <- c("total", "mining", "construction", "manufacturing", 
                                 "durable_goods", "nondurable_goods", "wholesale_retail",  "transportation_utilities", 
                                 "information", "financial", "professional", "education_health", 
                                 "leisure", "other", "agriculture", "government")

## Pull the data via the API ---------------------------------------------------

payload_unemployment1 <- list(
  'seriesid'=c('LNU04000000', 'LNU04032230', 'LNU04032231', 'LNU04032232',
               'LNU04032233', 'LNU04032234', 'LNU04032235', 'LNU04032236', 
               'LNU04032237', 'LNU04032238', 'LNU04032239', 'LNU04032240', 
               'LNU04032241', 'LNU04032242', 'LNU04035109', 'LNU04028615'),
  'startyear'=1997,
  'endyear'=2001,
  'catalog'=FALSE,
  'calculations'=TRUE,
  'annualaverage'=TRUE,
  'registrationKey'='4c48322612414050a0db4c1a31a340f3')
response_unemployment1 <- blsAPI(payload_unemployment1, 2)
json_unemployment1 <- fromJSON(response_unemployment1)

payload_unemployment2 <- list(
  'seriesid'=c('LNU04000000', 'LNU04032230', 'LNU04032231', 'LNU04032232',
               'LNU04032233', 'LNU04032234', 'LNU04032235', 'LNU04032236', 
               'LNU04032237', 'LNU04032238', 'LNU04032239', 'LNU04032240', 
               'LNU04032241', 'LNU04032242', 'LNU04035109', 'LNU04028615'),
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
                 values_to = "unemploy_rate") %>% 
    mutate(unemploy_rate = as.numeric(unemploy_rate)) %>% 
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
                 values_to = "unemploy_rate") %>% 
    mutate(unemploy_rate = as.numeric(unemploy_rate)) %>% 
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
  select(year, month, industry, unemploy_rate)

# Here's a page I used to help me out: https://stat.ethz.ch/pipermail/r-help/2008-August/171301.html
bls$quarter <- character(length = NROW(bls))
bls$quarter[bls$month %in% month.abb[c(1:3)]] <- "Q1"
bls$quarter[bls$month %in% month.abb[c(4:6)]] <- "Q2"
bls$quarter[bls$month %in% month.abb[c(7:9)]] <- "Q3"
bls$quarter[bls$month %in% month.abb[c(10:12)]] <- "Q4"
bls$quarter <- factor(bls$quarter, levels = c("Q1","Q2","Q3","Q4"))

with(bls, aggregate(unemploy_rate, list(quarter = quarter, year =
                                              year), FUN = mean))

bls <- bls %>%
  filter(month != "Annual") %>%
  group_by(industry, year, quarter) %>%
  summarise(unemployment_rate = mean(unemploy_rate)) %>%
  ungroup()

bls_wr <- bls %>%
  filter(industry == "wholesale_retail") %>%
  separate_rows(industry, sep = "_") 

bls_tu <- bls %>%
  filter(industry == "transportation_utilities") %>%
  separate_rows(industry, sep = "_")

bls_others <- bls %>%
  filter(industry != "wholesale_retail" & industry != "transportation_utilities")

bls_full <- rbind(bls_wr, bls_tu, bls_others)


  
# BEA SECTION ==================================================================

# For this section I pull data from BEA on Real Value Added by Industry
# [Billions of 2012 chain dollars]
# and I wrangle it to get Percent Change in Real Value Added by Industry

## Upload CSV ------------------------------------------------------------------

RVA_1 <- read_csv("RVA_1.csv")
RVA_2 <- read_csv("RVA_2.csv")

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

rva <- rbind(rva_1, rva_2)

rvaindustry <- rva %>%
  count(industry)

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
                         'Other services, except government',
                         'Agriculture, forestry, fishing, and hunting',
                         'Government')) %>%
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
                              industry == "Other services, except government" ~ "other",
                              industry == "Agriculture, forestry, fishing, and hunting" ~ "agriculture",
                              industry == "Government" ~ "government"))

rva_select %>%
  count(industry)
industry_names_bea <- c("total", "mining", "construction", "manufacturing", 
                        "durable_goods", "nondurable_goods", "wholesale", "retail",  
                        "transportation", "utilities", "information", "financial", 
                        "professional", "education_health", "leisure", "other", 
                        "agriculture", "government")

df_list1 <- list()
# iterate through the industry names
for (i in 1:length(industry_names_bea)) {
  
  # The formula for gdp rate I'm using is: 
  # Real GDP growth rate = (most recent years real GDP - the last years real GDP) / the previous years real GDP
  bea_temp <- rva_select[rva_select$industry==industry_names_bea[i],] %>%
    mutate(year = as.numeric(year),
           gdp = as.numeric(gdp),
           numerator = gdp - lag(gdp), # Difference in route between years
           gdp_rate = numerator/lag(gdp) * 100)
  
  # set temp to be the ith element in the list
  df_list1[[i]] <- bea_temp
}

# row bind all of the elements in the list
rva_full <- bind_rows(df_list1) 

rva_full %>%
  count(industry)

# MEGA DATASET =================================================================

okunslawdata <- bls_full %>%
  mutate(year = as.numeric(year)) %>%
  left_join(rva_full, by = c("industry", "year", "quarter")) %>%
  drop_na() %>%
  arrange(industry) %>%
  select(!numerator) %>%
  mutate(industry_num = case_when(industry == "total" ~ "0", 
                                  industry == "mining" ~ "1",
                                  industry == "construction" ~ "2",
                                  industry == "manufacturing" ~ "3",
                                  industry == "durable_goods" ~ "4",
                                  industry == "nondurable_goods" ~ "5",
                                  industry == "wholesale" ~ "6",
                                  industry == "retail" ~ "7",
                                  industry == "transportation" ~ "8",
                                  industry == "utilities" ~ "9",
                                  industry == "information" ~ "10",
                                  industry == "financial" ~ "11",
                                  industry == "professional" ~ "12",
                                  industry == "education_health" ~ "13",
                                  industry == "leisure" ~ "14",
                                  industry == "other" ~ "15",
                                  industry == "agriculture" ~ "16",
                                  industry == "government" ~ "17"))

write_xlsx(okunslawdata,"~/thoron_thesis_reedcollege/okunslawdata.xlsx") 

okunslawdata_agg <- okunslawdata %>%
  filter(industry == "total") %>%
  group_by(industry, year) %>%
  summarise(unemployment_rate = mean(unemployment_rate),
            gdp_rate = mean(gdp_rate))

okunslawdata_other <- okunslawdata %>%
  filter(industry == "construction" |
         industry == "leisure" |
         industry == "government") %>%
  group_by(industry, year) %>%
  summarise(unemployment_rate = mean(unemployment_rate),
            gdp_rate = mean(gdp_rate)) %>%
  pivot_wider(names_from = industry, values_from = c(year, unemployment_rate, gdp_rate))



