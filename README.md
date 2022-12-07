# thoron_thesis_reedcollege

Bureau of Labor Statistics
The dependent variable of interest that I gather from the BLS is the unemployment rate spanning the time period 1997 to 2022. For my analysis, I selected BLS data on a monthly basis covering 15 industries. Based on my needs, I merged 2 sets of BLS major industry data into one master dataset by the appropriate identifiers. In the master datasets, I have industry, year, quarter, month, and then the unemployment rate as variables. 
In order to start data collection, I went to the BLS website’s data retrieval tool found here: https://www.bls.gov/webapps/legacy/cpsatab14.htm. From this page on the BLS, I get household data on unemployed persons by industry, not seasonally adjusted. 
In order to pull data from the BLS, I utilized the BLS API with a personalized API key:
payload_unemployment1 <- list(
  'seriesid'=c('LNU04000000', 'LNU04032229', 'LNU04032230', 
'LNU04032231', 'LNU04032232',
               'LNU04032233', 'LNU04032234', 'LNU04032235', 
'LNU04032236', 'LNU04032237',
               'LNU04032238', 'LNU04032239', 'LNU04032240', 
'LNU04032241', 'LNU04032242',
               'LNU04035109', 'LNU04028615', 'LNU04035181'),
  'startyear'=1997,
  'endyear'=2001,
  'catalog'=FALSE,
  'calculations'=TRUE,
  'annualaverage'=TRUE,
  'registrationKey'='4c48322612414050a0db4c1a31a340f3')
response_unemployment1 <- blsAPI(payload_unemployment1, 2)
json_unemployment1 <- fromJSON(response_unemployment1)

Then I build a function to grab the specific datasets I would like and wrangle them into tidy format for easy analysis:
df_list_unemployment1 <- list()
for (i in 1:length(industry_names_unemployment)) {
  
  # use apiDF to read in the ith data frame 
  temp_unemployment1 <- apiDF(json_unemployment1$Results$series[[i]]$data)
  
  # Rename the industry column after pivoting to make the coding easier
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

There are two important footnotes to this data. The first is that both people with no previous work experience and those who last worked in the U. S. Armed Forces are included in the unemployed total. Second, the classifications for sectors changed in 2017 to be in line with the 2017 North American Industry Classification System (NAICS). This change in classification systems affects the comparability of data over time and is important to consider in our analysis. 
All three of the BLS datasets that I use come with data in a monthly and annual basis. In order to transform the data into a form that can be compared to the GDP data from the BEA which only comes in quarters, I transform the BLS data from monthly data to quarterly data. I do this through a simple average for each quarter of the year from monthly data. My quarters are Jan-Mar, Apr-June, July-Sep, and Oct-Dec. 

Bureau of Economic Analysis
In order to start collection of the GDP data, I visited the BEA website at https://www.bea.gov/data/gdp. From this webpage, I selected GDP by Industry. On the GDP by Industry page, I then selected Interactive Data and then Industry Tables. All of this takes me to a page for GDP data by industry which includes value added, gross output, intermediate inputs, and KLEMS. The data from this page can either be pulled up as in current or chained (real) dollars. From this page, I selected the orange “begin using data” button. This takes me to the Interactive Access to Industry Economic Accounts Data webpage reachable at this following link: https://apps.bea.gov/iTable/iTable.cfm?reqid=150&step=2&isuri=1&categories=gdpxind. For my analysis I have selected a dataset titled “Gross Output by Industry.” 
In order to analyze correlation coefficients for the unemployment rate and GDP data, I merge both the BLS and BEA master datasets into one grandmaster dataset for my analysis. 
