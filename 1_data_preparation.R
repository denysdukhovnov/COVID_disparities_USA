# This sequence of scripts accompanies the analysis in the paper
# "County-level socioeconomic disparities in COVID-19 mortality in the United States"
# by Denys Dukhovnov and Magali Barbieri.
# 
# Last modified on December 9, 2021
# Author: Denys Dukhovnov

## Set random number generator for later simulations 
set.seed(12345)

options(scipen = 99999,                     # Switch off the scientific notation for decimal numbers
        tidyverse.quiet = TRUE,             # Disable excessive messages, warnings will remain unmuted
        dplyr.summarise.inform = FALSE)


## Install the packages by uncommenting and running the relevant lines, if not already installed
# install.packages("sf")
# install.packages("tigris")
# install.packages("tidyverse")


## Load the packages to be used at various points throughout the analysis
library(sf)
library(tigris)
library(tidyverse)


## 0. Set the working directory (if different from the current), and source the utility
##    functions that will be used at various points throughout the analysis
##    NOTE: Check utility_functions.R script for the detailed code and description of each function's mechanics

## VERY IMPORTANT: Store the input data in the directory called "Input data" inside the working directory set above
setwd(".")

# Create the directories for the output tables and plots
if (!dir.exists("Plots")) dir.create("Plots")
if (!dir.exists("Output tables")) dir.create("Output tables")

# Import utility functions to be used in this analysis
source("utility_functions.R")


# Create state FIPS, code, and name dictionary to facilitate data linkage later
state.dictionary <- bind_cols(
  State.Code = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL",
                "GA","HI","ID","IL","IN","IA","KS","KY","LA","ME",
                "MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
                "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
                "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI",
                "WY", "PR"),
  
  State.Name = c("Alabama","Alaska","Arizona","Arkansas","California",
                   "Colorado","Connecticut","Delaware","District of Columbia",
                   "Florida","Georgia","Hawaii","Idaho","Illinois","Indiana",
                   "Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland",
                   "Massachusetts","Michigan","Minnesota","Mississippi",
                   "Missouri","Montana","Nebraska","Nevada","New Hampshire",
                   "New Jersey","New Mexico","New York","North Carolina",
                   "North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
                   "Rhode Island","South Carolina","South Dakota","Tennessee",
                   "Texas","Utah","Vermont","Virginia","Washington",
                   "West Virginia","Wisconsin","Wyoming", "Puerto Rico"),
  
  State.FIPS = c("01","02","04","05","06","08","09","10","11","12","13","15",
                  "16","17","18","19","20","21","22","23","24","25","26","27",
                  "28","29","30","31","32","33","34","35","36","37","38","39",
                  "40","41","42","44","45","46","47","48","49","50","51","53",
                  "54","55","56","72")
)


## 1. Read and process she static version of John Hopkins data (static time series as of September 22, 2021),
##    OR uncomment the "data.link" and "download.file()" rows below to download the most recent version of time series.

# Download the running total of daily death as time series from GitHub and process it by removing extraneous 
# columns and values (if any) thus leaving only the running total of deaths by county by date, excluding 
# dates after December 31, 2020. 
#
# NOTE: The current version of John Hopkins COVID-19 time series is well-organized, and may not require much
# processing as done below. However, this code below will also work on older archived versions of the data that are 
# much more haphazardly organized.
# Please note that the latter option may include newer data and the final results may differ from those reported in the paper.

# data.link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
# download.file(data.link, "Input data/time_series_covid19_confirmed_US.csv")

df.list <- read.csv("Input data/time_series_covid19_confirmed_US.csv") %>% 
  rename(CombinedKey = Combined_Key) %>% 
  mutate(FIPS = if_else(is.na(FIPS), "", if_else(nchar(as.character(FIPS)) == 4, formatC(FIPS, width = 5, flag = "0"), as.character(FIPS)))) %>% 
  filter(!is.na(CombinedKey), FIPS != "") %>%  
  mutate(CombinedKey = gsub(",US|, US", "", CombinedKey),
         CombinedKey = gsub("^,", "", CombinedKey),
         CombinedKey = gsub("\\>,\\<", ", ", CombinedKey)) %>%
  filter(grepl(",", CombinedKey), !grepl("Unassigned", CombinedKey), !grepl("Out of", CombinedKey)) %>%
  mutate(State = sub("^(.*?,\\s)(.)", "\\2", CombinedKey),
         County = sub("^(.*?),.*", "\\1", CombinedKey)) %>% 
  select(FIPS, State, County, starts_with("X")) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "CalendarDate", values_to = "Deaths") %>% 
  mutate(CalendarDate = gsub("\\.", "\\/", CalendarDate),
         CalendarDate = gsub("X", "", CalendarDate),
         CalendarDate = paste0(CalendarDate, "20"),
         CalendarDate = as.Date.character(CalendarDate, format = "%m/%d/%y")) %>% 
  filter(CalendarDate <= as.Date("2020-12-31"), CalendarDate >= as.Date("2020-02-29")) 


## 2. Split the data frame into a list where each element is a daily cumulative total COVID-19 deaths by county
##    to compute the daily differences.
if (any(class(df.list) != "list")) df.list <- split(df.list, f = df.list$CalendarDate)



## 3. Due to John Hopkins focusing on reporting the cumulative daily deaths, some small inconsistencies occurred,
##    such that there may be several fewer deaths in more recent time than a day or week earlier. John Hopkins team 
##    occasionally issues retroactive updates to adjust for these inconsistencies, but some may still remain.
##
##    As such, taking the most recent data as the "gold standard", the data is to be patched by sequential
##    backpropagation, starting from the end of the series, to ensure that no day before has a greater total than the day after
##    NOTE: This may take about half a minute to a minute to run
df.list.patched <- list()
df.list.patched[(length(df.list) - 1):length(df.list)] <- df.list[(length(df.list) - 1):length(df.list)]
for (i in seq(length(df.list.patched) - 1, 1, -1)) {
  df.list.patched[[i]] <- JH.covid.df.fun(df = df.list.patched, df.index = i+1) %>% 
    left_join(JH.covid.df.fun(df = df.list, df.index = i) %>% select(FIPS, JH.deaths, CalendarDate), by = "FIPS") %>% 
    mutate(Deaths = if_else(JH.deaths.x < JH.deaths.y, JH.deaths.x, JH.deaths.y)) %>% 
    rename(CalendarDate = CalendarDate.y) %>% 
    select(FIPS, State, County, CalendarDate, Deaths)
}


## 4. Read and process the county SES data with overall deaths by county for 2015-2019.
##    County deaths for were averaged for 2015-2019 to prevent annual fluctuations due to flu, etc.
##    
##    Users interested in the computation of SES classification for counties are referred to the published report:
##      Barbieri, Magali, November 2020. Report on "Mortality by Socioeconomic Category in the United States", Society of Actuaries,
##      Mortality and Longevity Strategic Research Program Steering Committee, 46 p.
deaths.SES <- read.csv("Input data/deaths_by_county_SES_2015_2019.csv", stringsAsFactors = F, as.is = T, 
                       colClasses = c("Y2015" = "numeric", "Y2016" = "numeric", "Y2017" = "numeric", "Y2018" = "numeric", "Y2019" = "numeric")) %>% 
  rowwise() %>% 
  mutate(Deaths.15.19 = mean(c(Y2015, Y2016, Y2017, Y2018, Y2019), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(FIPS = sprintf("%05.0f", as.numeric(FIPS))) %>% 
  select(FIPS, Deaths.15.19, SES.Quintile)


## 5. Read the CDC national weekly deaths by sex and age group, (downloaded on September 22, 2021, data last updated by CDC on September 22, 2021).
##    OR for the latest data, uncomment the block below and input your CDC API credentials to download the data directly from CDC.
##    Note that the latter option may include newer data and the final results may differ from those reported in the paper.

# install.packages("RSocrata")
# library(RSocrata)
# # List your API credentials here in quotes
# token <- ""
# email <- ""
# password <- ""
# 
# # Download the data
# CDC_COVID_national_rates_weekly <- read.socrata(url = "https://data.cdc.gov/resource/vsak-wrfu.json",
#                                                 app_token = token,
#                                                 email =  email,
#                                                 password = password) %>%
#    mutate(week_ending_date = as.Date.character(week_ending_date, "%Y-%m-%d"),
#           mmwr_week = as.factor(mmwr_week)) %>%
#    filter(week_ending_date <= "2021-01-02")
# 
# # Save the most up-to-date file of weekly rates
# write.csv(CDC_COVID_national_rates_weekly, "Input data/CDC_COVID_national_rates_weekly.csv", row.names = F)
# 
# # Clean up password and other credentials from the global environment
# rm(password, token, email)

CDC_COVID_national_rates_weekly <- 
  read.csv("Input data/CDC_COVID_national_rates_weekly.csv", 
           stringsAsFactors = F) %>%
  mutate(week_ending_date = as.Date.character(week_ending_date, "%Y-%m-%d"),
         mmwr_week = as.factor(mmwr_week))

# Extract age labels for later use
age.labels <- CDC_COVID_national_rates_weekly %>%
  summarise(age.labels = unique(age_group)) %>% 
  unlist() %>% 
  unname() %>% 
  str_to_sentence()


## 6. Read the 2017 population in county quintiles by age and sex, as the mid-interval denominator for 2015-2019 deaths average,
##    downloaded on September 23, 2021 from https://www.cdc.gov/nchs/nvss/bridged_race/data_documentation.htm#Vintage2020
##    Aggregate the single years of age by 10-year wide age groups for both sexes together
pop.2017 <- read.csv("Input data/vintage2020_county_population_2017.csv", stringsAsFactors = F, as.is = T, colClasses = c(FIPS = "character")) %>% 
  mutate(TOTAL = rowSums(.[2:173]),
         pop0 = rowSums(.[2:3]),                          
         pop1_4 = rowSums(.[4:11]),
         pop5_14 = rowSums(.[12:31]),
         pop15_24 = rowSums(.[32:51]),
         pop25_34 = rowSums(.[52:71]),
         pop35_44 = rowSums(.[72:91]),
         pop45_54 = rowSums(.[92:111]),
         pop55_64 = rowSums(.[112:131]),
         pop65_74 = rowSums(.[132:151]),
         pop75_84 = rowSums(.[152:171]),
         pop85plus = rowSums(.[172:173])) %>% 
  mutate(State.FIPS = substr(FIPS, 1, 2)) %>% 
  pivot_longer(cols = TOTAL:pop85plus,
               names_to = "Age.Group",
               values_to = "County.Population") %>% 
  select(FIPS, State.FIPS, Age.Group, County.Population) %>% 
  mutate(Age.Group = factor(Age.Group, levels = unique(.$Age.Group), labels = age.labels)) %>% 
  
  # Append SES.Quintile classification to the counties
  left_join(deaths.SES %>% select(FIPS, SES.Quintile), by = "FIPS") %>% 

  # Adding SES designation to counties that changed administrative code after the ACS survey has been issued, on which SES analysis was based
  mutate(SES.Quintile = as.numeric(case_when(FIPS == "02158" ~ 1,
                                             FIPS == "15005" ~ 4,
                                             FIPS == "46102" ~ 1,
                                             FIPS == "02063" ~ 3,
                                             FIPS == "02066" ~ 3,
                                             TRUE ~ as.numeric(SES.Quintile))))

## Repeat the same steps to read in and process the 2020 population by age
pop.2020 <- read.csv("Input data/vintage2020_county_population_2020.csv", stringsAsFactors = F, as.is = T, colClasses = c(FIPS = "character")) %>% 
  mutate(TOTAL = rowSums(.[2:173]),
         pop0 = rowSums(.[2:3]),                          
         pop1_4 = rowSums(.[4:11]),
         pop5_14 = rowSums(.[12:31]),
         pop15_24 = rowSums(.[32:51]),
         pop25_34 = rowSums(.[52:71]),
         pop35_44 = rowSums(.[72:91]),
         pop45_54 = rowSums(.[92:111]),
         pop55_64 = rowSums(.[112:131]),
         pop65_74 = rowSums(.[132:151]),
         pop75_84 = rowSums(.[152:171]),
         pop85plus = rowSums(.[172:173])) %>% 
  mutate(State.FIPS = substr(FIPS, 1, 2)) %>% 
  pivot_longer(cols = TOTAL:pop85plus,
               names_to = "Age.Group",
               values_to = "County.Population") %>% 
  select(FIPS, State.FIPS, Age.Group, County.Population) %>% 
  mutate(Age.Group = factor(Age.Group, levels = unique(.$Age.Group), labels = age.labels)) %>% 
  
  # Append SES.Quintile classification to the counties
  left_join(deaths.SES %>% select(FIPS, SES.Quintile), by = "FIPS") %>% 
  
  # Adding SES designation to counties that changed administrative code after the ACS survey has been issued, on which SES analysis was based
  mutate(SES.Quintile = as.numeric(case_when(FIPS == "02158" ~ 1,
                                             FIPS == "15005" ~ 4,
                                             FIPS == "46102" ~ 1,
                                             FIPS == "02063" ~ 3,
                                             FIPS == "02066" ~ 3,
                                             TRUE ~ as.numeric(SES.Quintile))))



## 7. Read and group the deaths by age, sex, month and SES decile of 2015-2019 and aggregate by 10-year wide age groups, SES quintile, and
##    January-February, March-May, June-August, and September-December ranges (to correspond to the COVID-19 waves in the analysis).
##    Average deaths are then computed to avoid year-to-year fluctuations of the pre-COVID background scenario.
##
##    NOTE: Due to confidentiality agreements, cells with less than 10 deaths have been suppressed. Though there are very few of 
##    such cells, we attribute a systematic mid-point value of 5 deaths in these cells. This would have a minimal, if not negligible
##    bearing on the outcome, given the scope of the analysis.
NCHS.county.deaths <- read.csv("Input data/deaths_by_age_sex_month_SES_2015_2019.csv", stringsAsFactors = F) %>% 
  filter(SES.Quintile != "All", !is.na(SES.Quintile)) %>%
  mutate(across(starts_with("Deaths"), ~if_else(is.na(.), 5, .))) %>% 
  rowwise() %>% 
  mutate(Deaths.15.19 = mean(c(Deaths.2015, Deaths.2016, Deaths.2017, Deaths.2018, Deaths.2019))) %>% 
  ungroup() %>% 
  mutate(Age.Group = trimws(Age),
         Age.Group.Order = rep(1:11, each = nrow(.)/11),
         SES.Quintile = as.numeric(SES.Quintile),
         Range = cut(Month, breaks = c(0, 3, 6, 9, 13), right = FALSE, 
                     labels = c("January-February", "March-May", "June-August", "September-December"))) %>%
  group_by(SES.Quintile, Range, Age.Group, Age.Group.Order) %>% 
  summarise(Deaths.15.19 = sum(Deaths.15.19)) %>% 
  ungroup() %>% 
  arrange(SES.Quintile, Range, Age.Group.Order)


####### THIS CONCLUDES THE DATA LOADING AND PREPARATION STEP, PLEASE RUN 2_main_analysis.R #######