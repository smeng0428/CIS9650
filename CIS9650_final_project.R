##loading required libs here
library(tidyverse)
library(lubridate)

##step 1, read in data
## since we are focusing on PM2.5 and NO2 from 2004 to 2020. there will be 34 csv file to be read in
## the following function will read data to a data frame.

# this function will read data for PM2.5, please set the dataset folder to your work directory.
# default value for year is 2004:2020, default value for state is NY
# you can overwrite it by explicitly telling the function which 
# year and state you want to read in.
read_data <- function(pollutant, state="NY", year = 2004:2020) {
  datamerge <- data.frame()
  for (i in year){
    temp <- read_csv(file = paste0("dataset/", as.character(pollutant), "_", as.character(state), "_", i,".csv" ))
    datamerge <- rbind(datamerge, temp)
  }
  return(as_tibble(datamerge))
}

PM2.5_NY_2004to2020 <- read_data(pollutant="PM25")

NO2_NY_2004to2020 <- read_data(pollutant="NO2")

##step 2, cleaning and manipulating data



##step 3, exploratory data analysis



# seasonal change? long term trend?
CO_NY_2020 <- as_tibble(read_csv("dataset/CO_NY_2020.csv"))
names(CO_NY_2020)
CO_NY_2020 %>%  select(Date, DAILY_AQI_VALUE, COUNTY) %>%  mutate(Date = mdy(Date)) %>% 
  ggplot(aes(x = Date, y= DAILY_AQI_VALUE, color = COUNTY ))+geom_point()
PM2.5_NY_2020 <- as_tibble((read_csv("dataset/PM25_NY_2020.csv")))
PM2.5_NY_2020
PM2.5_NY_2020 %>%  select(Date, DAILY_AQI_VALUE, COUNTY) %>%  mutate(Date = mdy(Date)) %>% mutate(month=month(Date)) %>% filter(COUNTY=="Kings") %>% 
  ggplot(aes(x = Date, y= DAILY_AQI_VALUE, color = month ))+geom_point()+geom_smooth()
PM2.5_NY_2019 <- as_tibble((read_csv("dataset/PM25_NY_2019.csv")))
PM2.5_NY_2019 %>%  select(Date, `Daily Mean PM2.5 Concentration`, COUNTY) %>%  mutate(Date = mdy(Date)) %>% mutate(month=month(Date)) %>% filter(COUNTY=="Kings") %>% 
  ggplot(aes(x = factor(month), y= `Daily Mean PM2.5 Concentration`))+geom_boxplot()

##step 4, modeling
## model 1 random walk(RW) model?

## model 2 Time lag analysis (TLA) model?

## step 5, using historical data to inferenece on 2020 and compare with actural data

## step 6, conclude and suggestion for further analysis.
