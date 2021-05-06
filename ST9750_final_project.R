##loading required libs here
library(tidyverse)
library(lubridate)
library(modelr)
library(broom)
library(randomForest)
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
#193975 obs of 20 variables
NO2_NY_2004to2020 <- read_data(pollutant="NO2")
#42814 obs of 20 variables

##step 2, cleaning and manipulating data
#checking for missing values
PM2.5_NY_2004to2020 %>% select(`Daily Mean PM2.5 Concentration`) %>%
  filter(is.na(`Daily Mean PM2.5 Concentration`))
#no missing value for Daily mean PM2.5
NO2_NY_2004to2020 %>% select(`Daily Max 1-hour NO2 Concentration`) %>%
  filter(is.na(`Daily Max 1-hour NO2 Concentration`))
#no mission value for Daily Max No2
#somehow there are daily mean pm2.5 readings which are negative, these reading probably caused by faulty sensors. 
#we will exclude from the dataset
PM2.5_NY_2004to2020_cleaned <- PM2.5_NY_2004to2020 %>% filter(`Daily Mean PM2.5 Concentration`>=0)
#check for the percentage of the cleaned data to the total data
paste0(format(nrow(PM2.5_NY_2004to2020_cleaned)/nrow(PM2.5_NY_2004to2020)*100,trim=TRUE, digits = 5),"%")
#99.717% of the total data which I think it is reasonable.

#similarly, clean the data for NO2
NO2_NY_2004to2020_cleaned <- NO2_NY_2004to2020 %>%  filter(`Daily Max 1-hour NO2 Concentration`>0)
paste0(format(nrow(NO2_NY_2004to2020_cleaned)/nrow(NO2_NY_2004to2020)*100, trim = TRUE, digits = 5), "%")
#99.615% of total data


#transform the date to the format of datetime 
PM2.5_NY_2004to2020_cleaned <- PM2.5_NY_2004to2020_cleaned %>% 
  mutate(Date = mdy(Date)) %>% mutate(month=month(Date))
NO2_NY_2004to2020_cleaned <- NO2_NY_2004to2020_cleaned %>% 
  mutate(Date = mdy(Date)) %>% mutate(month=month(Date))

# check how many counties involved in this dataset
unique(PM2.5_NY_2004to2020_cleaned$COUNTY)
unique(NO2_NY_2004to2020_cleaned$COUNTY)
# create NYC counties
NYC_counties <- c("Kings", "Bronx", "Queens", "New York", "Richmond")
#creat the date that NY Pause order go into effect
NY_pause <- mdy_h("March 22 2020 8PM", tz="EST")
##step 3, exploratory data analysis
#first look at how the shutdown affects the pollutant in 2020 in NYC
#pm2.5 trend in 2020 in NYC

PM2.5_NY_2020 <- PM2.5_NY_2004to2020_cleaned %>% filter(year(Date)==2020)
PM2.5_NY_2020 %>%  select(Date, `Daily Mean PM2.5 Concentration`, COUNTY) %>%  
  filter(COUNTY %in% NYC_counties) %>% 
  ggplot(aes(x = Date, y= `Daily Mean PM2.5 Concentration`, color = COUNTY))+geom_point(alpha=0.3)+
  geom_vline(mapping = aes(xintercept = date(NY_pause))) +geom_smooth(se=FALSE)+
  geom_text(aes(x = date(NY_pause), y = 20, label = "NY PAUSE executive order"), size = 4, hjust=-0.05)+
  labs(title = "Daily PM 2.5 concetration in NYC in 2020", x = "", y="Daily Mean PM2.5 concentration (ug/m3 LC)")

# seasonal change? long term trend?
PM2.5_NY_2004to2020_cleaned %>% select(Date, `Daily Mean PM2.5 Concentration`, COUNTY) %>%  
  filter(COUNTY %in% NYC_counties) %>% 
  ggplot(aes(x = Date, y= `Daily Mean PM2.5 Concentration`, color = COUNTY))+geom_point(alpha=0.03)+
  geom_vline(mapping = aes(xintercept = date(NY_pause))) +geom_smooth(se=FALSE, size =0.8, aes(color = "All"))+
  geom_text(aes(x = date(NY_pause), y = 40, label = "NY PAUSE executive order", color ="All"), size = 4, hjust=1)+
  labs(title = "long term trend of Daily PM 2.5 concetration in NYC", x = "", y="Daily Mean PM2.5 concentration (ug/m3 LC)")



PM2.5_NY_2020 %>%  select(Date, DAILY_AQI_VALUE, COUNTY) %>%  mutate(Date = mdy(Date)) %>% mutate(month=month(Date)) %>% filter(COUNTY=="Kings") %>% 
  ggplot(aes(x = Date, y= DAILY_AQI_VALUE, color = month ))+geom_point()+geom_smooth()
PM2.5_NY_2019 <- as_tibble((read_csv("dataset/PM25_NY_2019.csv")))
PM2.5_NY_2019 %>%  select(Date, `Daily Mean PM2.5 Concentration`, COUNTY) %>%  mutate(Date = mdy(Date)) %>% mutate(month=month(Date)) %>% filter(COUNTY=="Kings") %>% 
  ggplot(aes(x = factor(month), y= `Daily Mean PM2.5 Concentration`))+geom_boxplot()

##step 4, modeling
## model 1 

## model 2 Time lag analysis (TLA) model?

## step 5, using historical data to inferenece on 2020 and compare with actural data

## step 6, conclude and suggestion for further analysis.
