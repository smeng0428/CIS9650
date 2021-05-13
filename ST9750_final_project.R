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
  mutate(Date = mdy(Date)) %>% mutate(month=month(Date, label = TRUE)) %>% 
  rename(PM2.5_concentration = `Daily Mean PM2.5 Concentration`,site_ID=`Site ID`)
NO2_NY_2004to2020_cleaned <- NO2_NY_2004to2020_cleaned %>% 
  mutate(Date = mdy(Date)) %>% mutate(month=month(Date, label = TRUE))

# check how many counties involved in this dataset
unique(PM2.5_NY_2004to2020_cleaned$COUNTY)
unique(NO2_NY_2004to2020_cleaned$COUNTY)
unique(PM2.5_NY_2004to2020_cleaned$site_ID)
unique(NO2_NY_2004to2020_cleaned$`Site Name`)
# create NYC counties
NYC_counties <- c("Kings", "Bronx", "Queens", "New York", "Richmond")
#creat the date that NY Pause order go into effect
NY_pause <- mdy_h("March 22 2020 8PM", tz="EST")
##step 3, exploratory data analysis
#first look at how the shutdown affects the pollutant in 2020 in NYC
#pm2.5 trend in 2020 in NYC

PM2.5_NY_2020 <- PM2.5_NY_2004to2020_cleaned %>% filter(year(Date)==2020)
PM2.5_NY_2020 %>%  select(Date, PM2.5_concentration, COUNTY) %>%  
  filter(COUNTY %in% NYC_counties) %>% 
  ggplot(aes(x = Date, y= PM2.5_concentration, color = COUNTY))+geom_point(alpha=0.3)+
  geom_vline(mapping = aes(xintercept = date(NY_pause))) +geom_smooth(se=FALSE)+
  geom_text(aes(x = date(NY_pause), y = 20, label = "NY PAUSE executive order"), size = 3, hjust=-0.05, color = "black")+
  labs(title = "Daily PM 2.5 concetration in NYC in 2020", x = "", y="Daily Mean PM2.5 concentration (ug/m3 LC)")

# long term trend?
PM2.5_NY_2004to2020_cleaned %>% select(Date, PM2.5_concentration, COUNTY) %>%  
  filter(COUNTY %in% NYC_counties) %>% 
  ggplot(aes(x = Date, y= PM2.5_concentration, color = COUNTY))+geom_point(alpha=0.03)+
  geom_vline(mapping = aes(xintercept = date(NY_pause))) +geom_smooth(se=FALSE, size =0.8)+
  geom_text(aes(x = date(NY_pause), y = 40, label = "NY PAUSE executive order"), size = 3, hjust=1, color="black")+
  labs(title = "long term trend of Daily PM 2.5 concetration in NYC", x = "", y="Daily Mean PM2.5 concentration (ug/m3 LC)")
#boxplot to explore potential seasonality
PM2.5_NYC <- PM2.5_NY_2004to2020_cleaned %>% select(Date, PM2.5_concentration, COUNTY, site_ID, month, POC) %>%  
  filter(COUNTY %in% NYC_counties, POC==1) %>% 
  mutate(year=year(Date),weekday=weekdays(Date),weekend=if_else(weekday %in% c("Saturday","Sunday"), TRUE, FALSE))

PM2.5_NYC %>% ggplot(aes(x= month, y= PM2.5_concentration))+
  geom_boxplot()+facet_wrap(~year)+
  labs(title = "Seasonality change of Daily PM2.5 concentration in NYC", x= "month", y= "Daily Mean PM2.5 Concentration (ug/m3 LC)")

PM2.5_NYC %>% ggplot(aes(x= weekend, y= PM2.5_concentration))+
  geom_boxplot()+facet_wrap(~year)+
  labs(title = "Seasonality change of Daily PM2.5 concentration in NYC", x= "weekend", y= "Daily Mean PM2.5 Concentration (ug/m3 LC)")


#needs work  
PM2.5_NYC_Mean<- PM2.5_NYC %>% group_by(Date) %>% summarise(Date,PM2.5_daily_mean = mean(PM2.5_concentration))
#needs work
PM2.5_NYC_ts <- ts(PM2.5_NYC %>% select(Date, PM2.5_concentration))
autoplot(PM2.5_NYC_ts,facets = TRUE)
  
summary(PM2.5_NY_2004to2020_cleaned)
summary(NO2_NY_2004to2020_cleaned)

PM2.5_NY_2020 %>%  select(Date, DAILY_AQI_VALUE, COUNTY) %>%  mutate(Date = mdy(Date)) %>% mutate(month=month(Date)) %>% filter(COUNTY=="Kings") %>% 
  ggplot(aes(x = Date, y= DAILY_AQI_VALUE, color = month ))+geom_point()+geom_smooth()
PM2.5_NY_2019 <- as_tibble((read_csv("dataset/PM25_NY_2019.csv")))
PM2.5_NY_2019 %>%  select(Date, PM2.5_concentration, COUNTY) %>%  mutate(Date = mdy(Date)) %>% mutate(month=month(Date)) %>% filter(COUNTY=="Kings") %>% 
  ggplot(aes(x = factor(month), y= PM2.5_concentration))+geom_boxplot()

##step 4, modeling
## model 1 random forest
set.seed(9750)

#set seed for reproducible result
## we will only use POC ==1 for simplicity
##POC Parameter Occurrence Code - A numeric sequence number used to distinguish between different monitors at one site that are measuring the same pollutant.
#https://aqs.epa.gov/aqsweb/helpfiles/poc.htm
PM2.5_NYC_rf <- PM2.5_NY_2004to2020_cleaned %>% select(Date, PM2.5_concentration, COUNTY, site_ID, POC) %>%  
  filter(COUNTY %in% NYC_counties) %>% 
  mutate(COUNTY=as.factor(COUNTY),year=as.factor(year(Date)),month=as.factor(month(Date)),day=as.factor(day(Date)), weekday=weekdays(Date),weekend=if_else(weekday %in% c("Saturday","Sunday"), TRUE, FALSE)) %>% 
  filter(POC==1)
#subset for 70% training and 30% testing data
train = sample(1:nrow(PM2.5_NYC_rf), nrow(PM2.5_NYC_rf)*.7)

PM2.5_rf_mdl <- randomForest(PM2.5_concentration~Date+COUNTY+site_ID+year+month+day, data = PM2.5_NYC_rf, subset = train, ntree=500)
PM2.5_rf_mdl2 <- randomForest(PM2.5_concentration~Date+COUNTY+site_ID+year+month+weekday+weekend, data = PM2.5_NYC_rf, subset = train, ntree=500)
PM2.5_NYC_rf$pred <- predict(PM2.5_rf_mdl, PM2.5_NYC_rf)
rf_train_rmse <- sqrt(mean((PM2.5_NYC_rf$pred-PM2.5_NYC_rf$PM2.5_concentration)^2))
rf2_train_rmse <- sqrt(mean((predict(PM2.5_rf_mdl2, PM2.5_NYC_rf)-PM2.5_NYC_rf$PM2.5_concentration)^2))
importance(PM2.5_rf_mdl)
importance(PM2.5_rf_mdl2)
varImpPlot(PM2.5_rf_mdl)
varImpPlot(PM2.5_rf_mdl2)

PM2.5_NYC_rf_test <- PM2.5_NYC_rf[-train,"PM2.5_concentration"]#testing set for data validation
PM2.5_NYC_rf_test$rf_yhat <- predict(PM2.5_rf_mdl, newdata = PM2.5_NYC_rf[-train,])
PM2.5_NYC_rf_test %>% ggplot(aes(x=rf_yhat, y=PM2.5_concentration))+
  geom_point()+geom_abline()
rf_test_rmse <- sqrt(mean((PM2.5_NYC_rf_test$rf_yhat-PM2.5_NYC_rf_test$PM2.5_concentration)^2))

PM2.5_NYC_rf_test2 <- PM2.5_NYC_rf[-train,"PM2.5_concentration"]#testing set for data validation
PM2.5_NYC_rf_test2$rf_yhat <- predict(PM2.5_rf_mdl2, newdata = PM2.5_NYC_rf[-train,])
PM2.5_NYC_rf_test2 %>% ggplot(aes(x=rf_yhat, y=PM2.5_concentration))+
  geom_point()+geom_abline()
rf_rmse2 <- sqrt(mean((PM2.5_NYC_rf_test2$rf_yhat-PM2.5_NYC_rf_test2$PM2.5_concentration)^2))
## model 2 lm or ts
#trying with lm first
#same as PM2.5_NYC_rf, rename for easy understanding
PM2.5_NYC_lm <- PM2.5_NY_2004to2020_cleaned %>% select(Date, PM2.5_concentration, COUNTY, site_ID, POC) %>%  
  filter(COUNTY %in% NYC_counties) %>% 
  mutate(COUNTY=as.factor(COUNTY),year=as.factor(year(Date)),month=as.factor(month(Date)),day=as.factor(day(Date)), weekday=weekdays(Date),weekend=if_else(weekday %in% c("Saturday","Sunday"), TRUE, FALSE)) %>% 
  filter(POC==1)
PM2.5_lm_mdl <- lm(PM2.5_concentration~Date, data = PM2.5_NYC_lm, subset = train)
PM2.5_NYC_lm$pred <-predict(PM2.5_lm_mdl, PM2.5_NYC_lm)
lm_train_rmse <- sqrt(mean((PM2.5_NYC_lm$pred-PM2.5_NYC_lm$PM2.5_concentration)^2))
augment(PM2.5_lm_mdl)
summary(PM2.5_lm_mdl)

PM2.5_NYC_lm_test <- PM2.5_NYC_lm[-train,"PM2.5_concentration"]#testing set for data validation
PM2.5_NYC_lm_test$lm_yhat <- predict(PM2.5_lm_mdl, newdata = PM2.5_NYC_lm[-train,])
PM2.5_NYC_lm_test %>% ggplot(aes(x=lm_yhat, y=PM2.5_concentration))+
  geom_point()+geom_abline()
lm_test_rmse <- sqrt(mean((PM2.5_NYC_lm_test$lm_yhat-PM2.5_NYC_lm_test$PM2.5_concentration)^2))
## step 5, using historical data to inference on 2020 and compare with actual data

## step 6, conclude and suggestion for further analysis.
