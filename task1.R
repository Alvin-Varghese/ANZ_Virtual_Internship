library(readxl)
library(tidyverse)
library(lubridate)
library(funModeling)
library(geosphere)

#Extracting weeks and hours from dates
# 1. WEEKS
df$weekday = weekdays(df$date)
# 2. HOUR
df$hour = as.character(df$extraction)
df$hour = substr(df$hour,12,19)
df$hour = hour(as.POSIXct(df$hour, format = "%H:%M:%S"))

#Extracting Latitude and Longitudes
#1. Customer location
cust_loc = data.frame(df$long_lat)
cust_loc = data.frame(separate(cust_loc, col = df.long_lat, into = c("cust_long","cust_lat"), sep = " "))
df <- cbind(df, cust_loc)
#2. Merchant location
merc_loc = data.frame(df$merchant_long_lat)
merc_loc = data.frame(separate(merc_loc, col = df.merchant_long_lat, into = c("merc_long","merc_lat"), sep = " "))
df <- cbind(df, merc_loc)


#AVERAGE TRANSACTION BY DAY
df2 <- df %>% group_by(customer_id) %>% summarise(mon_avg_vol = round(n()/3,0))

hist(df2$mon_avg_vol,
     xlab= 'Monthly transaction volume', ylab='No. of customers', main = "Histogram of customers' monthly transaction volume")

df4 <- df%>% select(date, weekday) %>%
  group_by(date, weekday) %>%
  summarise(daily_avg_vol =n()) %>%
  group_by(weekday) %>%
  summarise(avg_vol=mean(daily_avg_vol,na.rm=TRUE ))
df4 <- df4 %>% mutate(weekday =  factor(weekday, levels = c( "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>% arrange(weekday)

ggplot(df4,aes(x=weekday, y=avg_vol)) +geom_point()+geom_line(aes(group = 1))+
  ggtitle('Average transaction volume by weekday') +
  labs(x='Weekday',y='Transaction volume')

#AVERAGE TRANSACTION BY HOUR
df_hr <- df %>%
  select(date,hour) %>%
  group_by(date,hour) %>%
  summarise(trans_vol=n()) %>%
  group_by(hour) %>%
  summarise(trans_vol_per_hr = mean(trans_vol,na.rm=TRUE))
ggplot(df_hr,aes(x=hour,y=trans_vol_per_hr))+geom_point()+geom_line(aes(group = 1))+ ggtitle('Average transaction volume by hour') + labs(x='Hour',y='Transaction volume') + expand_limits( y = 0)