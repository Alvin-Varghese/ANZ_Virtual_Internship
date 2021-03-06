library(readxl)
library(tidyverse)
library(lubridate)
library(funModeling)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

df = read_xlsx("ANZ/ANZ synthesised transaction dataset.xlsx", col_names = TRUE)
df_status(df)
df$bpay_biller_code = as.character(df$bpay_biller_code)
plot_num(df)

#EXTRACTING WEEKS AND HOURS FROM DATE AND EXTRACTION RESPECTIVELY
df$weekday = weekdays(df$date)
df$hour = as.character(df$extraction)
df$hour = substr(df$hour,12,19)
df$hour = hour(as.POSIXct(df$hour, format = "%H:%M:%S"))

#AVERAGE MONTHLY TRANSACTION DISTRIBUTION
df_mon <- df %>% group_by(customer_id) %>% summarise(mon_avg_vol = round(n()/3,0))

hist(df_mon$mon_avg_vol,
     xlab= 'Monthly transaction volume', ylab='No. of customers', main = "Histogram of customers' monthly transaction volume",col = "#27ba9f")

#AVERAGE DAILY TRANSACTION DISTRUBUTION
df_day <- df %>% group_by(customer_id) %>% summarise(da_avg_vol = round(n()/91,0))

hist(df_day$da_avg_vol, xlab= 'Daily transaction volume', ylab='No. of customers', main = "Histogram of customers' Daily transaction volume",col = "#27ba9f")

#AVERAGE TRANSACTION BY WEEKDAY
df_wk <- df%>% select(date, weekday) %>%
  group_by(date, weekday) %>%
  summarise(daily_avg_vol =n()) %>%
  group_by(weekday) %>%
  summarise(avg_vol=mean(daily_avg_vol,na.rm=TRUE ))
df_wk <- df_wk %>% mutate(weekday =  factor(weekday, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>% arrange(weekday)

ggplot(df_wk,aes(x=weekday, y=avg_vol)) +geom_point(colour = "#CC0000")+geom_line(aes(group = 1), colour = "Steelblue") + 
  ggtitle('Average transaction volume by weekday') +
  labs(x='Weekday',y='Transaction volume')

#AVERAGE TRANSACTION PLOTTED BY HOURS
df_hr <- df %>%
  select(date,hour) %>%
  group_by(date,hour) %>%
  summarise(trans_vol=n()) %>%
  group_by(hour) %>%
  summarise(trans_vol_per_hr = mean(trans_vol,na.rm=TRUE))

ggplot(df_hr,aes(x=hour,y=trans_vol_per_hr))+geom_point(colour ="Red")+geom_line(aes(group = 1), colour ="Steelblue")+ ggtitle('Average transaction volume by hour') + labs(x='Hour',y='Transaction volume') + expand_limits( y = 0)

#PLOTTING OUTLIERS IN THE TRANSACTIONAL DATA
tapply(df$amount, df$movement, summary)

ggplot(data = df, aes(x=movement, y=amount)) +
  geom_point(aes(color=movement), alpha=0.2, position ="jitter") +
  geom_boxplot(outlier.size=4, outlier.colour='blue', alpha=0.1)


#Extracting Latitude and Longitudes
#1. Customer location
cust_loc = data.frame(df$long_lat)
cust_loc = data.frame(separate(cust_loc, col = df.long_lat, into = c("cust_long","cust_lat"), sep = " "))

#2. Merchant location
merc_loc = data.frame(df$merchant_long_lat)
merc_loc = data.frame(separate(merc_loc, col = df.merchant_long_lat, into = c("merc_long","merc_lat"), sep = " "))

df_loc <- cbind(df$customer_id,cust_loc,merc_loc)
colnames(df_loc)[which(names(df_loc)=="df$customer_id")] <- "customer_id"

df_loc$cust_long = as.numeric(df_loc$cust_long)
df_loc$cust_lat = as.numeric(df_loc$cust_lat)
df_loc$merc_lat = as.numeric(df_loc$merc_lat)
df_loc$merc_long = as.numeric(df_loc$merc_long)

#PLOTTING THE CUSTOMER AND THE MERCHANT
merc_map <- function (id ){
  
  l = subset (df_loc[,c("customer_id","merc_long","merc_lat")], customer_id == id)
  l <- l[c("merc_long","merc_lat")]
  
  k = subset (df_loc[,c("customer_id","cust_long","cust_lat")], customer_id == id)
  k <- unique(k[c("cust_long","cust_lat")])
  
  ggplot(data = ne_countries(scale = "medium", returnclass = "sf")) +
    geom_sf(fill = "#5a5a5a") +
    coord_sf(xlim = c(113,154), ylim = c(-44,-10), expand = FALSE) +
    geom_point(data = l, aes(x = merc_long, y = merc_lat), size = 1, shape = 5, colour ="steelblue" ) +
    geom_point(data = k, aes(x = cust_long, y = cust_lat), size = 1, shape = 23, fill = "red") + xlab("Longitude") + ylab("Latitude")
}
merc_map(id = "CUS-2487424745")
