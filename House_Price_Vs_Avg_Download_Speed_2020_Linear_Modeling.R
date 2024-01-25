library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
setwd("C:/Users/DELL/Desktop/220094_Shreejan_DSFD")

#importing the cleaned house prices
cleaned_houseprices= read_csv('Cleaned Data/Cleaned House Prices.csv') 

#importing the cleaned broadband speed
cleaned_broadband_speed= read_csv('Cleaned Data/Cleaned Broadband Speed Dataset.csv') 

#grouping house prices by town and county and finding average price for each group
grouped_house_prices = cleaned_houseprices %>%
  filter(`Date of Transfer`=="2020") %>%
  group_by(`Town/City`,County) %>%
  summarise(Price=mean(Price))

#grouping broadband speed by town and county and finding average download speed for each group
grouped_broadband_speeds = cleaned_broadband_speed %>%
  group_by(`Town/City`,County) %>%
  summarise(`Average download speed (Mbit/s)`= mean(`Average download speed (Mbit/s)`))

#joining house price data and broadband speed data in a single table
house_price_broadband_data = grouped_house_prices %>% 
  left_join(grouped_broadband_speeds,by="Town/City")

#creating a linear model 
l_model = lm(data=house_price_broadband_data, Price~`Average download speed (Mbit/s)`) #this model predicts Price as a function of Average download speed (Mbit/s)

#showing summary of the Linear Model
summary(l_model) 

#creating the linear model graph
ggplot(house_price_broadband_data,aes(x=`Average download speed (Mbit/s)`,y=Price)) +
  scale_y_continuous(limits=c(0,2500000), breaks = seq(0,2500000,1000000))+ #setting limits and breaks
  geom_point(data = filter(house_price_broadband_data,County.x=="KENT"),aes(color=c("Red"="Kent")))+ 
  #setting color as red for Kent's data point
  geom_point(data = filter(house_price_broadband_data,County.x=="SURREY"), aes(color=c("Blue"="Surrey"))) +
  #setting color as blye for Surrey's data point
  geom_smooth(method=lm,se=FALSE,color="violet")+ #adding linear regression line and omitting error bands 
  labs(x="Average Download Speed (Mbit/s)",
       y="Price",
       title="2020 House Prices vs Average Download Speed",color="County") #setting labels
