library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
setwd("C:/Users/DELL/Desktop/220094_Shreejan_DSFD")
#importing the cleaned broadband speed
cleaned_broadband_speed= read_csv('Cleaned Data/Cleaned Broadband Speed Dataset.csv') 

#importing the cleaned school dataset
cleaned_school_dataset= read_csv('Cleaned Data/Cleaned School Dataset.csv')

#grouping broadband speed by town and county and finding average download speed for each group
grouped_broadband_speeds = cleaned_broadband_speed %>%
  group_by(`Town/City`,County) %>%
  mutate(`Town/City`= tolower(`Town/City`)) %>%  #converting the town from to all lowercase
  summarise(`Average download speed (Mbit/s)`= mean(`Average download speed (Mbit/s)`))

#grouping school data by town and county and finding average score for each group
grouped_school_dataset = cleaned_school_dataset %>%
  group_by(`Town`,County) %>%
  mutate(Town= tolower(Town)) %>%  #converting the town from to all lowercase
  summarise(`Attainment Score`=mean(`Attainment Score`))

#joining broadband data and school data in a single table
broadband_attainment_data = grouped_broadband_speeds %>% 
  left_join(grouped_school_dataset,by=c("Town/City"="Town")) %>% 
  na.omit #removing rows with null value

#creating a linear model 
l_model = lm(data=broadband_attainment_data, `Average download speed (Mbit/s)`~`Attainment Score`) #this model predicts Average download speed as a function of Drug offence rate

#showing summary of the Linear Model
summary(l_model) 

#creating the linear model graph
ggplot(broadband_attainment_data,aes(x=`Attainment Score`,y=`Average download speed (Mbit/s)`)) +
  scale_y_continuous(limits=c(0,80), breaks = seq(0,80,5))+ #setting limits and breaks
  geom_point(data = filter(broadband_attainment_data,County.x=="KENT"),aes(color=c("Red"="Kent")))+ 
  #setting color as red for Kent's data point
  geom_point(data = filter(broadband_attainment_data,County.x=="SURREY"), aes(color=c("Blue"="Surrey"))) +
  #setting color as blue for Surrey's data point
  geom_smooth(method=lm,se=FALSE,color="yellow")+ #adding linear regression line and omitting error bands 
  labs(x="Attainment Score",
       y="Average Download Speed (Mbit/s)",
       title="Average Download Speed vs Attainment Score",color="County") #setting labels

