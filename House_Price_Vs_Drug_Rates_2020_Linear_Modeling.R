library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
setwd("C:/Users/DELL/Desktop/220094_Shreejan_DSFD")

#importing population dataset
population_dataset<- read_csv('Cleaned Data/Cleaned Population.csv')

#importing the cleaned house prices
cleaned_houseprices= read_csv('Cleaned Data/Cleaned House Prices.csv') 

#importing the cleaned crime dataset
cleaned_crime_dataset= read_csv('Cleaned Data/Cleaned Crime Dataset.csv') 

#grouping house prices by town and county and finding average price for each group
grouped_house_prices = cleaned_houseprices %>%
  filter(`Date of Transfer`=="2020") %>%
  group_by(`Town/City`,County) %>%
  summarise(Price=mean(Price))

#modifying our crime dataset to show drug offence rate and crime count  
crime_dataset_drugs2 <-cleaned_crime_dataset %>% 
  mutate(`Date of crime`= substr(`Date of crime`, 1, 4)) %>% #Mutating this column to only show year
  group_by(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% #Grouping to show crime count in each postcode by year
  select(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  na.omit() %>% 
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>% 
  right_join(population_dataset, by = "Short Postcode") %>% #joining with population dataset to show district and population
  select(`Short Postcode`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, `Town/City`, District) %>% 
  #select the required columns
  na.omit() %>% 
  filter(`Crime type`== "Drugs") %>% #filtering to show only drug crimes of 2022
  mutate(`Drug Offence Rate` = (`Crime Count` / Population)) #calculating drug offence rate

#grouping the drug crime dataset by county and town and showing the rate for each group for the year 2020
grouped_drug_crime <- crime_dataset_drugs2 %>% 
  filter(`Date of crime`=="2020") %>% 
  group_by(`Falls within`,`Town/City`) %>% 
  summarise(`Drug Offence Rate`= mean(`Drug Offence Rate`))


#joining house price data and drug crime rate data in a single table
house_price_drug_crime_data = grouped_house_prices %>% 
  left_join(grouped_drug_crime,by="Town/City") %>% 
  na.omit #removing null values


#creating a linear model 
l_model = lm(data=house_price_drug_crime_data, Price~`Drug Offence Rate`) 
#this model predicts House Price as a function of Drug offence rate

#showing summary of the Linear Model
summary(l_model) 

#creating the linear model graph
ggplot(house_price_drug_crime_data,aes(x=`Drug Offence Rate`,y=Price)) +
  scale_y_continuous(limits=c(0,1000000), breaks = seq(0,1000000,200000))+ #setting limits and breaks
  geom_point(data = filter(house_price_drug_crime_data,County=="KENT"),aes(color=c("Red"="Kent")))+
  #setting color as red for Kent's data point
  geom_point(data = filter(house_price_drug_crime_data,County=="SURREY"), aes(color=c("Blue"="Surrey"))) +
  #setting color as blue for Surrey's data point
  geom_smooth(method=lm,se=FALSE,color="orange")+ #adding linear regression line and omitting error bands 
  labs(x="Drug Offence Rate",
       y="Price",
       title="2020 House Prices vs Drug Offence Rate",color="County") #setting labels

