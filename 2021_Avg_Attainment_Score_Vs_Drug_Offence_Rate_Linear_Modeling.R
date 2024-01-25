library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
setwd("C:/Users/DELL/Desktop/220094_Shreejan_DSFD")
#importing population dataset
population_dataset<- read_csv('Cleaned Data/Cleaned Population.csv')


#importing the cleaned school dataset
cleaned_school_dataset= read_csv('Cleaned Data/Cleaned School Dataset.csv')

#importing the cleaned crime dataset
cleaned_crime_dataset= read_csv('Cleaned Data/Cleaned Crime Dataset.csv') 


#grouping school data by town and county and finding average score for each group
grouped_school_dataset = cleaned_school_dataset %>%
  filter(`Year`=="2021") %>%
  group_by(`Town`,County) %>%
  mutate(Town= tolower(Town)) %>%  #converting the town from to all lowercase
  summarise(`Attainment Score`=mean(`Attainment Score`))

#modifying crime dataset to show drug offence rate and crime count  
crime_dataset_drugs2 <-cleaned_crime_dataset %>% 
  mutate(`Date of crime`= substr(`Date of crime`, 1, 4)) %>% #Mutating this column to only show year
  group_by(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  #Grouping to show crime count in each postcode by year
  select(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  na.omit() %>% 
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>% 
  right_join(population_dataset, by = "Short Postcode") %>%
  #joining with population dataset to show district and population
  select(`Short Postcode`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, `Town/City`, District) %>%
  #select the required columns
  na.omit() %>% 
  filter(`Crime type`== "Drugs") %>% #filtering to show only drug crimes of 2022
  mutate(`Drug Offence Rate` = (`Crime Count` / Population)) #calculating drug offence rate

#grouping the drug crime dataset by county and town and showing the rate for each group for the year 2021
grouped_drug_crime <- crime_dataset_drugs2 %>% 
  filter(`Date of crime`=="2021") %>% 
  group_by(`Falls within`,`Town/City`) %>% 
  mutate(`Town/City`= tolower(`Town/City`)) %>%  #converting the town from to all lowercase
  summarise(`Drug Offence Rate`= mean(`Drug Offence Rate`))

#joining school data and house price data in a single table
school_drug_data = grouped_school_dataset %>% 
  left_join(grouped_drug_crime ,by=c("Town"="Town/City")) %>% 
  na.omit #removing rows with null value

#creating a linear model 
l_model = lm(data=school_drug_data, `Attainment Score`~`Drug Offence Rate`) 
#this model predicts Average attainment score as a function of Drug offence rate

#showing summary of the Linear Model
summary(l_model) 


#creating the linear model graph
ggplot(school_drug_data,aes(x=`Drug Offence Rate`,y= `Attainment Score`)) +
  scale_y_continuous(limits=c(0,50), breaks = seq(0,50,5))+ #setting limits and breaks
  geom_point(data = filter(school_drug_data,County=="Kent"),aes(color=c("Red"="Kent")))+ 
  #setting color as red for Kent's data point
  geom_point(data = filter(school_drug_data,County=="Surrey"), aes(color=c("Blue"="Surrey"))) + 
  #setting color as blye for Surrey's data point
  geom_smooth(method=lm,se=FALSE,color="brown")+ #adding linear regression line and omitting error bands 
  labs(x="Drug Offence Rate",
       y="Attainment Score",
       title="2021 Attainment Score vs Drug Offence Rate",color="County") #setting labels
