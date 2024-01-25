library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library("scales")

setwd("C:/Users/DELL/Desktop/220094_Shreejan_DSFD")
#importing the cleaned school dataset
cleaned_school_dataset= read_csv('Cleaned Data/Cleaned School Dataset.csv')

#Creating a new dataset consisting district and short postcode
district= read_csv('Cleaned Data/Cleaned Population.csv') %>% 
  select(`Short Postcode`, District) %>% 
  rename(`Short Post Code`= `Short Postcode`) #renaming to match the column name in school dataset

#Joining the district dataset into Schoo Dataset by Short Post Code
cleaned_school_dataset <- cleaned_school_dataset %>% 
  left_join(district, by = "Short Post Code") %>% 
  na.omit() 

#-----------2021 Average Attainment Score Box plot-----------#

#grouping school dataset by town, distrct, county and year and showing avg. price for each group
Grouped_school_dataset = cleaned_school_dataset %>% 
  group_by(`Town`,District,County,Year) %>% 
  summarise(`Average Attainment Score`= mean(`Attainment Score`)) %>% 
  ungroup(`Town`,District,County,`Year`) 

#creating box plot to visualize average attainment score in kent and surrey in 2021
Grouped_school_dataset %>% 
  filter(Year==2021) %>% #filtering to show only data of 2021
  group_by(County) %>% #grouping by county since we are comparing counties only
  ggplot(aes(x = County, y = `Average Attainment Score`, fill=County)) + #setting x-axis and y-axis values
  scale_y_continuous(limits=c(0,80), breaks = seq(0,80,5))+ #setting limits and breaks
  geom_boxplot() + #specifying the type of plot we need
  labs(title="2021 Average Attainment Score By County Box Plot") #setting label for the chart

#-----------2018-2021 Average Attainment Score Line Graph For Kent's District-----------#

#grouping the cleaned school dataset by county and year and showing the average price for each group
Grouped_school_dataset2 <- cleaned_school_dataset %>% 
  filter(County=="Kent") %>% #filtering to show only rows with county as Kent
  group_by(District,Year) %>% 
  summarise(`Average Attainment Score`= mean(`Attainment Score`)) 

#creating line graph of average Attainment score from 2018-2021
Grouped_school_dataset2 %>%
  group_by(District, Year) %>% 
  #grouping by District and year since we are comparing average score of districts, year after year
  ggplot( aes(x = `Year`, y = `Average Attainment Score`, group = District, color = District)) + 
  #defining x-axis and y-axis values and colors of line
  geom_line(linewidth = 1) + #defining line width 
  geom_point(size = 1, color = "black") + #defining point size and color
  scale_y_continuous(limits=c(0,80), breaks = seq(0,80,5)) + #defining limits, breaks 
  labs(title = "2018-2021 Average Attainment Score Line Graph For Kent's District", #defining labels 
       x = "Year",
       y = "Average Attainment Score") 



#-----------2018-2021 Average Attainment Score Line Graph For Surrey's District-----------#

#grouping the cleaned school dataset by county and year and showing the average price for each group
Grouped_school_dataset3 <- cleaned_school_dataset %>% 
  filter(County=="Surrey") %>% #filtering to show only rows with county as Surrey
  group_by(District,Year) %>% 
  summarise(`Average Attainment Score`= mean(`Attainment Score`)) 

#creating line graph of average Attainment score from 2018-2021
Grouped_school_dataset3 %>%
  group_by(District, Year) %>%  #grouping by District and year since we are comparing average score of districts, year after year
  ggplot( aes(x = `Year`, y = `Average Attainment Score`, group = District, color = District)) + #defining x-axis and y-axis values and colors of line
  geom_line(linewidth = 1) + #defining line width 
  geom_point(size = 1, color = "black") + #defining point size and color
  scale_y_continuous(limits=c(0,40), breaks = seq(0,40,3)) + #defining limits, breaks 
  labs(title = "2018-2021 Average Attainment Score Line Graph For Surrey's District", 
       x = "Year",
       y = "Average Attainment Score") #defining labels 
