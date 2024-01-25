library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library("scales")
setwd("C:/Users/DELL/Desktop/220094_Shreejan_DSFD")

#importing the cleaned house prices
cleaned_houseprices= read_csv('Cleaned Data/Cleaned House Prices.csv') 

#-----------2022 House Price Box plot-----------#

#grouping the cleaned house prices by county , towns and DOT and showing the average price for each group
Grouped_houseprice = cleaned_houseprices%>% 
  group_by(`Town/City`,District,County,`Date of Transfer`) %>% 
  summarise(`Average Price`= mean(Price)) %>% 
  ungroup(`Town/City`,District,County,`Date of Transfer`) 

#creating box plot to visualize average house prices in kent and surrey in 2022
Grouped_houseprice %>% 
  filter(`Date of Transfer`==2022) %>% #filtering to show only houseprice data of 2022
  group_by(County) %>% #grouping by county since we are comparing counties only
  ggplot(aes(x = County, y = `Average Price`, fill=County)) + #setting x-axis and y-axis values
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,300000))+ #setting limits and breaks
  geom_boxplot() + #specifying the type of plot we need
  labs(title="2022 Average House Prices By County Box Plot") #setting label for the chart
 
  


#-----------2022 Average House Price Bar Chart-----------#

#creating bar chart to visualize average house prices in kent and surrey
Grouped_houseprice %>% 
  filter(`Date of Transfer`==2022) %>% #filtering to show only houseprice data of 2022
  group_by(County) %>% #grouping by county since we are comparing counties only
  ggplot(aes(x = County, y = `Average Price`, fill= County)) + #defining x-axis and y-axis values
  geom_bar(stat = "identity") + #using average prices as heigh of the bar
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,300000))+ #setting limits and breaks
  labs(title = "2022 Average House Prices Barchart") 



#-----------2019-2022 Average House Line Graph-----------#

#grouping the cleaned house prices by county and year and showing the average price for each group
Grouped_houseprice2 = cleaned_houseprices%>% 
  group_by(County,`Date of Transfer`) %>% 
  summarise(`Average Price`= mean(Price)) 

#creating line graph of average house prices from 2021-2022
Grouped_houseprice2 %>%
  filter(`Date of Transfer`==2019 | `Date of Transfer`==2020 | `Date of Transfer`==2021 |`Date of Transfer`==2022) %>% 
  #filtering to show only houseprice data of 2019,2020,2021,2022
  group_by(County, `Date of Transfer`) %>%  
  #grouping by county and date of transfer since we are comparing prices of counties year after year
  ggplot( aes(x = `Date of Transfer`, y = `Average Price`, group = County, color = County)) + 
  #defining x-axis and y-axis values and colors of line
  geom_line(linewidth = 1) + #defining line width 
  geom_point(size =1, color = "brown") + #defining point size and color
  scale_y_continuous(limits=c(0,700000), breaks = seq(0,700000,100000), labels = label_number()) + 
  #defining limits, breaks and setting label as number instead of sceintifc notation
  labs(title = "2019-2022 Average House Prices Line Graph", #defining labels 
       x = "Year",
       y = "Average Price") 





