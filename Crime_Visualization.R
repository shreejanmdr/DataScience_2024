install.packages("fmsb") #installing this package for radar chart

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library("scales")
library(fmsb)
setwd("C:/Users/DELL/Desktop/220094_Shreejan_DSFD")

#importing the cleaned crime dataset
cleaned_crime_dataset= read_csv('Cleaned Data/Cleaned Crime Dataset.csv') 

#importing population dataset
population_dataset<- read_csv('Cleaned Data/Cleaned Population.csv')


#-----------2022 Drug Offence Rate Box plot-----------#

#modifying our crime dataset to show drug offence rate and crime count for 2022
crime_dataset_drugs <-cleaned_crime_dataset %>% 
  mutate(`Date of crime`= substr(`Date of crime`, 1, 4)) %>% #Mutating this column to only show year
  group_by(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  #Grouping to show crime count in each postcode by year
  select(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  na.omit() %>% 
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>% 
  right_join(population_dataset, by = "Short Postcode") %>%
  #joining with population dataset to show district and population
  select(`Short Postcode`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, District) %>% 
  #select the required columns
  na.omit() %>% 
  filter(`Crime type`== "Drugs" & `Date of crime`==2022) %>%
  #filtering to show only drug crimes of 2022
  mutate(`Drug Offence Rate` = (`Crime Count` / Population)) #calculating drug offence rate

#creating box plot to visualize drug offence rate in kent and surrey's district in 2022
ggplot(data = crime_dataset_drugs, aes(x = District, y = `Drug Offence Rate`, fill = `Falls within`)) +
  #setting x-axis and y-axis values
  scale_y_continuous(limits=c(0,0.1), breaks = seq(0,0.1,0.005), labels = label_number()) + #defining limits, breaks 
  geom_boxplot() + #defining the type of plot we want
  labs(title = "2022 Drug Offence Rate By District Box Plot") +
  coord_flip() 



  
#-----------2022 June Vehicle Crime Rate Per 10000 people Radar Chart-----------#

#modifying our crime dataset to show vehicle crime rate and crime count 
crime_dataset_vehicle <-cleaned_crime_dataset %>% 
  group_by(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>%
  #Grouping to show crime count in each postcode by year
  select(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  na.omit() %>% 
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>% 
  ungroup(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  right_join(population_dataset, by = "Short Postcode") %>% 
  #joining with population dataset to show district and population
  select(`Short Postcode`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, District) %>% 
  #select the required columns
  na.omit() %>% 
  filter(`Crime type`== "Vehicle crime" & `Date of crime`=="2022-06") %>%  
  #filtering to show only vehicle crimes of 2022 June
  mutate(`Vehicle Crime Rate` = (`Crime Count` / Population)*10000) #calculating vehicle crime rate per 10000 people

radar_data<- crime_dataset_vehicle %>%
  select(District, `Vehicle Crime Rate`) %>%
  unique()  # Assuming you want unique districts


#-----------2022 June Vehicle Crime Rate Per 10000 people Radar Chart-----------#

#modifying our crime dataset to show vehicle crime rate and crime count 
crime_dataset_vehicle <-cleaned_crime_dataset %>% 
  group_by(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% #Grouping to show crime count in each postcode by year
  select(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  na.omit() %>% 
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>% 
  ungroup(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  right_join(population_dataset, by = "Short Postcode") %>% #joining with population dataset to show district and population
  select(`Short Postcode`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, District) %>% 
  #select the required columns
  na.omit() %>% 
  filter(`Crime type`== "Vehicle crime" & `Date of crime`=="2022-06") %>%   #filtering to show only vehicle crimes of 2022 June
  mutate(`Vehicle Crime Rate` = (`Crime Count` / Population)*10000) #calculating vehicle crime rate per 10000 people

radar_data<- crime_dataset_vehicle %>%
  select(District, `Vehicle Crime Rate`, `Crime Count`) %>%
  unique()  # Assuming you want unique districts


# Find the max value for scaling the radar chart
max_value <- max(radar_data$`Vehicle Crime Rate`, na.rm = TRUE)
max_crime_count <- max(radar_data$`Crime Count`, na.rm = TRUE)

# Create a dataframe with max values
max_row <- data.frame(District = "Max", `Vehicle Crime Rate` = max_value, `Crime Count`=max_crime_count) %>% 
  rename(`Vehicle Crime Rate`= `Vehicle.Crime.Rate`) %>% 
  rename(`Crime Count`= `Crime.Count`)



# Add the max_row dataframe to the start of radar_data
radar_data <- rbind(max_row, radar_data)

# Normalize the data for radar chart
radar_data_normalized <- as.data.frame(lapply(radar_data[, -1], function(x) (x - min(x)) / (max(x) - min(x))))
radar_data_normalized <- cbind(District = radar_data$`Crime Count`, radar_data_normalized)

# Create the radar chart
radar_chart <- radarchart(radar_data_normalized, axistype = 1,
                          pcol = c("green", rep("red", nrow(radar_data_normalized) - 1)),
                          pfcol = c(NA, rep(rgb(0, 0, 1, 0.5), nrow(radar_data_normalized) - 1)),
                          plwd = 2)


#-----------2022 June Robbery Rate Per 10000 people Pie Chart-----------#

#modifying our crime dataset to show robbery crime rate and crime count 
crime_dataset_robbery <-cleaned_crime_dataset %>% 
  group_by(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  #Grouping to show crime count in each postcode by year
  select(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  na.omit() %>% 
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>% 
  ungroup(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  right_join(population_dataset, by = "Short Postcode") %>% 
  #joining with population dataset to show district and population
  select(`Short Postcode`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, District) %>%
  #select the required columns
  na.omit() %>% 
  filter(`Crime type`== "Robbery" & `Date of crime`=="2022-06") %>%   
  #filtering to show only vehicle crimes of 2022 June
  mutate(`Robbery Crime Rate` = (`Crime Count` / Population)*10000) %>% 
  #calculating vehicle crime rate per 10000 people
  group_by(District) %>% #grouping by district
  summarise(TotalRobberyCrimeRate = sum(`Robbery Crime Rate`)) 
#aggregating crime rates by District


ggplot(crime_dataset_robbery, aes(x = "", y = TotalRobberyCrimeRate, fill = District)) +
  #defining x axis and y axis values
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  theme_void() +
  labs(fill = "District", title = "Robbery Crime Rate by District in June 2022") 
#defining lables



#-----------2019-2022 Drug Offence Rate In Kent and Surrey Line Chart-----------#

#modifying our crime dataset to show drug offence rate and crime count  
crime_dataset_drugs2 <-cleaned_crime_dataset %>% 
  mutate(`Date of crime`= substr(`Date of crime`, 1, 4)) %>% #Mutating this column to only show year
  group_by(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  #Grouping to show crime count in each postcode by year
  select(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  na.omit() %>% 
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>% 
  right_join(population_dataset, by = "Short Postcode") %>% #joining with population dataset to show district and population
  select(`Short Postcode`,`Crime type`,`Crime Count`, `Population`, `Date of crime`, `Falls within`, District) %>% 
  #select the required columns
  na.omit() %>% 
  filter(`Crime type`== "Drugs") %>% #filtering to show only drug crimes of 2022
  mutate(`Drug Offence Rate` = (`Crime Count` / Population)) #calculating drug offence rate

#grouping the drug crime dataset by county and year and showing the rate for each group
Grouped_drug_crime <- crime_dataset_drugs2%>% 
  group_by(`Falls within`,`Date of crime`) %>% 
  summarise(`Drug Offence Rate`= mean(`Drug Offence Rate`))

#creating line graph of average house prices from 2021-2022
Grouped_drug_crime %>%
  group_by(`Falls within`, `Date of crime`) %>%  
  #grouping by county and date of crime since we are comparing offence rate in counties year after year
  ggplot( aes(x = `Date of crime`, y = `Drug Offence Rate`, group = `Falls within`, color = `Falls within`)) + 
  #defining x-axis and y-axis values and colors of line
  geom_line(linewidth = 1) + #defining line width 
  geom_point(size = 1, color = "brown") + #defining point size and color
  scale_y_continuous(limits=c(0,0.2), breaks = seq(0,0.2,0.01), labels = label_number()) + #defining limit and breaks
  labs(title = "2020-2023 Drug Offence Rate", #defining labels 
       x = "Year",
       y = "Drug Offence Rate") #setting labels

