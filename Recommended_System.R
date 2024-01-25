library(tidyverse)
library(dplyr)
setwd("C:/Users/DELL/Desktop/220094_Shreejan_DSFD")


#-----------House Price Ranking-----------#

#Importing cleaned population data
cleaned_population_data = read_csv("Cleaned Data/Cleaned Population.csv")


#Importing cleaned house price data
cleaned_houseprices = read_csv("Cleaned Data/Cleaned House Prices.csv")

#Creating a new house rank table
houseprice_rank= cleaned_houseprices %>%
  filter(`Date of Transfer`=="2020") %>%
  group_by(`Town/City`) %>%
  summarise(Price=mean(Price),County=first(County)) %>% 
  #reducing the table by merging multiple same towns that belong to the same county
  arrange(Price) %>% #arranging price in ascending order
  mutate(`House Score`=10-(Price/100000)) %>%  
  #calculating score. We are subtracting from 10 because lower house prices need to have higher rank
  select(`Town/City`,County, `House Score`)

#defining path to save the house ranking csv
file_path <- "Recommended System/House Pricing Ranks.csv"

#saving the house ranking csv
write.csv(houseprice_rank, file_path, row.names = FALSE)
view(houseprice_rank)

#-----------Download Speed Ranking-----------#

#importing the cleaned broadband speed
cleaned_broadband_speed= read_csv('Cleaned Data/Cleaned Broadband Speed Dataset.csv')

#Creating a new download speed rank table
download_speed_rank <- cleaned_broadband_speed %>%
  group_by(`Town/City`) %>%
  rename(Town=`Town/City`) %>% #renaming to maintain consistency
  summarise(`Average download speed (Mbit/s)`=`Average download speed (Mbit/s)`,County=first(County)) %>%
  arrange(desc(`Average download speed (Mbit/s)`)) %>% #arranging download speed in descending order
  mutate(`Download Score`= (`Average download speed (Mbit/s)`/100)) %>% #calculating score
  select(Town, County, `Download Score`) %>%
  distinct(Town, .keep_all = TRUE) #keeping .keep_all as true because we want to preserve other columns

#defining path to save the download speed speed ranking csv
file_path <- "Recommended System/Broadband speed rank.csv"
view(download_speed_rank)

#saving the download speed ranking csv
write.csv(download_speed_rank, file_path, row.names = FALSE)

#-----------Crime Ranking-----------#
#importing cleaned crime dataset
cleaned_crime_data =read_csv('Cleaned Data/Cleaned Crime Dataset.csv')

#importing cleaned population dataset
population_dataset<- read_csv('Cleaned Data/Cleaned Population.csv')

crime_dataset_count <-cleaned_crime_data %>%
  mutate(`Date of crime`= substr(`Date of crime`, 1, 4)) %>% #Mutating this column to only show year
  group_by(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>% 
  #Grouping to show crime count in each postcode by year
  select(`Short Postcode`,`Crime type`,`Date of crime`, `Falls within`) %>%
  na.omit() %>%
  tally() %>% #creating crime count column
  rename(`Crime Count`=n) %>%  #renaming crime count column %>%
  left_join(population_dataset, by = "Short Postcode") %>%
  #joining with population dataset to show district and population
  select(`Short Postcode`,`Crime Count`, `Town/City`, County) %>% #select the required columns
  na.omit()
crime_rank <-crime_dataset_count %>%
  rename(Town=`Town/City`) %>% #renaming to maintain consistency
  group_by(Town) %>%
  summarise(`Mean Crime`= mean(`Crime Count`),County=first(County)) %>%
  arrange((`Mean Crime`)) %>%  #arranging mean crime in ascending order
  mutate(`Crime Score`=10-(`Mean Crime`/1000)) %>% 
  #calculating score. We are subtracting from 10 because lower mean crime need to have higher rank
  select(Town,County,`Crime Score`)
#defining path to save the crime rank csv
file_path <- "Recommended System/Crime rank.csv"

#saving the download crime rank csv
write.csv(crime_rank, file_path, row.names = FALSE)
view(crime_rank)

#-----------School Ranking-----------#

#importing the cleaned school dataset
cleaned_school_dataset <-read_csv('Cleaned Data/Cleaned School Dataset.csv')

school_rank <-cleaned_school_dataset %>%
  mutate(Town= toupper(Town), County= toupper(County)) %>% #converting into all upper case for consistency
  group_by(Town) %>%
  mutate(`Mean Attainment`=mean(`Attainment Score`),County=first(County)) %>%
  arrange(desc(`Mean Attainment`)) %>% #arranging in descending order
  mutate(`School Score`= (`Mean Attainment`/10)) %>%
  select(Town, County, `School Score`) %>%
  distinct()

#defining path to save school rank csv
file_path <- "Recommended System/School rank.csv"

#saving the school rank csv
write.csv(school_rank, file_path, row.names = FALSE)
view(school_rank)

#-----------Joining all the ranking table-----------#

combined_ranking_table <-houseprice_rank %>% #starting with house price rank table
  left_join(download_speed_rank, by = c("Town/City" = "Town", "County" = "County")) %>% #joining with download speed rank table
  na.omit() %>%
  left_join(crime_rank, by = c("Town/City" = "Town", "County" = "County")) %>% #joining with crime rank table
  na.omit() %>%
  left_join(school_rank, by = c("Town/City" = "Town", "County" = "County")) %>% #joining with school rank table
  na.omit()

#-----------Calculation of total score-----------#

final_rank <- combined_ranking_table %>%
  mutate(`Total Score` = (`House Score` + `Download Score` + `Crime Score` + `School Score`) / 4) %>%
  #creating a new column to show the total score
  select(`Town/City`, County, `House Score`, `Download Score`, `Crime Score`,`School Score`, `Total Score`) %>% 
  #arranging the order for columns
  arrange(desc(`Total Score`)) %>%   #showing the highest score first
  mutate(Rank= row_number()) %>%
  select(Rank, everything()) #moving the serial number column at first

#defining path to save final ranks csv
file_path <- "Recommended System/Final rank.csv"

#saving the final ranks csv
write.csv(final_rank, file_path, row.names = FALSE)
view(final_rank)
