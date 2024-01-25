library(tidyverse)
library(dplyr)
library(lubridate)
setwd("C:/Users/DELL/Desktop/220094_Shreejan_DSFD")

# Define the path to the main directory containing all the year-month folders
main_dir = "Obtained Data/Crime Dataset"

# Create a list of all CSV file paths
file_paths = list.files(main_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

# Read and combine all CSV files into one dataframe
combined_crime_dataset = file_paths %>%
  set_names() %>% # Ensure each element in file_paths is named
  map_df(~read_csv(.x)) %>%   # Apply read_csv to each file path
  as_tibble() #converting into tibble
   
#Importing cleaned postcode to LSOA csv into R
cleaned_postcode_to_LSOA= read_csv("Cleaned Data/Cleaned Postcode To LSOA Code.csv")

#Cleaning the combined crime data set through the use of pipe operator
combined_crime_dataset= combined_crime_dataset %>% 
  select(Month, `Falls within`, `Crime type`, `LSOA code`) %>% #selecting only columns that are required
  rename(`Date of crime`= `Month`, `LSOA Code`= `LSOA code`) %>% #renaming the month column
  right_join(cleaned_postcode_to_LSOA, join_by(`LSOA Code`)) %>% #joining with another table to show towns 
  select(`Date of crime`, `Falls within`, `Crime type`, `LSOA Code`, `Postcode`, `Short Postcode`, `Town/City`) %>% 
  #selecting only columns that are requried
  na.omit() %>% #removing null values
  mutate(S_No = row_number()) %>% #Adding a new serial number column
  select(S_No, everything()) #moving the serial number column at first


#defining path to save the cleaned dataset
file_path = "Cleaned Data/Cleaned Crime Dataset.csv"


#saving the cleaned dataset
write.csv(combined_crime_dataset,file_path, row.names = FALSE) 


  





