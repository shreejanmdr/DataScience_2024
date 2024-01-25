library(tidyverse)
library(dplyr)
library(lubridate)
setwd("C:/Users/DELL/Desktop/220094_Shreejan_DSFD")

#importing cleaned house price dataset
cleaned_houseprices = read_csv("Cleaned Data/Cleaned House Prices.csv") 

#Cleaning and joining data through the use of pipe operator
postcode_to_lsoa = read_csv("Obtained Data/Postcode to LSOA.csv") %>%
  #importing Postcode to LSOA csv file
  select(pcd7, lsoa11cd) %>% #selecting only required columns
  rename(Postcode= pcd7, `LSOA Code`= lsoa11cd) %>% #renaming columns
  right_join(cleaned_houseprices, by="Postcode") %>% 
  #Joining with the cleaned house price dataset by matching Postcode
  select(`LSOA Code`, Postcode,`Short Postcode`,`Town/City`, District, County, ) %>% 
  #selecting only required columns
  mutate(S_No = row_number()) %>% #Adding a new serial number column
  select(S_No, everything()) #moving the serial number column at first


#defining path to save the cleaned dataset
file_path = "Cleaned Data/Cleaned Postcode To LSOA Code.csv"


#saving the cleaned dataset
write.csv(postcode_to_lsoa,file_path, row.names = FALSE) 

