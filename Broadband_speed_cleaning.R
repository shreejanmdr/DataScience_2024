library(tidyverse)
library(dplyr)
library(lubridate)
setwd("C:/Users/DELL/Desktop/220094_Shreejan_DSFD")

#Importing cleaned postcode to LSOA csv into R
cleaned_postcode_to_LSOA= read_csv("Cleaned Data/Cleaned Postcode To LSOA Code.csv")

#Using pipe operator to Clean and join data  
broadband_speed=read_csv("Obtained Data/Broadband Speed.csv") %>% #Importing broadband speed csv into R
  as_tibble() %>% #converting into tibble 
  select('Average download speed (Mbit/s)', postcode_space) %>%  #only selecting columns that are required 
  rename(Postcode= `postcode_space`) %>% #renaming the post_space column to Postcode
  right_join(cleaned_postcode_to_LSOA, by="Postcode") %>% #Joining with the cleaned house price dataset by matching Postcode
  select('Average download speed (Mbit/s)',Postcode, `Short Postcode`, `Town/City`, District, County,) %>% #selecting only required columns
  na.omit() %>%  #Removing rows with null value
  mutate(`Short Postcode`= substr(Postcode, 1,5)) %>% #Filling missing short code values
  mutate(S_No = row_number()) %>% #Adding a new serial number column
  select(S_No, everything()) #moving the serial number column at first

 
# path defining to save the cleaned dataset
file_path = "Cleaned Data/Cleaned Broadband Speed Dataset.csv"


#cleaned dataset saving
write.csv(broadband_speed,file_path, row.names = FALSE) 





