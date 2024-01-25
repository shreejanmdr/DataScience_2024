library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
setwd("C:/Users/DELL/Desktop/220094_Shreejan_DSFD")
#Importing cleaned postcode to LSOA csv into R
cleaned_postcode_to_LSOA<- read_csv("Cleaned Data/Cleaned Postcode To LSOA Code.csv")

#importing population dataset and managing the postcode column
population <- read_csv("Obtained Data/Population Dataset.csv")%>%
  rename(`Short Postcode`= Postcode) %>% #renaming postcode to short postcode
  mutate(`Short Postcode` = gsub(" ", "", `Short Postcode`),  # Remove all spaces
         `Short Postcode` = if_else(nchar(`Short Postcode`) == 5, 
                            paste0(substr(`Short Postcode`, 1, 4), " ", substr(`Short Postcode`, 5, 6)), 
                            paste0(substr(`Short Postcode`, 1, 3), " ", substr(`Short Postcode`, 4, 5)))) 
                            #fixing inconsistent spacing in postcode column %>% 

#cleaning the population dataset further and joining with Postcode to LSOA table
population<- population %>% 
  as_tibble() %>% #converting into tibble 
  right_join(cleaned_postcode_to_LSOA, by="Short Postcode") %>%  
  #Joining with the cleaned Postcode to LSOA dataset by matching Postcode
  na.omit() %>%  #removing null values
  select(S_No, everything()) #moving the serial number column at first


#defining path to save the cleaned dataset
file_path <- "Cleaned Data/Cleaned Population.csv"


#saving the cleaned dataset
write.csv(population,file_path, row.names = FALSE) 
