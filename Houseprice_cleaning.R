library(tidyverse)
library(dplyr)
library(lubridate)
setwd("C:/Users/DELL/Desktop/220094_Shreejan_DSFD")

#-----------2019 Dataset Cleaning#

#Cleaning data through the use of pipe operator
#-----------2019 Dataset Cleaning#
houseprices_2019 = read_csv("Obtained Data/House Price Dataset/House Price Dataset 2019.csv", col_names = FALSE) %>%  #Importing CSV into R
  setNames(c("Transaction unique identifier", "Price", "Date of Transfer", "Postcode", "Property Type", "Old/New", "Duration", "PAON", 
             "SAON", "Street", "Locality", "Town/City", "District", "County", "PPD Category type", "Record Status")) %>% #Changing Column name
  as_tibble() %>% #Converting into tibble
  na.omit() %>% #Removing rows with null value
  select(Price, `Date of Transfer`, Postcode, `Town/City`, District, County) %>% #selecting only columns that are required
  filter(County =="KENT" | County== "SURREY") %>% #Preserving rows with Kent and Surrey as county
  mutate(`Date of Transfer` = year(as.Date(`Date of Transfer`, format = "%y/%m/%d"))) %>%  #modifying the date of transfer column to only show year
  mutate(S_No = row_number()) %>% #Adding a new serial number column
  select(S_No, everything()) #moving the serial number column at first


#-----------2020 Dataset Cleaning#

houseprices_2020 =read_csv("Obtained Data/House Price Dataset/House Price Dataset 2020.csv", col_names = FALSE) %>%  #Importing CSV into R
  setNames(c("Transaction unique identifier", "Price", "Date of Transfer", "Postcode", "Property Type", "Old/New", "Duration", "PAON", 
             "SAON", "Street", "Locality", "Town/City", "District", "County", "PPD Category type", "Record Status")) %>% #Changing Column name
  as_tibble() %>% #Converting into tibble
  na.omit() %>% #Removing rows with null value
  select(Price, `Date of Transfer`, Postcode, `Town/City`, District, County) %>% #selecting only columns that are required
  filter(County =="KENT" | County== "SURREY") %>% #Preserving rows with Kent and Surrey as county
  mutate(`Date of Transfer` = year(as.Date(`Date of Transfer`, format = "%y/%m/%d"))) %>%  #modifying the date of transfer column to only show year
  mutate(S_No = row_number()) %>% #Adding a new serial number column
  select(S_No, everything()) #moving the serial number column at first

#-----------2021 Dataset Cleaning#

houseprices_2021 =read_csv("Obtained Data/House Price Dataset/House Price Dataset 2021.csv", col_names = FALSE) %>%  #Importing CSV into R
  setNames(c("Transaction unique identifier", "Price", "Date of Transfer", "Postcode", "Property Type", "Old/New", "Duration", "PAON", 
             "SAON", "Street", "Locality", "Town/City", "District", "County", "PPD Category type", "Record Status")) %>% #Changing Column name
  as_tibble() %>% #Converting into tibble
  na.omit() %>% #Removing rows with null value
  select(Price, `Date of Transfer`, Postcode, `Town/City`, District, County) %>% #selecting only columns that are required
  filter(County =="KENT" | County== "SURREY") %>% #Preserving rows with Kent and Surrey as county
  mutate(`Date of Transfer` = year(as.Date(`Date of Transfer`, format = "%y/%m/%d"))) %>%  #modifying the date of transfer column to only show year
  mutate(S_No = row_number()) %>% #Adding a new serial number column
  select(S_No, everything()) #moving the serial number column at first


#-----------2022 Dataset Cleaning#

houseprices_2022 =read_csv("Obtained Data/House Price Dataset/House Price Dataset 2022.csv", col_names = FALSE) %>%  #Importing CSV into R
  setNames(c("Transaction unique identifier", "Price", "Date of Transfer", "Postcode", "Property Type", "Old/New", "Duration", "PAON", 
             "SAON", "Street", "Locality", "Town/City", "District", "County", "PPD Category type", "Record Status")) %>% #Changing Column name
  as_tibble() %>% #Converting into tibble
  na.omit() %>% #Removing rows with null value
  select(Price, `Date of Transfer`, Postcode, `Town/City`, District, County) %>% #selecting only columns that are required
  filter(County =="KENT" | County== "SURREY") %>% #Preserving rows with Kent and Surrey as county
  mutate(`Date of Transfer` = year(as.Date(`Date of Transfer`, format = "%y/%m/%d"))) %>%  #modifying the date of transfer column to only show year
  mutate(S_No = row_number()) %>% #Adding a new serial number column
  select(S_No, everything()) #moving the serial number column at first

#merging all the cleaned dataset into a single tibble

combined_houseprices= bind_rows(houseprices_2019, houseprices_2020, houseprices_2021, houseprices_2022) %>% 
  mutate(`Short Postcode`= substr(Postcode, 1,5)) #adding another column to the combine dataset


#defining path to save the cleaned dataset
file_path = "Cleaned Data/Cleaned House Prices.csv"


#saving the cleaned dataset
write.csv(combined_houseprices,file_path, row.names = FALSE) 
  
