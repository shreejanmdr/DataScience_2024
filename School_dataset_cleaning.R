library(tidyverse)
library(dplyr)
library(lubridate)
setwd("C:/Users/DELL/Desktop/220094_Shreejan_DSFD")

#-----------2018 Kent School Dataset Cleaning-----------#

kent_2018_2019_school <- read_csv("Obtained Data/School Dataset/Kent 2018-2019 School Dataset.csv") %>% 
  select(SCHNAME,ATT8SCR, TOWN, PCODE ) %>%  #Selecting only the required columns
  rename(`School Name`=SCHNAME,`Attainment Score`=ATT8SCR, Town= TOWN, `Postcode`= PCODE) %>% 
  as_tibble() %>% #Converting into tibble
  mutate('Short Post Code'= substr(Postcode, 1, 5)) %>% 
  na.omit() %>%  #Removing rows with null value
  filter (`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>%  #removing NE and SUPP from Attainment Score row
  mutate(County = "Kent") %>% #adding a new column for county 
  mutate(Year= "2018") %>% #adding a new column for year
  mutate(S_No = row_number()) %>% #Adding a new serial number column
  select(S_No, everything())  #moving the serial number column at first

#-----------2021 Kent School Dataset Cleaning-----------#

kent_2021_2022_school <- read_csv("Obtained Data/School Dataset/Kent 2021-2022 School Dataset.csv") %>% 
  select(SCHNAME,ATT8SCR, TOWN, PCODE ) %>%  #Selecting only the required columns
  rename(`School Name`=SCHNAME,`Attainment Score`=ATT8SCR, Town= TOWN, `Postcode`= PCODE) %>% 
  as_tibble() %>% #Converting into tibble
  mutate('Short Post Code'= substr(Postcode, 1, 5)) %>% 
  na.omit() %>%  #Removing rows with null value
  filter (`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>%  #removing NE and SUPP from Attainment Score row
  mutate(County = "Kent") %>% #adding a new column for county 
  mutate(Year= "2021") %>% #adding a new column for year
  mutate(S_No = row_number()) %>% #Adding a new serial number column
  select(S_No, everything())  #moving the serial number column at first
  

#-----------2018 Surrey School Dataset Cleaning-----------#

Surrey_2018_2019_school <- read_csv("Obtained Data/School Dataset/Surrey 2018-2019 School Dataset.csv") %>% 
  select(SCHNAME,ATT8SCR, TOWN, PCODE ) %>%  #Selecting only the required columns
  rename(`School Name`=SCHNAME,`Attainment Score`=ATT8SCR, Town= TOWN, `Postcode`= PCODE) %>% 
  as_tibble() %>% #Converting into tibble
  mutate('Short Post Code'= substr(Postcode, 1, 5)) %>% 
  na.omit() %>%  #Removing rows with null value
  filter (`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>%  #removing NE and SUPP from Attainment Score row
  mutate(County = "Surrey") %>% #adding a new column for county 
  mutate(Year= "2018") %>% #adding a new column for year
  mutate(S_No = row_number()) %>% #Adding a new serial number column
  select(S_No, everything())  #moving the serial number column at first

#-----------2021 Surrey School Dataset Cleaning-----------#

Surrey_2021_2022_school <- read_csv("Obtained Data/School Dataset/Surrey 2021-2022 School Dataset.csv") %>% 
  select(SCHNAME,ATT8SCR, TOWN, PCODE ) %>%  #Selecting only the required columns
  rename(`School Name`=SCHNAME,`Attainment Score`=ATT8SCR, Town= TOWN, `Postcode`= PCODE) %>% 
  as_tibble() %>% #Converting into tibble
  mutate('Short Post Code'= substr(Postcode, 1, 5)) %>% 
  na.omit() %>%  #Removing rows with null value
  filter (`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>%  #removing NE and SUPP from Attainment Score row
  mutate(County = "Surrey") %>% #adding a new column for county 
  mutate(Year= "2021") %>% #adding a new column for year
  mutate(S_No = row_number()) %>% #Adding a new serial number column
  select(S_No, everything())  #moving the serial number column at first

#merging all the cleaned dataset into a single tibble
combined_school_dataset= bind_rows(kent_2018_2019_school, kent_2021_2022_school, Surrey_2018_2019_school, Surrey_2021_2022_school)


#defining path to save the cleaned dataset
file_path <- "Cleaned Data/Cleaned School Dataset.csv"


#saving the cleaned dataset
write.csv(combined_school_dataset,file_path, row.names = FALSE) 

