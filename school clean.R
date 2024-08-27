library(tidyverse)

rm(list=ls())

 #read school data
 school_2018To2019 <- read_csv("dataset\\school\\2018-2019\\england_ks4final.csv")
 school_2021To2022 <- read_csv("dataset\\school\\2021-2022\\england_ks4final.csv")
 
 #view school data
 school_2018To2019

 school_2021To2022

 #take only necessary data from school data from both data set
 school_2018To2019<- school_2018To2019 %>% 
  select("SCHNAME","PCODE","ATT8SCR")
 
 school_2021To2022<- school_2021To2022 %>% 
   select("SCHNAME","PCODE","ATT8SCR")
 
 #add year column
 school_2018To2019 <- school_2018To2019 %>%
   mutate(Year = "2018-2019")
 
 school_2021To2022<- school_2021To2022 %>% 
   mutate(Year = "2021-2022")

 #combine data set of two school year
 schoolData<- bind_rows(school_2021To2022,school_2018To2019)
 
 glimpse(schoolData)
 

 
 #checking for duplicate data
 dim(schoolData)
 
 dim(distinct(schoolData))
 

 
 #postcode data to get data on district of school
 PostcodeToLAOS <- read_csv("clean data\\PostCodeToLAOS.csv")


 #take observation related to oxfordshire and yorkshire
 #add country column to dataset using postcode data
 schoolData <- schoolData %>% 
  filter(startsWith(PCODE, "OX") | startsWith(PCODE, c("YO"))) %>% 
  mutate(Country = ifelse(startsWith(PCODE, "OX"), "OXFORDSHIRE", "YORKSHIRE"))

 # Remove spaces for consistent data
 schoolData$PCODE <- str_remove_all(schoolData$PCODE, "\\s")

 #renaming column for better understanding
 schoolData<-schoolData %>% 
   rename("Attainment 8 score"="ATT8SCR","School Name"=SCHNAME)
 
 #removing null values
 schoolData <- schoolData %>%
   filter(!is.na(`Attainment 8 score`) | !is.na(PCODE))
 
 #change attainment 8 score to numeric data
 schoolData$`Attainment 8 score`<-as.numeric(schoolData$`Attainment 8 score`)
 

 #join school data with postcode data to get data on district
 schoolClean<-PostcodeToLAOS %>%
  group_by(Postcode) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(Postcode, shortPostcode, District) %>% 
  right_join(schoolData, by = c("Postcode"="PCODE"))


 #clean school data
 glimpse(schoolClean)

 #write clean school data to csv file
 write_csv(schoolClean,"clean data\\schoolData.csv")

