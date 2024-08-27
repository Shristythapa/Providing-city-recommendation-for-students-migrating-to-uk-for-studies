
library(tidyverse)




 #reading data add adding year column accordingly
 year_2019_OX<- read_csv("dataset\\internet speed\\2019 internet\\201905_fixed_pc_performance_r01_OX.csv")
 year_2019_YO<- read_csv("dataset\\internet speed\\2019 internet\\201905_fixed_pc_performance_r01_YO.csv") 

 internet_2019<-  bind_rows(year_2019_OX, year_2019_YO) %>% 
  mutate(year=2019)

 year_2020_OX<-read_csv("dataset\\internet speed\\2020 internet\\202006_fixed_pc_performance_r03_OX.csv")
 year_2020_YO<- read_csv("dataset\\internet speed\\2020 internet\\202006_fixed_pc_performance_r03_YO.csv")

 internet_2020<- bind_rows(year_2020_OX,year_2020_YO) %>% 
  mutate(year=2020)

 year_2021_OX<- read_csv("dataset\\internet speed\\2021 internet\\202105_fixed_pc_performance_r01_OX.csv")
 year_2021_YO<- read_csv("dataset\\internet speed\\2021 internet\\202105_fixed_pc_performance_r01_YO.csv")

 internet_2021<- bind_rows(year_2021_OX,year_2021_YO) %>% 
  mutate(year=2021)

 year_2022_OX<- read_csv("dataset\\internet speed\\2022 internet\\202205_fixed_pc_performance_r02_OX.csv")
 year_2022_YO<- read_csv("dataset\\internet speed\\2022 internet\\202205_fixed_pc_performance_r02_YO.csv")

 internet_2022<- bind_rows(year_2022_OX,year_2022_YO) %>% 
  mutate(year=20022)

 #conbining all data set
 internetData <- bind_rows(internet_2019, internet_2020, internet_2021, internet_2022)


 #looking at the dataset
 glimpse(internetData)

#Checking the dimension to see if the dataset contains duplicate rows
dim(internetData)

dim(unique(internetData))

 #Taking only the relevant columns to solve the problem.
 internetData <- internetData %>% 
  select("postcode","Average download speed (Mbit/s)","Average upload speed (Mbit/s)","year")
 


 #Adding country column according the postcode.
 internetData=internetData %>% 
  mutate(Country = case_when(
    startsWith(`postcode`,"OX") ~ "Oxforshire",
    startsWith(`postcode`,"YO" )~ "Yorkshire"
  ))



 #remove all na value in country as not relevent to yorkshire or oxfordshire
 internetData <- internetData[complete.cases(internetData$Country), ]



#to get the information on which district a paticular post code is realated to
PostcodeToLAOS <- read_csv("clean data\\PostCodeToLAOS.csv")

#looking at the post code data
glimpse(PostcodeToLAOS)



 #Removing duplicate postcode enries and joining the data
internetData<-PostcodeToLAOS %>% 
  group_by(Postcode) %>% 
  filter(row_number()==1) %>% 
  right_join(internetData, by = c("Postcode"="postcode")) 
 
 #removing null values
 internetClean <- internetData %>%
   filter(!is.na(`Average download speed (Mbit/s)`) | !is.na(Postcode) | !is.na(District) )

 internetClean <- internetClean[complete.cases(internetClean$District), ]
 

 #clean internet data
 glimpse(internetClean)


 #write clean data into csv file
 write_csv(internetClean,"clean data\\internetData.csv")

