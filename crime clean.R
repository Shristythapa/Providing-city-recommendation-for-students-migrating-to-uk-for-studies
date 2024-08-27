 library(tidyverse)


 library(lubridate)



 # Specify the folder path
 folder_path <- "dataset\\crime"

 # Retrieve a list of all CSV files in the folder
 csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

 # Create an empty data frame to store the merged data
 crimeData <- data.frame()

 # Iterate over each CSV file, read its contents, and append it to the merged data frame
 for (file in csv_files) {
  data <- read_csv(file)
  crimeData <- rbind(crimeData, data)
 }

 #taking a look at crime data
 glimpse(crimeData)

 #checking for duplicate rows
 dim(crimeData)

 dim(unique(crimeData))
 
 #remove duplicate rows
crimeData <- distinct(crimeData)

length(unique(crimeData$`Crime ID`))

#remove duplicate entries of crimeID
crimeData <- crimeData[!duplicated(crimeData$`Crime ID`, fromLast = TRUE), ]


 #formate date
 crimeData$Month <- as.Date(paste0(crimeData$Month, "-01"), format = "%Y-%m-%d")

 #renaming column month as date
 crimeData <- crimeData %>% rename(Date = Month)

 #adding year column for year wise calculations
 crimeData<- crimeData %>% 
  mutate(year= year(Date))
 

 #taking only relevant columns
 crimeData <- crimeData %>% 
  select(year,`Falls within`,`LSOA code`,`Crime type`)

crimeData <- crimeData %>%
  mutate(`Falls within` = case_when(
    endsWith(`Falls within`, "Yorkshire Police") ~ "Yorkshire Police",
    TRUE ~ "Oxfordshire Police"
  ))



 #create country column according to police name
 crimeData<- crimeData %>% 
  mutate('Country'=case_when(
    `Falls within`=="Yorkshire Police" ~ "Yorkshire",
    `Falls within`=="Oxfordshire Police" ~ "Oxfordshire"
  ))


 #using postcode to laos data to get district information
 PostcodeToLAOS <- read_csv("clean data\\PostCodeToLAOS.csv")
 
 glimpse(PostcodeToLAOS)
 
 #taking unique entries of post code in postcodetoloas
 #and joining with the common column in crime data
 crimeData<- PostcodeToLAOS %>%
  group_by(LSOA) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  right_join(crimeData, by = c("LSOA"="LSOA code"))

 #taking a look at clean data
 glimpse(crimeData)
 
 
 #write clean data into seperate csv file 
 write_csv(cleanCrimeData,"clean data\\crime.csv")

