
 library(tidyverse)


 library(lubridate)


#defining folder path
folder_path <- here("dataset", "housePricing")

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

#Colum names
column <- c("Transaction unique identifier", "Price","Date of Transfer","PostCode","Property Type ","Old/New"
            ,"Duration","PAON","SAON","Street","Locality","Town/City","District","Country","Data Item ","PDF Category Type")

#create a empty dataframe to store housePricing data 
housePricingData <- data.frame()

for (file in csv_files) {
  # Read the current CSV file
  current_data <- read_csv(file)
  colnames(current_data) <- column
  # Convert the Date of Transfer column to datetime type
  current_data$`Date of Transfer` <- as.character(current_data$`Date of Transfer`)
  # Append the current data frame to housePricing
  housePricingData <- bind_rows(housePricingData, current_data)
  
}

rm(current_data)

#housing data
glimpse(housePricingData)

head(housePricingData)

#Checking for duplicate rows
dim(housePricingData)

dim(distinct(housePricingData))

#housing data
glimpse(housePricingData)


#checking for double entry
length(housePricingData$`Transaction unique identifier`)

length(unique(housePricingData$`Transaction unique identifier`))


 #Dates in the data set are in different format so using lubricate package to format dates for consistency.
 housePricingData <- housePricingData %>%
  mutate(`Date of Transfer` = parse_date_time(`Date of Transfer`, orders = c("%m/%d/%Y %H:%M", "%Y-%m-%d")))


 #Taking data of only Oxfordshire and Yorkshire countries and renaming all of Yorkshire country names to Yorkshire.
 housePricingData=housePricingData %>% 
  filter(Country=="OXFORDSHIRE" | Country=="EAST RIDING OF YORKSHIRE" | Country=="WEST YORKSHIRE" | 
           Country=="NORTH YORKSHIRE" | Country=="SOUTH YORKSHIRE") %>% 
  mutate(Country = case_when(
    endsWith(Country, "YORKSHIRE") ~ "YORKSHIRE",
    Country == "OXFORDSHIRE" ~ "OXFORDSHIRE"
  ))


 
 #Selecting columns relevant to our analysis.
 housePricingData<- housePricingData %>% 
  select(Price,`Date of Transfer`,PostCode ,District,Country)
 
 #change to title for consistent data
 housePricingData$District<-str_to_title(housePricingData$District)
 
 #shrotpostcode for district wise calculations
 housePricingData <-housePricingData %>%
   mutate(shortPostcode =  str_extract(PostCode, "^[^[:space:]]+"))
 
 # Remove spaces for consistent data
 housePricingData$PostCode <- str_remove_all(housePricingData$PostCode, "\\s")
 
 #For consistent naming
 housePricingData <- housePricingData %>%
   rename(Postcode = PostCode)

 
  #removing null values
  housePricingData <- housePricingData %>%
    filter(!is.na(Price) & !is.na(Postcode))
  
  #clean dataset
  glimpse(housePricingData)
  
  #write into csv file
 output_path <- file.path("clean data", "housingData.csv")
 write_csv(housePricingData, output_path)  


