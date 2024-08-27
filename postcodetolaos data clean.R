library(tidyverse)

#read postocde data
 PostcodeToLAOS <- read_csv("dataset\\Postcode to LSOA.csv")
 
 #taking look at the postcodetolaos data
 glimpse(PostcodeToLAOS)

 
 #checking if duplicate rows exist
 dim(PostcodeToLAOS)

 dim(unique(PostcodeToLAOS))

 #taking only required values
 PostcodeToLAOS<- PostcodeToLAOS %>% 
  select(pcds,lsoa11cd,ladnm)
 
 #renaming columns to a meaning full name for better understading
 PostcodeToLAOS <- PostcodeToLAOS %>% rename(District = ladnm)
 

 PostcodeToLAOS <- PostcodeToLAOS %>% rename(Postcode = pcds)
 
 
 PostcodeToLAOS <- PostcodeToLAOS %>%  rename(LSOA = lsoa11cd)
 
 
 #short post code provides information on district to perform district wise evaluation
 PostcodeToLAOS <-PostcodeToLAOS %>%
  mutate(shortPostcode =  str_extract(Postcode, "^[^[:space:]]+"))
 
 # Remove spaces for consistent data
 PostcodeToLAOS$Postcode <- str_remove_all(PostcodeToLAOS$Postcode, "\\s")
 

 #taking a look at clean dataset
 glimpse(PostcodeToLAOS)

 #write clean data into a seperate csv file
 write_csv(PostcodeToLAOS,"clean data\\PostCodeToLAOS.csv")




