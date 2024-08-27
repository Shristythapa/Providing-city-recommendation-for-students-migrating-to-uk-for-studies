library(tidyverse)


#read population data
population<- read_csv("dataset\\Population2011.csv")



 #checking for duplicate data
 dim(population)
 dim(distinct(population))


 #short postcode for year wise calculation
 population <-population %>%
  mutate(shortPostcode =  str_extract(Postcode, "^[^[:space:]]+"))

 # Remove spaces for consistent data
 population$Postcode <- str_remove_all(population$Postcode, "\\s")

 #write clean population data into csv file
 write_csv(population,"clean data\\populationData.csv")
