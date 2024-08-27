

options(scipen=1000)

#imports
library(tidyverse)

rm(list = ls())


#-----------house prices and download speed--------------#


 housePricing <- read_csv("clean data\\housingData.csv")
 internet<- read_csv("clean data\\internetData.csv")

 housePricing_internet_ <- housePricing %>% 
   inner_join(internet,by="Postcode") %>% 
   group_by(shortPostcode.x) %>% 
   mutate(ads=mean(`Average download speed (Mbit/s)`),average_price=mean(Price))
 

 
 
 housePricing %>% 
  inner_join(internet,by="Postcode") %>% 
  group_by(shortPostcode.x) %>% 
   mutate(ads=mean(`Average download speed (Mbit/s)`),average_price=mean(Price)) %>% 
  ggplot(aes(x=ads,y=average_price)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_fill_brewer("PuOr")+
  labs(title = "House Pricing and Download speed",x="Average Download Speed", y= "Average House Pricing") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))
  
 summary( lm(average_price~ads,data=housePricing_internet_))
 
#----------house pricing vs drug crime rate-----------# 
 


 housePricing <- read_csv("clean data\\housingData.csv")
 crime<- read_csv("clean data\\crime.csv")

 #to calculate drug crime rate per thousand
 population<- read_csv("clean data//populationData.csv")

 #sum of population of district using shortpostcode
 population_<- population %>% 
   group_by(shortPostcode) %>% 
   summarise(shortPostcode_population=sum(Population))

 #crime rate per 1000 people for each district
 crime_<-crime %>% 
         filter(`Crime type`=="Drugs") %>% 
         inner_join(population_,by="shortPostcode") %>%
         group_by(shortPostcode) %>% 
         mutate(count=n()) %>% 
         filter(row_number() == 1) %>% 
         mutate(drug_crime_rate=(count/shortPostcode_population)*1000)
          

 #calculate average housepricing of each district
 housePricing_<-housePricing %>% 
  group_by(shortPostcode) %>% 
  mutate(mean_price_district=mean(Price)) %>% 
  slice(1) %>% 
  select(mean_price_district)

 #join housepricing and crime data
 housepricing_crime_ <- housePricing_ %>% 
  inner_join(crime_, by="shortPostcode")
 

 #create linear model
 housepricing_crime_ %>% 
  ggplot(aes(x=drug_crime_rate,y=mean_price_district)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE) +
  scale_fill_brewer("PuOr")+
  labs(title = "House Pricing according and Drug crime Rate",x="Drug crime Rate", y= "House Pricing") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))
 
 summary(lm(mean_price_district~drug_crime_rate,data=housepricing_crime_))


#----------average attainment 8 score vs house price---------#

rm(list=ls())

 school <- read_csv("clean data\\schoolData.csv")
 housePricing <- read_csv("clean data\\housingData.csv")

 school_housePricing<-school %>% 
  inner_join(housePricing, by="Postcode")

 school_housePricing %>% 
  ggplot(aes(x=`Attainment 8 score`,y=Price)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE) +
  scale_fill_brewer("PuOr")+
  labs(title = "House Pricing and Average Attainment 8 score ",x="Average attainmnet 8 score", y= "House price") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))
  

 summary(lm(Price~`Attainment 8 score`, data=school_housePricing))



#----------average download speed vs drug offence----------#

 rm(list=ls())

 internet<- read_csv("clean data\\internetData.csv")
 crime<- read_csv("clean data\\crime.csv")

 #to calculate drug crime rate per thousand
 population<- read_csv("clean data//populationData.csv")


 #sum of population of district using shortpostcode
 population_<- population %>% 
  group_by(shortPostcode) %>% 
  summarise(shortPostcode_population=sum(Population))

 #crime rate per 1000 people for each district
 crime_<-crime %>% 
  filter(`Crime type`=="Drugs") %>% 
  inner_join(population_,by="shortPostcode") %>%
  group_by(shortPostcode) %>% 
  mutate(count=n()) %>% 
  filter(row_number() == 1) %>% 
  mutate(drug_crime_rate=(count/shortPostcode_population)*1000)

 #calculate average
 internet_<- internet %>% 
  group_by(shortPostcode) %>% 
  mutate(avg_download_speed= mean(`Average download speed (Mbit/s)`, na.rm=TRUE)) %>% 
  slice(1) %>% 
  select(avg_download_speed)

crime_internet_ <- crime_ %>% 
  inner_join(internet_, by = "shortPostcode") 

crime_internet_ %>% 
  ggplot(aes(x=drug_crime_rate,y=avg_download_speed)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE) +
  scale_fill_brewer("PuOr")+
  labs(title = "Average Download and Drug crime ",x="Drug crime Rate", y= "Average Download speed") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))



summary(lm(avg_download_speed~drug_crime_rate, data=crime_internet_))

#----------attainment 8 score vs average download---------#

rm(list=ls())

 school <- read_csv("clean data\\schoolData.csv")
 internet<- read_csv("clean data\\internetData.csv")

 school_internet_<-school %>% 
  inner_join(internet,by="Postcode")


 school_internet_ %>% 
  ggplot(aes(x=`Average download speed (Mbit/s)`,y=`Attainment 8 score`)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE) +
  scale_fill_brewer("PuOr")+
  labs(title = "Average Attainment rate vs Average download speed ",x="Average downoload speed", y= "Attainmnet 8 rate") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))

 summary(lm(`Attainment 8 score`~`Average download speed (Mbit/s)`, data=school_internet_))