

 #read all clean data
housePricing <- read_csv("clean data\\housingData.csv")
internet<- read_csv("clean data\\internetData.csv")
crime<- read_csv("clean data\\crime.csv")
school<- read_csv("clean data\\schoolData.csv")


 #calculate average House pricing according to district
 housePricing<- housePricing %>% 
  group_by(District) %>% 
  mutate(average_housePrice=mean(Price)) %>% 
  filter(row_number()==1)

 #calculate average internet speed using upload and download speed
 internet<- internet %>% 
   group_by(District) %>% 
   mutate(average_internet_speed=mean(`Average download speed (Mbit/s)`)) %>% 
   filter(row_number()==1)
 
 #calculate crime rate
 population<- read_csv("clean data\\populationData.csv")
 
 #calculate crime per thousand population in the district
 #sum of population of district using shortpostcode
 population_<- population %>% 
   group_by(shortPostcode) %>% 
   summarise(shortPostcode_population=sum(Population))
 
 #crime rate per 1000 people for each district
 crime<- crime %>% 
   inner_join(population_,by="shortPostcode") %>% 
   group_by(District) %>% 
   mutate(count=n()) %>% 
   filter(row_number()==1) %>% 
   mutate(crime_rate=(count/shortPostcode_population.x)*1000)
 
 #calculate the mean of Average attainmnet 8 score by district
 school<- school %>% 
   group_by(District) %>% 
   mutate(average_Attainment8Score= mean(`Attainment 8 score`, na.rm=TRUE)) %>% 
   filter(row_number()==1)
 
 #combine all data
 all<- housePricing %>% 
   inner_join(internet,by="District") %>% 
   inner_join(crime,by="District") %>% 
   inner_join(school,by="District") %>% 
   select(Country=Country.x,Postcode=Postcode.x,average_housePrice,average_Attainment8Score,
          average_internet_speed,crime_rate)

 
 
 #--------- Calculate IQR for each variable--------------#
 iqr_housePrice <- IQR(all$average_housePrice)
 iqr_attainment8Score <- IQR(all$average_Attainment8Score)
 iqr_internet_speed <- IQR(all$average_internet_speed)
 iqr_crime_rate <- IQR(all$crime_rate)
 
 # Calculate lower and upper bounds
 upper_bound_housePrice <- quantile(all$average_housePrice, 0.75) + 1.5 * iqr_housePrice
 lower_bound_attainment8Score <- quantile(all$average_Attainment8Score, 0.25) - 1.5 * iqr_attainment8Score
 lower_bound_internet_speed <- quantile(all$average_internet_speed, 0.25) - 1.5 * iqr_internet_speed
 upper_bound_crime_rate <- quantile(all$crime_rate, 0.75) + 1.5 * iqr_crime_rate
 
 # Identify potential outliers
 outliers_housePrice <-  all$average_housePrice > upper_bound_housePrice
 outliers_attainment8Score <- all$average_Attainment8Score < lower_bound_attainment8Score
 outliers_internet_speed <- all$average_internet_speed < lower_bound_internet_speed
 outliers_crime_rate <-  all$crime_rate > upper_bound_crime_rate
 
 #filter the outliers
 outliers <- data.frame(
   average_housePrice = all$average_housePrice[outliers_housePrice],
   average_attatinmnet8Score = all$average_Attainment8Score[outliers_attainment8Score],
   average_internet_speed = all$average_internet_speed[outliers_internet_speed],
   crime_rate = all$crime_rate[outliers_crime_rate]
 )
 
 # Print out the outliers
 print(outliers)
 

 
#---------------top 5-------------------#
 view(all %>% mutate(allover=(average_Attainment8Score+average_internet_speed-crime_rate-average_housePrice)^2) %>% 
     arrange(desc(as.numeric(allover))) %>% 
     slice_head(n=5))
 

