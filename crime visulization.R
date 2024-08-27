

#install.packages("tidyverse")
library(tidyverse)

#install.packages("RColorBrewer")
library(RColorBrewer)

library(ggradar)

#install.packages("gridExtra")
library(gridExtra)


#no scientific notations in graph
options(scipen=1000)



crime<- read_csv("clean data\\crime.csv")

glimpse(crime)

#--------box plot----------------#

 #randomly selecting 5 district from each country
 oxfordshire_districts<-crime %>% 
  filter(`Falls within`=="Oxfordshire Police"&(year == 2022 | year==2021)&`Crime type`=="Drugs") %>% 
  distinct(District) %>% 
  sample_n(size = 5, replace = FALSE) %>%
  select(District)

 yorkshire_districts<- crime %>% 
  filter(`Falls within`=="Yorkshire Police"&(year == 2022 | year==2021)&`Crime type`=="Drugs") %>% 
  distinct(District) %>% 
  sample_n(size = 5, replace = FALSE) %>%
  select(District)

 #draw box plot
 crime %>%
  filter(District %in%yorkshire_districts$District | District %in%oxfordshire_districts$District ) %>% 
  #  sample_n(size=100000) %>% 
  group_by(LSOA) %>% 
  mutate(COUNT = n()) %>%
  ggplot(aes(x = District, y = COUNT)) +
  geom_boxplot() +
  facet_wrap(~ `Falls within`,  scales = "free_x") +
  labs(title = "Drug crime rate Yorshire vs Oxfordhsire", x = "District", y = "Crime rate") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))


#-----------pie ------------#

 #randomly selecting 5 district from each country
 oxfordshire_districts_roberry<-crime %>% 
  filter(`Falls within`=="Oxfordshire Police"&(year == 2022 | year==2021)&`Crime type`=="Robbery") %>% 
  distinct(District) %>% 
  sample_n(size = 5, replace = FALSE) %>%
  select(District)

 yorkshire_districts_roberry<- crime %>% 
  filter(`Falls within`=="Yorkshire Police"&(year == 2022 | year==2021)&`Crime type`=="Robbery") %>% 
  distinct(District) %>% 
  sample_n(size = 5, replace = FALSE) %>%
  select(District)

 #draw pie chart
 oxfordshire_pie<- crime %>% filter(District %in% oxfordshire_districts_roberry$District) %>% 
  group_by(District) %>% 
  summarise(count=n())  %>% 
  ggplot( aes(x = "", y = count, fill = District)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Oxfordshire Robbery Rate", fill = "District")+
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text = element_blank(),
        axis.title = element_blank())+
  scale_fill_brewer(palette = "Paired")
  
 #draw pie chart
 yorkshire_pie<-crime %>% filter(District %in% yorkshire_districts_roberry$District) %>% 
  group_by(District) %>% 
  summarise(count=n())  %>% 
  ggplot( aes(x = "", y = count, fill = District)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Yorkshire Robbery Rate", fill = "District")+
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text = element_blank(),
        axis.title = element_blank())+
  scale_fill_brewer(palette = "Paired")

 # Display the pie chart side by side
 grid.arrange(oxfordshire_pie, yorkshire_pie, ncol = 2)



#-----------line graph----------#


 #to calculate drug offence rate accoring to population
 population<-read_csv("clean data\\populationData.csv")

 #sum of population of district using shortpostcode
 population_<- population %>% 
  group_by(shortPostcode) %>% 
  summarise(shortPostcode_population=sum(Population))


 #create line graph
 crime %>%
  filter(`Crime type` == "Drugs") %>% 
  inner_join(population_, by = "shortPostcode") %>%
  mutate(date = parse_date_time(year, orders = "%y"),year=year(date)) %>%
  group_by(Country, year) %>%
  summarize(drug_rate_per_thousand = n() / sum(shortPostcode_population) * 1000) %>%
  ggplot(aes(x = year, y = drug_rate_per_thousand, color = Country)) +
  geom_line() +
  labs(x = "Year", y = "Drug Offense Rate per 1000 People", title = "Drug Offense Rate by Country") +
  theme_minimal()
 
 
 
#------------radar chart-----------------#
 
 #random 3 district to show 
 oxfordshire_vehical_crime<- crime %>% 
   filter(`Crime type` == "Vehicle crime" & Country=="Oxfordshire") %>% 
   distinct(District) %>% 
   sample_n(size = 3, replace = FALSE) %>%
   select(District)
 
 
 #prepare data to create radar chart
 data_oxforshisre <- crime %>%
   filter(District %in% oxfordshire_vehical_crime$District ) %>% 
   group_by(District, year) %>%
   summarise(count=n()) %>% 
   pivot_wider(names_from = year, values_from = count) 
 
 
 
 # Create the radar chart oxfordshire
 radar_plot_oxfordshire <- data_oxforshisre %>%
   ggradar(
     grid.label.size = 0,
     axis.label.size = 5,
     group.point.size = 3,
     grid.max = 150000  # Adjust the maximum value for the grid
   )+
   labs(title = "Oxfordshire Vehical Crime Rate")+
   theme(plot.title = element_text(size=18,hjust = 0.5),
         legend.text = element_text(color = "black", size = 10))
 
 
 #select random 3 district to show
 yorkshire_vehical_crime<- crime %>% 
   filter(`Crime type` == "Vehicle crime" & Country=="Yorkshire") %>% 
   distinct(District) %>% 
   sample_n(size = 3, replace = FALSE) %>%
   select(District)
 
 #prepare data to show in radar chart
 data_yorkshire <- crime %>% 
   filter(`Crime type` == "Vehicle crime"& District %in% yorkshire_vehical_crime$District) %>% 
   group_by(District, year) %>% 
   summarise(count = n()) %>% 
   pivot_wider(names_from = year, values_from = count) 

 # Create the radar chart yorkshire
 radar_plot_yorkshire <- data_yorkshire %>%
   ggradar(
     grid.label.size = 0,
     axis.label.size = 5,
     group.point.size = 3,
     grid.max = 35000  # Adjust the maximum value for the grid
   )+
   labs(title = "Yorkshire Vehical Crime Rate")+  
     theme(plot.title = element_text(size=18,hjust = 0.5),
           legend.text = element_text(color = "black", size = 10)) 
 
 # Display the radar charts side by side
 grid.arrange(radar_plot_oxfordshire, radar_plot_yorkshire, ncol = 2) 
 
 
 ?theme()