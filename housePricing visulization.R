
 #install.packages("tidyverse")
 library(tidyverse)

 #install.packages("RColorBrewer")
 library(RColorBrewer)
 

#EDA


options(scipen=1000)

housePricing <- read_csv("clean data/housingData.csv")

glimpse(housePricing)


 # Filter the data for each country and select 5 random districts
 district_yorkshire <- housePricing %>%
  filter(Country == "YORKSHIRE") %>%
  distinct(District) %>%
  sample_n(size = 5, replace = FALSE) %>%
  select(District)

 district_oxfordshire <- housePricing %>%
  filter(Country == "OXFORDSHIRE") %>%
  distinct(District) %>%
  sample_n(size = 5, replace = FALSE) %>%
  select(District)

 # Filter the data based on the selected districts and plot the boxplot
 housePricing %>%
  filter((District %in% district_yorkshire$District | District %in% district_oxfordshire$District)& year(`Date of Transfer`)==2022) %>%
  sample_n(size = 500, replace = FALSE) %>%
  ggplot(aes(x = District, y = Price)) +
  geom_boxplot() +
  facet_wrap(~ Country, scales = "free_x") +
  labs(title = "Housing Price in Oxfordshire and Yorkshire", x = "District", y = "Price") +
  theme_minimal()  +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(color = "black", size = 6),
    axis.title = element_text(color = "black", size = 9),
    legend.title = element_blank(),
  )



#bargraph of house prices in districts

housePricing%>%
  filter((District %in% district_yorkshire$District | District %in% district_oxfordshire$District)&year(`Date of Transfer`)==2022) %>%
  group_by(District) %>% 
  mutate(mean_housePrice=mean(Price)) %>% 
  ggplot( aes(x = Country, y = mean_housePrice, fill = District)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "District", y = "Price", title = "House Pricing in Oxfordshire and Yorkshire year 2022") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))


#line graph

housePricing %>%
  group_by(Country,year(housePricing$`Date of Transfer`)) %>% 
  mutate(meanPrice=mean(Price)) %>% 
  ggplot(aes(x=year(`Date of Transfer`),y=meanPrice, colour= Country)) +
  geom_line()+
  theme_minimal()+
  scale_fill_brewer("PuOr")+
  labs(title = "Yearly house Pricing", x="Year", y="House Pricing")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))

