

#install.packages("tidyverse")
library(tidyverse)

#install.packages("RColorBrewer")
library(RColorBrewer)



school <- read_csv("clean data\\schoolData.csv")

glimpse(school)

unique(school$Country)

 #selecting 5 random district from each country
 districtInYorkshire<- school %>% 
   filter(Country=="YORKSHIRE") %>% 
   distinct(District) %>% 
   sample_n(size=5,replace=FALSE) %>% 
   select("District")
 
 districtInOxfordine<- school %>%
   filter(Country=="OXFORDSHIRE") %>% 
   distinct(District) %>% 
   sample_n(size=5,replace=FALSE) %>% 
   select(District)


 #boxplot of districts in oxforshshire and yorkshire showing the averag attainmnet rate 
 school %>%
  filter(District %in% districtInOxfordine$District | District %in% districtInYorkshire$District) %>%
  na.omit() %>%  # Remove rows with NA values in the ATT8SCR column
  ggplot(aes(x = District, y = `Attainment 8 score`)) +
  geom_boxplot() +
  facet_wrap(~ Country, scales = "free") +  # Replace Country with TOWN or District as per your data
  labs(title = "Average ATT8SCR by District", x = "District", y = "Average ATT8SCR") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))
 
 #------------bar graph-------------#


 #bargraph of average attatiment score in yorkshire 2018 to 2019 vs 2021 vs 2022 accross
 school %>%
  filter(District %in% districtInYorkshire$District) %>% 
  group_by(Year, District) %>% 
  mutate(ads = mean(`Attainment 8 score`, na.rm = TRUE)) %>% 
  filter(row_number()==1) %>% 
  ggplot(aes(x = factor(District), y = ads, fill = factor(District))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Average Attainment 8 score In Yorkshire", x = "District", y = "Average Attainment 8 score") +
  facet_wrap(~Year, ncol = 2, scales = "free_x")+
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black", size = 8),
        axis.title = element_text(color = "black", size = 10),
        legend.position="none") 


 #bargraph of average attatiment score in oxfordhsire 2018 to 2019 vs 2021 vs 2022 accross
 school %>%
  filter(District %in% districtInOxfordine$District) %>% 
  group_by(Year, District) %>% 
  mutate(ads = mean(`Attainment 8 score`, na.rm = TRUE)) %>% 
  filter(row_number()==1) %>% 
  ggplot(aes(x = factor(District), y = ads, fill = factor(District))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Average Attainment 8 score In Oxfordshire", x = "District", y = "Average Attainment 8 score") +
  facet_wrap(~Year, ncol = 2, scales = "free_x")+
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black", size = 8),
        axis.title = element_text(color = "black", size = 10),
        legend.position="none") 
 
 #line graph
 
 school %>%
   filter(District %in% districtInYorkshire$District) %>% 
   group_by(Year) %>% 
   summarise(ads = mean(`Attainment 8 score`, na.rm = TRUE)) %>% 
   ggplot(aes(x = Year, y = ads,group=1)) +
   geom_line()+
   theme_minimal() + 
   theme(plot.title = element_text(hjust = 0.5),
         axis.text = element_text(color = "black", size = 8),
         axis.title = element_text(color = "black", size = 10),
         legend.position="none") +
   labs(title="Line graph of yorkshire",x="Year",y="Average attainmnet 8 score")
 
 school %>%
   filter(District %in% districtInOxfordine$District) %>% 
   group_by(Year) %>% 
   summarise(ads = mean(`Attainment 8 score`, na.rm = TRUE)) %>% 
   ggplot(aes(x = Year, y = ads,group=1)) +
   geom_line()+
   theme_minimal() + 
   theme(plot.title = element_text(hjust = 0.5),
         axis.text = element_text(color = "black", size = 8),
         axis.title = element_text(color = "black", size = 10),
         legend.position="none") +
   labs(title="Line graph of oxfordshire",x="Year",y="Average attainmnet 8 score")

