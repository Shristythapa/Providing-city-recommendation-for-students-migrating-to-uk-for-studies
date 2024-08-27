

#install.packages("tidyverse")
library(tidyverse)

#install.packages("RColorBrewer")
library(RColorBrewer)



options(scipen=1000)

internet<- read_csv("clean data\\internetData.csv")


glimpse(internet)

#boxplot


 #selecting 5 district from yorkshire randomly
 district_yorkshire=internet %>% 
  filter(Country=="Yorkshire") %>% 
  distinct(District)%>%
  sample_n(size = 5, replace = FALSE) %>% 
  select(District)

 #selecting 5 districts from oxfordshire randomly
 district_oxfordshire=internet %>% 
  filter(Country=="Oxforshire") %>% 
  distinct(District)%>%
  sample_n(size = 5, replace = FALSE) %>% 
  select(District)


 #Average download speed boxplot
 internet %>%
  filter(( District %in% district_yorkshire$District| District %in% district_oxfordshire$District)
         &!is.na(`Average download speed (Mbit/s)`)) %>%
  sample_n(size=900,replace = FALSE) %>% 
  ggplot(aes(x = District, y = `Average download speed (Mbit/s)`)) +
  geom_boxplot() +
  facet_wrap(~ Country, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))


 #barchart of average download speed of district in oxfordhsire
 internet %>%
  filter( District %in% district_oxfordshire$District & is.na(`Average download speed (Mbit/s)`)==FALSE) %>%
  group_by(District) %>% 
  mutate(ads=mean(`Average download speed (Mbit/s)`)) %>% 
  filter(row_number() == 1) %>%
  ggplot(aes(x = District, y = ads,fill=District)) +
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Paired")+
  labs(title = "Average Download speed Distribution In Oxfordshire",x="District in Oxfordshire", y= "Average Download Speed") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))


 #barchart of average download speed of district in yorkshire
 internet %>%
  filter( District %in% district_yorkshire$District & is.na(`Average download speed (Mbit/s)`)==FALSE) %>% 
  group_by(District) %>% 
  mutate(ads=mean(`Average download speed (Mbit/s)`)) %>% 
  filter(row_number() == 1) %>%
  ggplot(aes(x = District, y = ads,fill=District)) +
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Paired")+
  labs(title = "Average Download speed Distribution In Yorkshire",x="District in Yorkshire",y="Average download speed") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 10))


