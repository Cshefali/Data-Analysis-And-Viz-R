##Analysis of Indian Crops Production, 2010-2017
#Last Update- April 13, 2024
#Author- Shefali C.

library(tidyverse)
library(ggthemes)
library(janitor)

#saves the path to the desired zip folder
#zip1 <- file.choose()
#unzip(zip1)

#read csv file
crop_data <- readr::read_csv("crops_data.csv")

#convert all colnames to lowercase, remove special characters
crop_data <- janitor::clean_names(crop_data)

#make a copy of the data
crop_data_copy <- crop_data

#The crops mentioned below only have "Area" column.
##Hence, dividing the data into 2 groups to 
#prevent NA values in analysis steps ahead.

major_crops <- crop_data_copy %>% 
                select(
                  -c("fruits_area_1000_ha", 
                    "vegetables_area_1000_ha",
                    "fruits_and_vegetables_area_1000_ha",
                    "potatoes_area_1000_ha",
                    "onion_area_1000_ha",
                    "fodder_area_1000_ha")
                )

minor_crops <- crop_data_copy %>% 
                select(
                  c("fruits_area_1000_ha", 
                    "vegetables_area_1000_ha",
                    "fruits_and_vegetables_area_1000_ha",
                    "potatoes_area_1000_ha",
                    "onion_area_1000_ha",
                    "fodder_area_1000_ha")
                )

#Convert data to long form
major_crops_long <- major_crops %>% 
              pivot_longer(cols = 6:74,
                           names_to = "original_name",
                           values_to = "quantity")

#Create a column 'crop'; extracting crop name from 'original_name'
major_crops_long <- major_crops_long %>% 
              mutate(crop = stringr::str_extract(original_name,
                                         pattern = "^.*(?=_area|_production|_yield)")
              )

#extract the type of quantitiy- area, yield or prod. from the same col
major_crops_long <- major_crops_long %>% 
              mutate(
                stat = stringr::str_extract(original_name,
                                            pattern = "area|production|yield")
              ) %>% 
              select(-original_name)

#widen the dataframe to have individual cols for each stat
major_crops_wide <- major_crops_long %>% 
              pivot_wider(names_from = "stat",
                          values_from = "quantity")

#Selecting only relevant column

major_crops_final <- major_crops_wide %>% 
                select(state_name, dist_name, year, crop, area,
                       production, yield)

##--------------------NATION-WIDE ANALYSIS---------------------

#trend in total production; 

plot1_data <- major_crops_final %>% 
                group_by(year) %>% 
                summarise(total_prod = sum(production)) %>%
                mutate(total_prod = total_prod/1000)

#adding y to nudge the labels up/down 
plot1_data$y <- with(plot1_data,
                             ifelse(
                               #for 2010, 2012, 2015->shift labels down
                               year %in% c(2010,2012,2015),-1,
                                    #for 2014, shift label to right.
                                    ifelse(year == 2014,0,
                                           ifelse(year == 2016,-0.5,0))))

#adding x to nudge labels right/left
plot1_data$x <- with(plot1_data,
                     ifelse(year %in% c(2014,2016), 0.2,0)
                    )


#plot
plot1_data %>% 
  ggplot(aes(x=year, y = total_prod))+
  geom_line()+
  geom_text(aes(label = round(total_prod,2)), size = 2, fontface = "bold",
            check_overlap = T,
            position = position_nudge(x = plot1_data$x,
                                      y = plot1_data$y))+
  scale_x_continuous(breaks = seq(2009, 2017, 1),
                     labels = seq(2009, 2017, 1))+
  scale_y_continuous(limits = c(300, 400),
                     breaks = seq(300,400,10),
                     labels = seq(300,400,10))+
  labs(title = "Trend in total crop production, 2010-2017",
       x = "",
       y = "Total production (in million tonnes)")+
  
  theme_bw()
  
##---------------------------STATE-WISE ANALYSIS----------------------------

##BIHAR

bihar_data <- major_crops_final %>% filter(state_name == "Bihar")

#all unique districts
unique(bihar_data$dist_name)

#Replacing "Shahabad (now part of Bhojpur district)" with "" in Shahabad dist.
bihar_data$dist_name <- str_replace_all(string = bihar_data$dist_name,
                                        pattern = "(?<=Shahabad).*$",
                                        replacement = "")

#checking rows with NA values--> Production & Yield because 
#cols like frutis, veggies do not have these 2 cols in original dataframe.
#Only Area col present for fruits, veggies, potatoes etc.

rbind(colSums(is.na(bihar_data)))

##Which crops have had largest production between 2010-2017 in Bihar?

highest_production <- bihar_data %>% 
                        select(year, crop, production) %>% 
                        group_by(year, crop) %>% 
                        summarize(total_production = sum(production)) %>% 
                        arrange(year, -total_production)

##Filter out crops with no production-- 0.00
zero_prod_bihar_crops <- highest_production[highest_production$total_production==0,]
unique(zero_prod_bihar_crops$crop)

#Find out all crops which had 0 production in all years 2010-2017

highest_production %>% 
  filter(total_production == 0) %>% 
  ggplot(aes(x = year, y = total_production))+
  geom_bar(stat = "identity")+
  theme_bw()+
  facet_wrap(~crop, scales = "free_x")