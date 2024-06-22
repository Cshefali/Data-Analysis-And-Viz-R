#solutions for common issues faced while creating graphs.
#Author- Shefali C.
#Start date- July 11, 2023

library(tidyverse)

#1. Create Bar graph with count of each group on Y-axis
#just don't state any value to Y-axis!

ggplot(data = diamonds, aes(x = cut))+
  geom_bar()

diamonds %>% 
  group_by(cut) %>% 
  summarize(total = n()) %>% 
  ggplot(aes(x = reorder(cut, -total), y = total))+
  geom_bar(stat = "identity", fill = "lightblue")+
  labs(title = "Number of samples per group",
       x = "cut type",
       y = "total count")+
  theme_classic()

#2. 