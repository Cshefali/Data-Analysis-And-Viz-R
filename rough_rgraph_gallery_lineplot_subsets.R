##Rough Work to understand how plot changes if a function or argument is used/omitted

#This script only contains the "Main Line Chart" section of main script.


##Keeping direct_label True in gghighlight()
df2 %>% 
  ggplot() +
  geom_hline(yintercept = 100, linetype = "solid", linewidth= 0.25)+
  geom_point(data = highest_values_data, 
             aes(x = date, y = value, color = country), shape = 16)+
  geom_line(aes(x = date, y = value, color = country))+
  gghighlight(#use_direct_label = F,
              unhighlighted_params = list(colour = alpha(colour = "grey85", alpha = 1)))+

  #this will add rounded values at the end of each line
  geom_text(data = highest_values_data,
            aes(x= date, y = value, color = country, label = round(value)),
            hjust = -0.5, vjust = -0.5, size = 2.5, family = font, fontface = "bold")+
  #Using color palette from metBrewer
  scale_color_met_d(name = "Redon")+
  scale_x_date(date_labels = "%y")+
  #create labels but only display 100 on Y-axis
  scale_y_continuous(breaks = c(90,95,100,105,110),
                     labels = c("","","100","",""))+
  
  #create subsets with each country
  facet_wrap(~country)

##Not using gghighlight() function at all
df2 %>% 
  ggplot() +
  geom_hline(yintercept = 100, linetype = "solid", linewidth= 0.25)+
  geom_point(data = highest_values_data, 
             aes(x = date, y = value, color = country), shape = 16)+
  geom_line(aes(x = date, y = value, color = country))+
  # gghighlight(use_direct_label = F,
  #   unhighlighted_params = list(colour = alpha(colour = "grey85", alpha = 1)))+
  
  #this will add rounded values at the end of each line
  geom_text(data = highest_values_data,
            aes(x= date, y = value, color = country, label = round(value)),
            hjust = -0.5, vjust = -0.5, size = 2.5, family = font, fontface = "bold")+
  #Using color palette from metBrewer
  scale_color_met_d(name = "Redon")+
  scale_x_date(date_labels = "%y")+
  #create labels but only display 100 on Y-axis
  scale_y_continuous(breaks = c(90,95,100,105,110),
                     labels = c("","","100","",""))+
  
  #create subsets with each country
  facet_wrap(~country)