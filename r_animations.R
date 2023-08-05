#Creating animations in R
#Last Updated- August 5, 2023

#Source- https://www.r-bloggers.com/2021/05/animated-graph-gif-with-gganimate-ggplot/

#load libraries
library(tidyverse)
library(gganimate)
library(ggthemes)
library(gapminder)
library(gifski)

#str(gapminder)

##GRAPH 1- Gapminder; Bubble Chart Animation

graph1 <- gapminder %>% 
              ggplot(aes(x = gdpPercap, y = lifeExp, color = continent, size = pop))+
              geom_point(alpha = 0.7, stroke = 0)+
              theme_fivethirtyeight()+
              scale_size(range = c(2,12), guide = "none")+
              scale_x_log10()+
              
              labs(title = "Life Expectancy vs GDP",
                   x = "GDP",
                   y = "Life Expectancy",
                   caption = "Source: Gapminder",
                   #this controls the title of continent legend;
                   #since color aesthetic has been used for continents in ggplot() line
                   color = "Continent")+
              
              #axis.title = element_text() is required to display the x and y title here.
              theme(axis.title = element_text(),
                    
                    ##Font family gives warnings.
                    #text = element_text(family = "Rubik"),
                    
                    legend.text = element_text(size = 10))+
              scale_color_brewer(palette = "Set2")

#Adding animation
(graph1.animation <- graph1 + 
                      transition_time(time = year) +
                      labs(subtitle = "Year: {frame_time}")+
                      shadow_wake(wake_length = 0.1))

#Saving the animation
animate(graph1.animation, height = 500, width = 800, fps = 30,
        duration = 10, end_pause = 60, res = 100)

gganimate::anim_save("gapminder_graph.gif")

##GRAPH 2- Video Game Sales Line Chart animation
video_games <- read_csv(file = paste0(getwd(),"/data/video_game_sales/vgsales.csv"))

#summarize the data
game_data <- video_games %>% 
                filter(Platform == "PS3",
                       Genre %in% c("Action", "Shooter", "Sports", "Racing", "Simulation")) %>% 
                drop_na() %>% 
                group_by(Year, Genre) %>% 
                summarize(sales = sum(Global_Sales, na.rm = T))

#base plot
game_data %>% 
  ggplot(aes(x = Year, y = sales, color = Genre))+
  geom_line(linewidth = 2, alpha = 0.75)+
  theme_solarized_2(light = F)+
  labs(title = "Video Game Sales",
       y = "Global Sales")+
  theme(text = element_text(family = "DM Sans Medium", colour = "#EEEEEE"),
        title = element_text(color = "#EEEEEE"),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#111111"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Pastel1") +
  geom_point() +
  scale_x_continuous(breaks = 0:2100)


graph2<- game_data %>%
  ggplot(aes(x=Year, y=sales, color=Genre)) +
  geom_line(linewidth = 2, alpha = 0.75) +
  theme_solarized_2(light = FALSE) +
  labs(title = "Video Game Sales",
       y = "Global Sales") +
  theme(text = element_text(family = "DM Sans Medium", colour = "#EEEEEE"),
        title = element_text(color = "#EEEEEE"),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#111111"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Pastel1") +
  geom_point() +
  scale_x_continuous(breaks = 0:2100)