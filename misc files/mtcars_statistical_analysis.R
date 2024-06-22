#Statistical Analysis of mtcars dataset
#Last Update- Feb 5, 2024
#Author- Shefali C.

library(tidyverse)
#to use textGrob() in a plot below
library(grid)

df1 <- mtcars
View(df1)

#histogram of horsepower
#using binwidth=40 and range 0-360
df1 %>% 
  ggplot(aes(x=hp))+
  geom_histogram(fill = "darkred", color = "black",
                 breaks = seq(0,360,40))+
  scale_y_continuous(expand = c(0,0), limits = c(0,15),
                     breaks = seq(0,15,3),
                     labels = seq(0,15,3))+
  scale_x_continuous(breaks = seq(0,360,40),
                     labels = seq(0,360,40))+
  theme_bw()

#using binwidth = 50 and range 0-350

df1 %>% 
  ggplot(aes(x=hp))+
  geom_histogram(fill = "darkred", color = "black",
                 breaks = seq(0,350,50))+
  scale_y_continuous(expand = c(0,0), limits = c(0,15),
                     breaks = seq(0,15,3),
                     labels = seq(0,15,3))+
  scale_x_continuous(breaks = seq(0,350,50),
                     labels = seq(0,350,50))+
  theme_bw()

hp_mean <- round(mean(df1$hp),2)

#Creating density plot
#adding "mean" label below vline on x axis- method 1
#Using "tags": tags are independent of X-Y scales. 
#If x=146 will be used to place label below the vline, it won't work. 
#It should be in the range of 0 to 1, as is the case with captions.
df1 %>% 
  ggplot(aes(x=hp))+
  geom_density(color = "black", fill = "lightblue")+
  #add a vertical line marking mean of horsepower
  geom_vline(aes(xintercept = mean(hp)), color = "red", linewidth=0.5,
             linetype = "dashed")+
  #add the mean value at x axis
  # annotate(geom = "text", x = floor(hp_mean), y=0,
  #          label = "shef")+
  scale_y_continuous(expand = c(0,0))+
  labs(tag = "mean")+
  theme_bw()+
  theme(
    plot.tag.position = c(0.3, 0.001)
  )

#--method 2
#text gets placed properly because of using clip="off" and then
#vjust = 1 to place the text below x axis.
df1 %>% 
  ggplot(aes(x=hp))+
  geom_density(color = "midnightblue", fill = "lightblue")+
  geom_vline(aes(xintercept = round(mean(hp),2)), linetype = "dashed",
             linewidth = 0.4, color = "red")+
  annotate(geom = "text", x = hp_mean, y = 0.000, color = "black",
           label = "shefali", vjust = 1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(clip = "off")+
  theme_bw()

#method 3--using textGrob() and annotation_custom()

vline_label <- grid::textGrob(label = "horsepower mean",
                              gp = gpar(fontsize = 6, 
                                        fontface = "bold",
                                        col = "red"),
                              vjust = 1)
#plot
df1 %>% 
  ggplot(aes(x=hp))+
  geom_density(color = "midnightblue", fill = "lightblue")+
  geom_vline(aes(xintercept = round(mean(hp),2)), linetype = "dashed",
             linewidth = 0.4, color = "red")+
  annotation_custom(vline_label, xmin = hp_mean, xmax = hp_mean,
                    ymin = 0.000, ymax = 0.000)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(clip = "off")+
  theme_bw()
  
#annotating along the line
vline_label2 <- grid::textGrob(label = "horsepower mean",
                               gp = gpar(col = "red",
                                         fontsize = 6,
                                         fontface = "bold"))

#plot
df1 %>% 
  ggplot(aes(x=hp))+
  geom_density(color = "midnightblue", fill = "lightblue")+
  geom_vline(aes(xintercept = round(mean(hp),2)), linetype = "dashed",
             linewidth = 0.5, color = "red")+
  annotation_custom(grob = vline_label2,
                    xmin = hp_mean, xmax = hp_mean,
                    ymin = 0.0015, ymax = 0.0025)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(clip = "off")+
  theme_bw()
###-----------USAGE OF ANNOTATE()-------------------
(p1 <- ggplot(data = df1, aes(x=wt, y=mpg))+
        geom_point()+
        theme_bw())

#add a rectangle
p1 + annotate("rect", xmin = 3, xmax = 4.2, ymin = 12, ymax = 21,
           alpha=0.7, fill = "pink")

#add a slant line segment (at an angle; not straight)
p1 + annotate("segment", x = 2.5, 
              xend = 4,
              y = 15, 
              yend = 25,
              colour = "blue")

#add a line segment with a point in between
p1 + annotate(geom = "pointrange",
              x = 3.5,
              #location of the point
              y = 20,
              #ymin and ymax is the starting and ending point of the line
              ymin = 12,
              ymax = 28,
              colour = "red",
              #size of the point
              size = 2.5,
              linewidth = 1.5)

