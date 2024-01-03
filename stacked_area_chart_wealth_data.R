#RGraphGallery- https://r-graph-gallery.com/web-stacked-area-chart-inline-labels.html
#Last Update- Jan 3, 2024
#Stacked Area Chart with clean labels

#load packages
library(tidyverse)
library(readxl)
library(showtext) #load custom fonts
library(ggstream) #used to smooth the area shapes
library(ggtext) 

##path to different directories
#working directory
working_dir <- getwd()
#excel file's path
data_dir <- paste0(working_dir, "/data/rgraphgallery_datasets/wealth_data.xlsx")
#path to store output graph
image_dir <- paste0(working_dir, "/images/")

#read data
df <- readxl::read_xlsx(data_dir)
#str(df)

#Basic Stacked Area Chart
df %>% 
  ggplot(aes(x=year, y=total_wealth, 
             fill=country, color=country, label = country))+
  geom_area()

#data subset for vertical lines
vline_data <- df %>% 
              filter(year %in% c(2000,2005,2010,2015,2021)) %>% 
              group_by(year) %>% 
              summarise(wealth_sum = sum(total_wealth))

#add label column with annotation text for points in the graph
#total wealth in Billion Dollars (USD)
vline_data$wealth_label <- paste0("$",
                                  prettyNum(vline_data$wealth_sum, big.mark = ",",
                                            scientific=F),"B")

title_text <- textgrob
#My Trials
df %>% 
  ggplot(aes(x=year, y=total_wealth))+
  geom_area(aes(fill=country,
                color = country))+
  geom_point(data = vline_data, aes(x=year, y=wealth_sum), color = "black")+
  #geom_vline(data = vline_data, aes(xintercept=year))+
  geom_segment(data = vline_data, aes(x=year, y=0,
                                      xend=year, yend=wealth_sum))+
  geom_text(data = vline_data, aes(x=year,y=wealth_sum,label=wealth_label),
            size=2.5, vjust=-1.5, fontface="bold")+
  #scale_x_discrete(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_minimal()+
  theme(
    #Axis Grids
    panel.grid = element_blank(),
    #Axis lines and labels
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.8),
    #axis.text.y.left = element_text(size = 2, face = "bold"),
    axis.title = element_blank(),
    
    #Plot title, subtitle and captions
    #plot.title = element_text(margin = margin(b=15))
    plot.margin = margin(t=20,b=5),
    legend.position = "none"
  )


##From the blog, Basic chart

#Color palette
pal=c("#003f5c",
      "#2f4b7c",
      "#665191",
      "#a05195",
      "#d45087",
      "#f95d6a",
      "#ff7c43",
      "#ffa600")

# Stacking order
order <- c("United States", "China", "Japan", "Germany", 
           "United Kingdom", "France", "India", "Other")

#BASIC CHART
df %>% 
  ggplot(aes(x=year, y=total_wealth,fill=country,
             color = country))+
  geom_stream(type = "ridge", bw=1)

