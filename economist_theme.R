#Trying out Economist theme plots from RGraphGallery
#Author- Shefali C.
#Last Updated- August 12, 2023

#Source- https://r-graph-gallery.com/web-lineplots-and-area-chart-the-economist.html

library(tidyverse)
library(shadowtext)
library(grid)
library(ggtext)
library(ggnewscale)
library(patchwork)
library(showtext)

#Defining some colors
BROWN <- "#AD8C97"
BROWN_DARKER <- "#7d3a46"
GREEN <- "#2FC1D3"
BLUE <- "#076FA1"
GREY <- "#C7C9CB"
GREY_DARKER <- "#5C5B5D"
RED <- "#E3120B"

#load the Economist font---Econ Sans not found! using another font

# font <- "Econ Sans Cnd"
# font_add_google(family = font, name = font, db_cache = T)
# font_path <- systemfonts::font_info(family = "Econ Sans Cnd")[["path"]]
# font_add(family = "Econ Sans Cnd", regular = font_path)

font <- "Gudea"
font_add_google(family = font, name = font, db_cache = T)
fa_path <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fa_path)

font <- "Gudea"
font_add_google(family = font, name = font, db_cache = T)
fa_path <- systemfonts::font_info(family = font)[["path"]]
font_add(family = font, regular = fa_path)


#Data Preparation


##DATA FOR LINE CHART

#define the 3 regions
regions <- c(
  "Sub-Saharan Africa", 
  "Asia and the Pacific", 
  "Latin America and the Caribbean"
)

#main dataframe
line_data <- data.frame(
  year = rep(c(2008, 2012, 2016, 2020), 3),
  percent = c(25.5, 21, 22.2, 24, 13.5, 9.5, 7.5, 5.5,10, 9, 7.5, 5.8),
  region = factor(rep(regions, each = 4), levels = regions)
)

#defining labels that will be used to annotate each line
line_labels<- data.frame(
  labels = c("Sub-Saharan Africa", "Asia and the Pacific", "Latin America and\nthe Caribbean"),
  x = c(2007.9, 2010, 2007.9),
  y = c(27, 13, 5.8),
  color = c(BLUE, GREEN, BROWN_DARKER)
)

##DATA FOR STACKED AREA CHART

regions <- c(
  "Sub-Saharan Africa", 
  "Asia and the Pacific", 
  "Latin America and the Caribbean", 
  "Rest of world"
)

#main dataframe
stacked_data <- data.frame(
  year = rep(c(2008, 2012, 2016, 2020), 4),
  percent = c(65, 55, 67, 85, 130, 85, 65, 50, 10, 10, 10, 8, 60, 20, 10, 16),
  region = factor(rep(regions, each = 4), levels = rev(regions))
)

#labels that will be used to annotate each area part.
stacked_labels <- data.frame(
  labels = c(
    "Sub-Saharan Africa", 
    "Asia and the Pacific", 
    "Latin America and\nthe Caribbean",
    "Rest\nof world"
  ),
  x = c(2014, 2014, 2014, 2008.1),
  y = c(25, 100, 225, 225),
  color = c("white", "white", BROWN_DARKER, GREY_DARKER)
)

#Basic line chart
(line_chart <- ggplot(data = line_data, aes(x = year, y = percent))+
                  geom_line(aes(color = region), linewidth = 2.4)+
                  geom_point(aes(fill = region),
                             color = "white",
                             size = 5,
                             #point-type pch = 21 shows both color and fill aesthetic.
                             pch = 21,
                             #stroke defines the width of points border
                             stroke = 1)+
                  scale_color_manual(values = c(BLUE, GREEN, BROWN_DARKER))+
                  scale_fill_manual(values = c(BLUE, GREEN, BROWN_DARKER))+
                  #omit the legend
                  theme(legend.position = "none"))

##CUSTOMIZATION OF THE PLOT
(line_chart <- line_chart +
                scale_x_continuous(
                  limits = c(2007.5, 2021.5),
                  breaks = c(2008, 2012, 2016, 2020),
                  labels = c("2008", "12", "16", "20"),
                  expand = c(0,0)
                )+
                #customize Y axis
                scale_y_continuous(
                  limits = c(0,32),
                  breaks = seq(0, 30, by = 5),
                  expand = c(0,0)
                )+
                #setting plot background, grids
                theme(
                  panel.background = element_rect(fill = "white"),
                  #remove the grid lines
                  panel.grid = element_blank(),
                  #add vertical grids with desired color
                  panel.grid.major.y = element_line(color = "#A8BAC4", linewidth = 0.3),
                  #remove ticks from Y-axis
                  axis.ticks.length.y = unit(0, "mm"),
                  #keep tickmarks on x-axis
                  axis.ticks.length.x = unit(2, "mm"),
                  #remove title from both axes
                  axis.title = element_blank(),
                  #make the bottom line of X-axis black
                  axis.line.x.bottom = element_line(color = "black"),
                  #remove labels from Y-axis
                  axis.text.y = element_blank(),
                  #change the font and size of X-axis labels
                  axis.text.x = element_text(size = 14)
                )
)

#Adding annotations
(line_chart <- line_chart +
                new_scale_color()+
                geom_text()
)