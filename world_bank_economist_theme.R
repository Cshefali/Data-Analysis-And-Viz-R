##The Economist theme plot- Line plot and stacked area chart
#Author0 Shefali C.
#Last updated- August 12, 2023

#Source- https://r-graph-gallery.com/web-lineplots-and-area-chart-the-economist.html

library(tidyverse)
library(showtext)
library(patchwork)
library(lubridate)
library(ggthemes)
library(ggtext)
library(gghighlight)

#data
wb_data <- read_csv(paste0(getwd(),"/data/world_bank_data/world_bank_development_indicators.csv"))

