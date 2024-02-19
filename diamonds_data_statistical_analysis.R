#Statistical Analysis of diamonds dataset
#Last Update- February 19, 2024
#Author- Shefali C.

##HELPFUL LINKS
#usage of stat_summary- mean/sd lines-->https://www.datanovia.com/en/lessons/ggplot-dot-plot/

library(tidyverse)
#to use stat_summary functionalities
library(Hmisc)

#working directory
working_dir <- getwd()
#directory to save plots to
img_dir <- paste0(working_dir, "/images/")

#dataset
df1 <- ggplot2::diamonds

#quick look
glimpse(df1)

#dataframe full view
View(df1)

#all column names
all_features <- colnames(df1)

#names of all numeric features
numeric_features <- colnames(df1 %>% select(where(is.numeric)))
#non-numeric features
non_numeric_features <- all_features[!(all_features %in% numeric_features)]

#Color palette
pal=c("#003f5c",
      "#2f4b7c",
      "#665191",
      "#a05195",
      "#d45087",
      "#f95d6a",
      "#ff7c43",
      "#ffa600")

##CATEGORICAL FEATURES

#1. cut
unique(df1$cut)

#Bar Chart- total number of data points for each type of cut

df1 %>% 
  group_by(cut) %>% 
  summarize(total_count = n()) %>% 
  ggplot(aes(x=cut, y = total_count))+
  geom_bar(stat = "identity",fill = "#d45087", color = "black")+
  geom_text(aes(label = total_count), fontface = "bold", vjust = -0.6,
            size = 3)+
  scale_y_continuous(expand = expansion(mult = c(0,.05)))+
  labs(title = "Number of diamonds per cut-type",
       subtitle = "Diamonds of ideal type have highest count",
       x = "", y = "total count of diamonds")+
  theme_bw()+
  theme(axis.text = element_text(color = "black", size = 8),
        axis.title = element_text(size = 9, color = "black"),
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())

#min and max value of carat for each cut
df1 %>% group_by(cut) %>% 
  summarize(min_value = min(carat),
         max_value = max(carat))

#Boxplot- to understand spread of each group
df1 %>% 
  ggplot(aes(x=cut, y=carat, fill = cut))+
  stat_boxplot(geom = "errorbar", width = 0.3)+
  geom_boxplot(color = "black", outlier.color = "red",
               outlier.size = 1)+
  #add a mean/sd range line
  # stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1),
  #              geom = "crossbar", color = "orange", linewidth = 0.2)+
  coord_flip()+
  labs(title = "Carat distribution is heavily right-skewed",
       subtitle = "The median carat value lies close to 1",
       x = "type of cut",
       y = "carats")+
  theme_bw()

