#Statistical Analysis of diamonds dataset
#Last Update- February 18, 2024
#Author- Shefali C.

library(tidyverse)

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

#total number of data points for each type of cut

df1 %>% 
  dplyr::group_by(cut) %>% 
  dplyr::summarise(total_count = n()) %>% 
  ggplot(aes(x=cut, y = total_count))+
  geom_bar(stat = "identity",fill = "#d45087", color = "black")+
  #remove space between X-axis and base of the bars
  scale_y_continuous(expand = c(2,0))+
  labs(title = "Expand = c(2,0)",
       subtitle = "1st agrument is multiplicative vector\nadds double space both at top and bottom")+
  theme_bw()

df1 %>% 
  dplyr::group_by(cut) %>% 
  dplyr::summarise(total_count = n()) %>% 
  ggplot(aes(x=cut, y = total_count))+
  geom_bar(stat = "identity",fill = "#d45087", color = "black")+
  #remove space between X-axis and base of the bars
  scale_y_continuous(expand = c(0,115))+ 
                     #limits = c(0,25000))+
  # labs(title = "Expand = c(2,0)",
  #      subtitle = "1st agrument is multiplicative vector\nadds double space both at top and bottom")+
  theme_bw()

df1 %>% 
  dplyr::group_by(cut) %>% 
  dplyr::summarise(total_count = n()) %>% 
  ggplot(aes(x=cut, y=total_count))+
  geom_bar(stat = "identity", fill= "#d45087", color = "black")+
  scale_y_continuous(expand = expansion(mult = c(0,0.05)))+
  #adding space to the left and right of x-axis
  scale_x_discrete(expand = expansion(mult = c(0.2,0.2)))+
  theme_bw()
