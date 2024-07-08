#Exploratory Data Analysis on Palmer penguins dataset
library(palmerpenguins)
library(ggplot2)
library(tidyr)
library(tidyverse)
penguins_data <- data.frame(penguins)

#na.omit() removes all the rows with missing data.
clean_penguins_data <- na.omit(penguins_data)

#Basic theme for plot are and labels has been stored in plot_theme and labels_theme.
#These themes will apply to all the plots; hence stored in a variable as a template.

plot_theme <- theme(panel.background = element_rect(fill = "white"),
                        panel.border = element_rect(color = "black", fill = NA, size = 1),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank())

labels_theme <- theme(text = element_text(size = 10, family = "mono",
                         face = "bold"), plot.title = element_text(hjust = 0.5),
                         plot.caption = element_text(face = "italic"))

#This caption will be included in every plot.
caption_line <- "Data collected by Dr. Kristen Gormon from 2007-2009"

#Which species of penguins account for highest population from 2007-2009 (all islands combined)?
ggplot(data = clean_penguins_data,aes(x = species, fill =species)) + 
  geom_bar(width = 0.4) +
  geom_text(aes(label=..count..), vjust = 1.5,stat = "count", color = "black") +
  labs(title = "Count of Each Species in 3 Years", caption = caption_line, y = "Count of each species") +
  plot_theme +
  theme(legend.position = "none") + 
  labels_theme

#Do penguin species co-habitate?
#Which species accounts for highest population on each island?
ggplot(data = clean_penguins_data,aes(x = species, fill = species)) + 
  geom_bar() +
  geom_text(aes(label = ..count..),vjust = 1.5,stat = "count",color = "black")+
  labs(title = "Count of Each Species per Island", caption = caption_line) +
  facet_wrap(~island) + plot_theme +  labels_theme +
  theme(axis.text.x = element_text(angle = 90),legend.position = "none") 
 

#Male and female population of each species on all islands
#ggplot(data = clean_penguins_data, aes(x = species, fill = sex))+
#  geom_bar(width = 0.7) + 
#  #geom_text(aes(label=..count..), vjust = 1.5,stat = "count", color = "black") + 
#  labs(title="Population of Male and Female Penguins per Island", caption = caption_line, x = "penguin species", y = "total count")+
#  plot_theme +
#  theme(axis.text.x = element_text(angle = 90)) +
#  facet_wrap(~island) +
#  labels_theme

#population of male and female penguins per year
ggplot(data = clean_penguins_data,aes(x = species, fill = sex)) +
  geom_bar() + 
  labs(title = "Population of Male and Female Penguins per Year", caption=caption_line, x="penguin species", y="total count")+
  plot_theme + labels_theme +
  theme(axis.text.x = element_text(angle=90))+
  facet_wrap(~year)
 

#Do penguins with higher body mass have longer flippers? 
ggplot(data=clean_penguins_data, aes(x=body_mass_g, y=flipper_length_mm)) +
  geom_jitter(mapping = aes(color = species))+
  geom_smooth(color = "black")+
  labs(title="Body mass vs. flipper length", caption = caption_line, 
       x = "body mass(in grams)", y = "flipper length(in mm)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) + 
  labels_theme 
#storing the average body measurements of each species
penguins_stats <- clean_penguins_data %>% 
  group_by(species) %>% 
  drop_na() %>% 
  summarize(avg_bill_length = mean(bill_length_mm,2), avg_bill_depth = mean(bill_depth_mm,2),
            avg_flipper_length = mean(flipper_length_mm,2), avg_body_mass = mean(body_mass_g,2))

#Average bill length of each species
ggplot(data = penguins_stats,aes(x=species, y=avg_bill_length, fill=species)) +
  geom_bar(stat = "identity",width=0.3)+
  geom_text(aes(label=avg_bill_length), vjust = 1.5,stat = "identity", color = "black") +
  labs(title = "Average Bill Length(mm) of Each Species",caption = caption_line,
       y = "average bill length(mm)")+
  plot_theme + labels_theme +
  theme(legend.position = "none")

#Average bill depth of each species
ggplot(data = penguins_stats, aes(x=species,y=avg_bill_depth,fill=species))+
  geom_bar(stat = "identity",width = 0.3)+
  geom_text(aes(label=avg_bill_depth), vjust = 1.5,stat = "identity", color = "black") +
  labs(title = "Average Bill Depth(mm) of Each Species", caption = caption_line,
       y = "average bill depth(mm)")+
  plot_theme + labels_theme +
  theme(legend.position = "none")

#Average flipper length of each species
ggplot(data=penguins_stats, aes(x=species,y=avg_flipper_length,fill=species))+
  geom_bar(stat = "identity",width = 0.3)+
  geom_text(aes(label=avg_flipper_length), vjust = 1.5,stat = "identity", color = "black")+
  labs(title="Average Flipper Length(mm) of Each Species",caption = caption_line,
       y = "average flipper length(mm)")+
  plot_theme + labels_theme +
  theme(legend.position = "none")

#Average body mass of each species
ggplot(data = penguins_stats, aes(x=species,y=avg_body_mass,fill=species))+
  geom_bar(stat = "identity",width = 0.3)+
  geom_text(aes(label=avg_body_mass), vjust = 1.5,stat = "identity", color = "black")+
  labs(title = "Average Body Mass(grams) of Each Species",caption=caption_line,
       y = "average body mass(grams)")+
  plot_theme + labels_theme +
  theme(legend.position = "none")

#gender_count stores the male and female count of each species
#gender_count <- clean_penguins_data %>% 
#  group_by(species) %>% 
#  drop_na() %>% 
#  summarize(male = sum(sex == 'male'), female = sum(sex=='female'))

gender_count <- clean_penguins_data %>% 
  group_by(species,island) %>% 
  drop_na() %>% 
  summarize(male = sum(sex == 'male'), female = sum(sex=='female'))

#gender_count converted to long form in order to plot male and female
#population of each species side by side and not as a stacked bar chart.
gender_count_long_form <- pivot_longer(gender_count, cols = c(male,female),names_to = "gender",values_to = "total_count")

#Plotting male and female count of each species separately
ggplot(data = gender_count_long_form, aes(x=species,y=total_count,fill=gender))+
  geom_bar(stat = "identity",width = 0.4,position = "dodge")+
  labs(title="Male and Female Penguin Population per Species",caption = caption_line, x="penguin species", y="total count")+
  plot_theme + labels_theme

#Plotting male and female population per species per island
ggplot(data = gender_count_long_form, aes(x=species,y=total_count,fill=gender))+
  geom_bar(stat = "identity",width = 0.4,position = "dodge")+
  labs(title="Male and Female Penguin Population per Species",caption = caption_line, x="penguin species", y="total count")+
  plot_theme + labels_theme + facet_wrap(~island) +
  theme(axis.text.x = element_text(angle = 90))

plots_directory <- list.files(tempdir(),pattern = "rs-graphics", full.names = TRUE)
plots_png_path <- list.files(plots_directory, pattern = ".png", full.names = TRUE)
file.copy(from = plots_png_path, to = "C:/Users/shefa/Desktop/COURSERA/Programming Language R/PenguinsDataset")




