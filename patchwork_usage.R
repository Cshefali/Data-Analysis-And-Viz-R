#Usage of Patchwork package
#References- 
#1. https://patchwork.data-imaginist.com/
#2. https://patchwork.data-imaginist.com/articles/guides/assembly.html
#Last Update- Oct 16, 2023

library(tidyverse)
library(patchwork)

##NOTE-
#ggplotify package functions can be used to convert different graphics to grobs
#and then add to plots

#scatterplot for mileage per gallon vs displacement
(p1 <- ggplot(mtcars)+ geom_point(aes(x = mpg, y = disp))+ggtitle("Plot 1"))
#boxplot for displacement range for each gear
(p2 <- ggplot(mtcars) + 
        geom_boxplot(aes(x = gear, y = disp, group = gear))+
        ggtitle("Plot 2"))

#third plot
(p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec)) + ggtitle("Plot 3"))

#4th plot
(p4 <- ggplot(mtcars) + geom_bar(aes(carb)) + ggtitle("Plot 4"))

#Join 2 plots into 1 figure
p1 + p2

#Join all 4 graphs- method 1
(p1 | p2 | p3 )/p4

#Join all 4 graphs- same as above, but using '+'
(p1 + p2 + p3)/p4

#adding labels using labs() adds labels to last plot because
#the last plot added is the active one
p1 + p2 + labs(subtitle = "Added to the last active plot,\nhere p2")

#patchwork, by default, keeps the figure square and adds plot row-wise
p1 + p2 + p3 + p4

#if byrow is set to False, graphs get added column-wise
p1 + p2 + p3 + p4 + plot_layout(byrow = F)

#now if labs() used, subtitle will be added to 4th plot (p4), because it 
#is the last one to be added to the grid
p1 + p2 + p3 + p4 + 
  plot_layout(byrow = F) + 
  labs(subtitle = "added to the last active plot,\nhere p4")

#Add plots in 3 rows and COLUMN-WISE
p1 + p2 + p3 + p4 + 
  plot_layout(nrow = 3, byrow = F)

# '/' stacks plots on top of each other
p1 / p2

# '|' adds plots side-by-side
p1 | p2

#use both '|' and '/'
p1 | (p2/p3)

#Add a common title to the combined graph
(p1 | (p2/p3)) + 
  plot_annotation(title = "All about mtcars", 
                  subtitle = "Make sure to enclose the plot assembly inside () before adding plot_annotation()")

#Numbering the plots
(p1 + p2 + p3) + 
  plot_annotation(tag_levels = "1")

##Save a plot assembly and add another one later on

patch1 <- p1 + p2

p3 + patch1

#Adding objects other than ggplot objects like grobs
#here, textgrob

#add text right or left
patch1 + grid::textGrob(label = '+ adds text horizontally')
#add text at top or bottom
patch1 / grid::textGrob(label = " '/' adds texts at bottom or top")

#add a more complex grob like a table to the plot
p1+ gridExtra::tableGrob(mtcars[1:5, c('mpg', 'disp')])

#below, converts rownames to a separate column in the table itself.
#p1 + gridExtra::tableGrob(tibble::rownames_to_column(mtcars[1:5, c('mpg', 'disp')], 'carname'))

#add a base R plot alongside ggplot
(p1 + ~plot(mtcars$mpg, mtcars$disp, main = "Plot 2"))+
  plot_annotation(title = "ggplot object with base R plot",
    subtitle = "Note: That tilde '~' is crucial to combining base R plot with ggplot object")

