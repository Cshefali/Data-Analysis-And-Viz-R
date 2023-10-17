#Usage of Patchwork package
#References- 
#1. https://patchwork.data-imaginist.com/
#2. https://patchwork.data-imaginist.com/articles/guides/assembly.html
#3. https://patchwork.data-imaginist.com/articles/guides/layout.html
#Last Update- Oct 17, 2023

library(tidyverse)
library(patchwork)
library(ggthemes)

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

##DIFFERENCE BETWEEN '+' AND '|'

#'+'- patchwork assembles the plots in best way possible, trying to fit all in a 
#square figure.
p1 + p2 + p3 + p4

#'|' is specific about placing all plots side by side only.
p1 | p2 | p3 | p4

##Save a plot assembly and add another one later on

patch1 <- p1 + p2

p3 + patch1

#Adding objects other than ggplot objects like grobs
#here, textgrob

##The first element to patchwork operators '+', '/' etc. must be 
##a ggplot object. If that's not the case, NULL is returned.
##In order to keep other objects as first one like grobs, base R plot etc.
##enclose that object inside wrap_elements().

#add text to the right
p1 + grid::textGrob(label = '+ adds text horizontally')
#adding text to the left-- this will return NULL
grid::textGrob(label = "To add text on LHS,\nenclose the textgrob inside wrap_elements()") + p1

#add text to LHS, this will work
patchwork::wrap_elements(grid::textGrob(label = "Add text to LHS using\nwrap_elements()")) + p1

#add text at bottom
patch1 / grid::textGrob(label = " '/' adds texts at bottom or top")

#adding text at top
wrap_elements(grid::textGrob(label = "Text 1")) / p1



#add a more complex grob like a table to the plot
p1+ gridExtra::tableGrob(mtcars[1:5, c('mpg', 'disp')])

#below, converts rownames to a separate column in the table itself.
#p1 + gridExtra::tableGrob(tibble::rownames_to_column(mtcars[1:5, c('mpg', 'disp')], 'carname'))

#add a base R plot alongside ggplot
#experiment with par() settings to get perfect alighment
(p1 + ~plot(mtcars$mpg, mtcars$disp, main = "Plot 2"))+
  plot_annotation(title = "ggplot object with base R plot",
    subtitle = "Note: That tilde '~' is crucial to combining base R plot with ggplot object")

#try altering margins, background color etc. to align base plot with ggplot graph
old_par <- par(mar = c(0, 2, 0, 0), bg = NA)
p1 + wrap_elements(panel = ~plot(mtcars$mpg, mtcars$disp, main = "Plot 2"), clip = FALSE)
par(old_par)

#In case, plots are being automatically generated through a loop or something
#Combining plots manually is hectic, use wrap_plots() for this

wrap_plots(p1 + p2 + p3 + p4)

##'Nesting the left-hand side'.
##When a new plot is added from LHS, this plot gets half of the entire plot area
##And others get small shares of the other half.
##But if plot is added on RHS, all plots get equal area.

patch2 <- p1 + p2

p3 + patch2 #plot 1 and 2 get nested, meaning they get shared in 50% of space while p3 gets entire 50%
patch2 + p3 #here, all three get equal area share.
patch2 - p3 #hyphen is used in nesting cases in LHS.
#to understand better, LHS nested plots

#the jitter plot gets added to last active plot, which is p2 here.
p3 + p2 + geom_jitter(data = mtcars, aes(x = mtcars$gear, y= mtcars$disp))

#Accessing individual plots in a patchwork object
(patch3 <- p1 + p2 + p4)

patch3[[2]] <- patch3[[2]] + theme_economist()

#see patch3 now
patch3

#Applying themes to ALL or ONLY CURRENT NESTING LEVEL PLOTS
(patch3 <- p1 + p2 + p4)
#Apply a theme to all plots using '&'
patch3 & ggthemes::theme_fivethirtyeight()

#Nested plot-- here plot 1,2 and 4 are nested ones.
p3 + patch3

#Economist theme gets applied only to the last nested plot, i.e. p3
(p3 / patch3) * theme_economist()

############################
##CONTROLLING LAYOUTS
#https://patchwork.data-imaginist.com/articles/guides/layout.html

#Adding space between plots
p1 + plot_spacer() + p2 + plot_spacer() + p3 + plot_spacer()

p1 + p2 + p3 + p4 +
  plot_layout(widths = c(2,1))

p1 + p2 + p3 + p4 +
  plot_layout(widths = c(1,2))

p1 + p2 + p3 + p4 + 
  plot_layout(#width of 1st column is twice the width of plots in 2nd col
              widths = c(2,1),
              #height of plots in 1st row is thrice the height of plots in 2nd row
              heights = unit(c(3,1), c("cm", "cm"))
              )

