#Working with fonts in R
#Author- Shefali C.
#Date- July 9, 2023

#Help taken from:
#https://stackoverflow.com/questions/52251290/add-font-to-r-that-is-not-in-extrafonts-library
#https://cran.r-project.org/web/packages/extrafont/extrafont.pdf

##Link Below has great explanation. Read it.
#https://gradientdescending.com/adding-custom-fonts-to-ggplot-in-r/

setwd("C:/Users/shefa/Desktop/Github/Data-Visualization-R")

#loading this library for fonts
library(extrafont)
library(ggplot2)

#better font package.
library(showtext)



#returns list of all registrered fonts
fonts()

#this functions loads the desired font, if specified, from the system directory
#to the database of extrafont --> 'extrafontdb'

#NOTE: only needs to run once.
font_import(pattern = "verdana")

#this function loads the font to R, here, only for windows device.
#NOTE- this needs to be done, one per session.
loadfonts(device = "win")

#using the loaded font.
ggplot(data = mtcars, aes(x=wt, y=mpg))+
  geom_point(aes(col = cyl))+
  labs(title = "Car Performance")+
  theme(plot.title = element_text(family = "Verdana"))+
  theme_classic()



##--------SHOWTEXT PACKAGE----------

#can be easily used to get fonts from anywhere on internet.

#fetch fonts available on Google fonts
font_add_google(name = "Amatic SC", family = "amatic-sc")

#run this command before creating plot. Helps R understand the font
showtext_auto(enable = T)

ggplot(data = mtcars, aes(x=wt, y=mpg))+
  geom_point(aes(col = cyl))+
  labs(title = "Car Performance")+
  theme(plot.title = element_text(family = "Amatic SC"))+
  theme_classic()

ggsave(filename = paste0(getwd(),"/font_sacramento.png"))