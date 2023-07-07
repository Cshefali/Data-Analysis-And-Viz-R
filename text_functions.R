#Collection of all functions used in R to add text annotations to plots
#Author- Shefali C.
#Date- July 7, 2023

#Sources:
#Stdha.com

library(ggplot2)
library(dplyr)
library(grid)

#create dataframe
df <- data.frame(x = 1:3, y = 1:3, 
                 name = c("Text1", "Text with \n 2 lines", "Text3"))

head(df)

##METHOD 1- using geom_text()

#whole thing in () saves + displays the plot.

(sp <- ggplot(data = df, aes(x,y,label = name))+
        geom_point()+
        xlim(0,3.5)+
        ylim(0,3.5)+
        #fontface- 2(bold), 3(italic), 4(bold.italic)
        geom_text(size = 6, hjust = 0, vjust = 0, fontface = 2))

#Change text color and size by groups
(sp2 <- ggplot(data = mtcars, aes(x = wt, y = mpg, label = rownames(mtcars)))+
          geom_point()+
          #set color of text based on number of cylinders- 4, 6 or 8
          #set size of text by a continuous variable- here, weight wt.
          geom_text(aes(col = factor(cyl), size = wt), fontface = "bold")+
          #define the limits of min and max text size.
          scale_size(range = c(1,3))+
          theme_bw())


##ADDING TEXT ANNOTATION AT PARTICULAR COORDINATES

#METHOD 2- using annotate() with geom_text()

#using geom_Text()
sp2+
  geom_text(x = 3, y = 30, label = "Scatter plot")

#same thing using annotate()
sp2+
  annotate(geom = "text", x = 3, y = 30, 
           label = "Scatter plot", colour = "hotpink")+
  theme(plot.margin = margin(t=15,r=1,b=40,l=1))

#METHOD 3- annotation_custom() & textGrob()

#Both these functions are used to add static annotation in top left/right etc.
#Static annotations remain at same coordinate in every panel of facet.

#create a text
grob <- grobTree(textGrob("Scatter plot", x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
sp2 + annotation_custom(grob)

#demo of grob text position in every facet. Same position.
sp2 + annotation_custom(grob)+facet_wrap(~cyl, scales="free")
  