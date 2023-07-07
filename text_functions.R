#Collection of all functions used in R to add text annotations to plots
#Author- Shefali C.
#Date- July 7, 2023

#Sources:
#Stdha.com

library(ggplot2)
library(ggpubr)
library(dplyr)
library(grid)
library(gridExtra)


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
grob <- grobTree(textGrob("Scatter plot", x=0.3,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot
sp2 + annotation_custom(grob)

#demo of grob text position in every facet. Same position.
sp2 + annotation_custom(grob)+facet_wrap(~cyl, scales="free")

#PRINTING A PARAGRAPH
#URL- https://www.rdocumentation.org/packages/ggpubr/versions/0.6.0/topics/text_grob

text <- paste("iris data set gives the measurements in cm",
              "of the variables sepal length and width",
              "and petal length and width, respectively,",
              "for 50 flowers from each of 3 species of iris.",
              "The species are Iris setosa, versicolor, and virginica.", sep = "\n")

# Create a text grob
tgrob <- text_grob(text, face = "italic", color = "steelblue")
# Draw the text
as_ggplot(tgrob)

#Applying same rule on mtcars graph. 
#The paragraph should be present below the graph.

sp2+
  theme(plot.margin = unit(c(t=1,r=0.5,b=5,l=1), "cm"))+
  annotation_custom(tgrob, xmin = unit(0,"npc"),
                    xmax = unit(5.5, "npc"),
                    ymin = -10, ymax = -1)

# annotation_custom(tableGrob(mytable, rows=NULL), 
#                   xmin=unit(11.5,"npc"),
#                   xmax = unit(14,"npc"),  
#                   ymin=3.7, ymax=7)
#   

#---------------
#URL- https://stackoverflow.com/questions/54271994/adding-text-outside-ggplot?rq=3

#Adding a table to RHS of the plot

A <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
B <- c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
C <- data.frame(A, B)

mytable<-cbind(c("variable_1","variable_2","variable_3"),c(0.5,1.5,3.5))
ggplot(data = C) + geom_point(mapping = aes(x = A, y = B)) + 
  labs(title = "Plot") + theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(1,5,1,1),"cm"))+
  annotation_custom(tableGrob(mytable, rows=NULL), 
                    xmin=unit(11.5,"npc"),xmax = unit(14,"npc"),  
                    ymin=3.7, ymax=7)

#create rectangle around the table
grid.rect(x=unit(0.83,"npc"),y=unit(0.5,"npc") ,
          width = unit(0.22,"npc"), height = unit(0.16,"npc"), 
          gp = gpar(lwd = 3, col="black", fill = NA))


#-------------