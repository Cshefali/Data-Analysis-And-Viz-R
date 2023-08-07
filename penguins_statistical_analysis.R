#Palmer Penguins data analysis- Statistical analysis vs ML
#Author- Shefali C.
#Last Updated- August 8, 2023

library(tidyverse)
library(palmerpenguins)

#data
penguins <- penguins

#scatter plot for flipper length vs body mass
ggplot(data = penguins, aes(x = body_mass_g, y = flipper_length_mm))+
  geom_point(aes(color = species), na.rm = T)+
  geom_smooth(se = F, na.rm = T)+
  theme_bw()

#penguins dataframe subset
penguins2 <- penguins %>% 
                select(species, flipper_length_mm, body_mass_g) %>% 
                drop_na()

## X- body mass in grams; Y - flipper length in mm

#calculating the mean and standard deviation of body mass and flipper length
mean_body_mass <- mean(penguins2$body_mass_g)
mean_flipper_len <- mean(penguins2$flipper_length_mm)

sd_body_mass <- sd(penguins2$body_mass_g)
sd_flipper_len <- sd(penguins2$flipper_length_mm)

#total number of species
n <- nrow(penguins2)

sum_zscores = 0

#calcualte correlation coefficient r
for (i in 1:n) {
  zscore_x = (penguins2$body_mass_g[i] - mean_body_mass)/sd_body_mass
  zscore_y = (penguins2$flipper_length_mm[i] - mean_flipper_len)/sd_flipper_len
  sum_zscores = sum_zscores + zscore_x*zscore_y
}

#Corr coefficient
r = sum_zscores/(n-1)

#slope of the line
m = r * (sd_flipper_len/sd_body_mass)

#Calculating y-intercept

#y_mean = m*x_mean + b
b = mean_flipper_len - (m*mean_body_mass)

#plotting the regression line
ggplot(data = penguins2, aes(x = body_mass_g, y = flipper_length_mm))+
  geom_point(color = "midnightblue")+
  geom_abline(slope = m, intercept = b, color = "black")+
  theme_bw()

#same plot with (xmean,ymean), and standard deviation lines
ggplot(data = penguins2, aes(x = body_mass_g, y = flipper_length_mm))+
  geom_point(color = "gold2")+
  geom_abline(slope = m, intercept = b, color = "black")+
  #dotted line passing through mean body mass
  geom_vline(xintercept = mean_body_mass, linetype = "dashed", color = "red",
             linewidth = 1)+
  #dotted line passing through mean flipper length
  geom_hline(yintercept = mean_flipper_len, linetype = "dashed", color = "red",
             linewidth = 1)+
  #line for 1 stardard deviation below and above the mean body mass
  geom_vline(xintercept = mean_body_mass + sd_body_mass, linetype = "dotted",
             color = "midnightblue", linewidth = 1.5)+
  geom_vline(xintercept = mean_body_mass - sd_body_mass, linetype = "dotted",
             color = "midnightblue", linewidth = 1.5)+
  #lines for 1 sd above and below the mean flipper length
  geom_hline(yintercept = mean_flipper_len + sd_flipper_len, linetype = "dotted",
             color = "midnightblue", linewidth = 1.5)+
  geom_hline(yintercept = mean_flipper_len - sd_flipper_len, linetype = "dotted",
             color = "midnightblue", linewidth = 1.5)+
  theme_bw()


#Data prep to create residual plot

#Equation for the regression line is: 
#predicted <- m*body_mass + b

y_predicted <- c()

for (i in 1:n) {
  predicted_length <- m*penguins2$body_mass_g[i] + b
  y_predicted <- append(y_predicted, predicted_length)
  
}

#create a dataframe with original body mass and predicted flipper length
predicted_data <- data.frame(species = penguins2$species,
                             original_body_mass = penguins2$body_mass_g,
                             original_flipper_length = penguins2$flipper_length_mm,
                             predicted_flipper_length = y_predicted)

#adding a colum with residual values
predicted_data <- predicted_data %>% 
                    mutate(residual = predicted_flipper_length - original_flipper_length)


#Residual plot

ggplot(data = predicted_data, aes(x = original_body_mass/1000, y = residual))+
  geom_point(color = "hotpink")+
  #scale_y_continuous(breaks = seq(-25, 25, 5))+
  #ylim(-25,25)+
  scale_y_continuous(breaks = seq(-25, 25, by = 5), 
                     labels = seq(-25,25,by = 5))+
  geom_hline(yintercept = 0, color = "black")+
  labs(title = "Residual plot for Flipper length vs body mass of penguins",
       x = "body mass (in kg)",
       y = "calculated residual")+
  theme_bw()+
  theme(plot.title = element_text(margin = margin(0.5,0,1,0)))