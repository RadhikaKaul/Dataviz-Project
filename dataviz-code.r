# Assignment 3

# Author: Radhika Kaul

# Dataset used: Quality of government dataset

#Summary: Is a compilation of several datasets and consists more than 2000 variables

# Loading necessary packages
library(tidyverse)
library(haven)


# Loading the Quality of Government Dataset

Data <- read_csv("data/qog_std_cs_jan20.csv")


# Visualization 1: Correlation b/w Fragile States and Corruption Perception in the country


df_1 <- select(Data, cname, ffp_fsi, ti_cpi, ht_region) 
df_1$ht_region <- as.factor(df_1$ht_region)


  ggplot(df_1, mapping = aes(x = ffp_fsi, y = ti_cpi, color = ht_region))+
  geom_point()+
    labs(title = paste("Fragile States are on average, more corrupt"),
         caption ="Quality of Government Dataset.",         
         x = "Fragile States Index",
         y = "Corruption Perceptions Index")+
    scale_x_continuous(limits=c(0,120),
                       breaks=c(0, 30, 60, 90, 120))+
    scale_y_continuous(limits=c(0,100),
                       breaks= c(0, 20, 40, 60, 80, 100))+
  theme_minimal()+
    theme(axis.title.x=element_text(hjust=+0),
          axis.title.y=element_text(angle=0, hjust=+1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(angle = 0),
          axis.text.y = element_text(angle = 0),
          axis.line.x = element_line(linetype = "solid"),
          axis.line.y = element_line(linetype = "solid"),
          axis.ticks.x = element_line(),
          axis.ticks.y = element_line(),
          plot.margin = unit(c(1,1,1,1), "cm"))
  
# Corruption perception and fragile states index is negatively correlated, but need to add more information about inference.
  
  
  
  
  

    
