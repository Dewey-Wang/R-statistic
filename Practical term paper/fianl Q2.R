library(ggplot2)
library(patchwork)
library(tidyverse)
library(RColorBrewer)
require(sf)
library(plyr)

Germany_health <- read.csv("/Users/deweywang/Desktop/LMU/2\ -\ semester/Pretty\ Plots\ -\ Visualizing\ Statistical\ Data/final\ assignment/Data\ for\ termpaper-20220528/Germany_overall_health.csv")
colnames(Germany_health)[1:2] <- c("state", "state id")

######Q1
edu_gender <- Germany_health %>%
  filter(state == "Deutschland" & Standard == 1 & age_group == "Gesamt" & education != "all" & gender != "both")

edu_gender$education <- factor(edu_gender$education, 
                              levels = c("basic", "intermediate", "advanced"))


edu_gender_Fig <- edu_gender %>% ggplot(aes(x = education, y = Percent,fill = education))+
  geom_col() +
  facet_grid(~gender)+
  geom_errorbar(aes(ymin=LowerCL, ymax=UpperCL),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))  +
  geom_text(aes(label = round(UpperCL, digits = 2), y = UpperCL), vjust = -.5, size = 3) +
  geom_text(aes(label = round(LowerCL, digits = 2), y = LowerCL), vjust = 1.5, size = 3) +
  theme(legend.position = "none", 
        plot.title=element_text(hjust = 0.5, size = rel(1.5)), 
        axis.text        = element_text(colour = "black", size = rel(0.8)),
        axis.ticks       = element_line(colour = "black", size = rel(01)),
        # NB: match the *visual* thickness of axis ticks to the panel border
        #     0.5 clipped looks like 0.25
        
        # pure black panel border and grid lines, but thinner
        panel.background = element_blank(),
        panel.border     = element_rect(fill = NA, colour = "black", size = rel(1)),
        panel.grid       = element_line(colour = "black"),
        panel.grid.major = element_line(size = rel(0.1)),
        panel.grid.minor = element_line(size = rel(0.05)),
        
        # strips with black background and white text
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(
          colour = "white",
          size = rel(1),
          margin = margin(0.8 * 5.5, 0.8 * 5.5, 0.8 * 5.5, 0.8 * 5.5)
        )
        )+
  scale_fill_brewer("Dark2") +
  labs(y = "Percent",
       x = "Education Level",
       title = "Health assessment by education level and gender")

edu_gender_Fig

###############

age_gender <- Germany_health %>% filter(state == "Deutschland" & Standard == 0 & age_group != "Gesamt" & education == "all" & gender != "both")

age_gender_Fig <- age_gender %>% ggplot(aes(x = age_group, y = Percent,fill = age_group))+
  geom_col() +
  facet_grid(~gender)+
  geom_errorbar(aes(ymin=LowerCL, ymax=UpperCL),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))  +
  geom_text(aes(label = round(UpperCL, digits = 2), y = UpperCL), vjust = -.5, size = 3) +
  geom_text(aes(label = round(LowerCL, digits = 2), y = LowerCL), vjust = 1.5, size = 3) +
  theme(legend.position = "none", 
        plot.title=element_text(hjust = 0.5, size = rel(1.5)), 
        axis.text        = element_text(colour = "black", size = rel(0.8)),
        axis.ticks       = element_line(colour = "black", size = rel(01)),
        # NB: match the *visual* thickness of axis ticks to the panel border
        #     0.5 clipped looks like 0.25
        
        # pure black panel border and grid lines, but thinner
        panel.background = element_blank(),
        panel.border     = element_rect(fill = NA, colour = "black", size = rel(1)),
        panel.grid       = element_line(colour = "black"),
        panel.grid.major = element_line(size = rel(0.1)),
        panel.grid.minor = element_line(size = rel(0.05)),
        
        # strips with black background and white text
        strip.background = element_rect(fill = "black"),
        strip.text       = element_text(
          colour = "white",
          size = rel(1),
          margin = margin(0.8 * 5.5, 0.8 * 5.5, 0.8 * 5.5, 0.8 * 5.5)
        )
  )+
  scale_fill_brewer("Dark2") +
  labs(y = "Percent",
       x = "Age group",
       title = "Health assessment by age and gender")

age_gender_Fig
