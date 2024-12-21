
raw_bmi_data <- read.csv("~/Desktop/LMU/2 - semester/Pretty Plots - Visualizing Statistical Data/Day 2/Data for barplots-20220511/bmi_2016.csv",
                         header = TRUE)

bmi_data <- raw_bmi_data %>% 
  filter(!is.na(BMI))

USA_bmi_data <- bmi_data %>% 
  filter(REGION == "AMR")
EU_bmi_data <- bmi_data %>% 
  filter(REGION == "EUR")

grid_USA <- with(USA_bmi_data, seq(min(BMI), max(BMI), length = 70))
library(plyr)

describeDist_USA <- ddply(USA_bmi_data, "sex", function(df) {
  data.frame( 
    BMI = grid_USA,
    density = dnorm(grid_USA,
                    mean(df$BMI, na.rm = T),
                    sd(df$BMI, na.rm = T))
  )
})
 
  
grid_EU <- with(EU_bmi_data, seq(min(BMI), max(BMI), length = 100))
describeDist_EU <- ddply(EU_bmi_data, "sex", function(df) {
  data.frame( 
    BMI = grid,
    density = dnorm(grid,
                  mean(df$BMI, na.rm = T),
                  sd(df$BMI, na.rm = T))
)
})
dev.off()
dev.new()

EU_dist <- EU_bmi_data %>% ggplot(aes(x = BMI, fill =sex))+ 
  geom_density(alpha = 0.3) + 
  ylab("density")+
  geom_line(aes(y = density), data = describeDist_EU, colour = "red")+
  ggtitle("EU")
  
USA_dist <- USA_bmi_data %>% ggplot(aes(x = BMI, fill =sex))+ 
  geom_density(alpha = 0.3) + 
  ylab("density")+
  geom_line(aes(y = density), data = describeDist_USA, colour = "red")+
  ggtitle("USA")

library("cowplot")                 # Load cowplot
plot_grid(EU_dist, USA_dist,   
          ncol = 1,
          align = "v")

######################Q2
EU_bmi_male <- filter(EU_bmi_data, sex == "Male")
EU_bmi_female <- filter(EU_bmi_data, sex == "Female")
USA_bmi_male <- filter(USA_bmi_data, sex == "Male")
USA_bmi_female <- filter(USA_bmi_data, sex == "Female")

shapiro.test(EU_bmi_male$BMI)
shapiro.test(EU_bmi_female$BMI)
shapiro.test(USA_bmi_male$BMI)
shapiro.test(USA_bmi_female$BMI)

summary_BMI_EU_USA_gender <- data.frame(EU_male = c(shapiro.test(EU_bmi_male$BMI)$statistic,
                                             shapiro.test(EU_bmi_male$BMI)$p.value),
                              EU_female =c(shapiro.test(EU_bmi_female$BMI)$statistic,
                                           shapiro.test(EU_bmi_female$BMI)$p.value),
                              USA_male =c(shapiro.test(USA_bmi_male$BMI)$statistic,
                                          shapiro.test(USA_bmi_male$BMI)$p.value),
                              USA_female =c(shapiro.test(USA_bmi_female$BMI)$statistic,
                                            shapiro.test(USA_bmi_female$BMI)$p.value)
                              )
rownames(summary_BMI_EU_USA_gender) <- c("statistic", "P-value")

summary_BMI_EU_USA_gender
t.test(BMI ~sex, EU_bmi_data)
t.test(BMI ~sex, USA_bmi_data)


USA_EU_bmi_data <- bmi_data %>% 
  filter(REGION == "EUR" |REGION == "AMR")
summary_BMI_EU_USA <- data.frame(EU =c(shapiro.test(EU_bmi_data$BMI)$statistic,
                                                     shapiro.test(EU_bmi_data$BMI)$p.value),
                                        USA = c(shapiro.test(USA_bmi_data$BMI)$statistic,
                                                      shapiro.test(USA_bmi_data$BMI)$p.value)
)
rownames(summary_BMI_EU_USA_gender) <- c("statistic", "P-value")

t.test(BMI ~REGION, USA_EU_bmi_data)


