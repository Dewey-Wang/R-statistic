library(ggplot2)
library(tidyr)
library(lubridate)
library(dplyr)
library(ggpubr)
######Q1-1
penguins <- palmerpenguins ::penguins

penguins$year <- as.character.Date(penguins$year)

av_group_sex_species_year <- penguins %>% 
  na.omit() %>% 
  group_by(sex, species, year) %>% 
  summarise_at("flipper_length_mm", mean, na.rm = TRUE)

colnames(av_group_sex_species_year)[4] <- "av_flipper"


av_species_year <- penguins %>% na.omit() %>% 
  group_by( species, year) %>% 
  summarise_at("flipper_length_mm", mean, na.rm = TRUE)
colnames(av_species_year)[3] <- "av_flipper"

av_species_year$sex <- "both"

av_year <- rbind(av_group_sex_species_year, av_species_year)


av_year %>% ggplot( aes(x=year, y=av_flipper, color = sex, group = sex)) +
  geom_point()+
  geom_smooth()+
  facet_grid(~species)+
  labs(x = "Year",
       y = "Average flipper length(mm)",
       title = "Figure 1: The trend of growing flipper length by year")+
  theme_linedraw()+
  scale_color_manual(values=c( "#838391", "#fc6f6f", "#346eeb"))

#####################Q1-2
test_normal_d <- data_frame( sex = rep(c("female", "male", "both"), c(9,9,9)),
                             species = rep(rep(c("Adelie", "Chinstrap", "Gentoo"), c(3,3,3)),3),
                             year = rep(c("2007", "2008", "2009"), 9),
                             "p-value" = c(
                               shapiro.test(filter(penguins,sex == "female" & species == "Adelie" & year == "2007")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "female" & species == "Adelie" & year == "2008")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "female" & species == "Adelie" & year == "2009")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "female" & species == "Chinstrap" & year == "2007")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "female" & species == "Chinstrap" & year == "2008")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "female" & species == "Chinstrap" & year == "2009")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "female" & species == "Gentoo" & year == "2007")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "female" & species == "Gentoo" & year == "2008")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "female" & species == "Gentoo" & year == "2009")$flipper_length_mm)$p.value,
                               
                               shapiro.test(filter(penguins,sex == "male" & species == "Adelie" & year == "2007")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "male" & species == "Adelie" & year == "2008")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "male" & species == "Adelie" & year == "2009")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "male" & species == "Chinstrap" & year == "2007")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "male" & species == "Chinstrap" & year == "2008")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "male" & species == "Chinstrap" & year == "2009")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "male" & species == "Gentoo" & year == "2007")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "male" & species == "Gentoo" & year == "2008")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins,sex == "male" & species == "Gentoo" & year == "2009")$flipper_length_mm)$p.value,
                               
                               shapiro.test(filter(penguins, species == "Adelie" & year == "2007")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins, species == "Adelie" & year == "2008")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins, species == "Adelie" & year == "2009")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins, species == "Chinstrap" & year == "2007")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins, species == "Chinstrap" & year == "2008")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins, species == "Chinstrap" & year == "2009")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins, species == "Gentoo" & year == "2007")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins, species == "Gentoo" & year == "2008")$flipper_length_mm)$p.value,
                               shapiro.test(filter(penguins, species == "Gentoo" & year == "2009")$flipper_length_mm)$p.value
                             )
)

test_normal_d <- test_normal_d %>%
  mutate(">0.05" = case_when ( test_normal_d$`p-value` >= "0.05" ~ "TRUE",
                               test_normal_d$`p-value` < "0.05" ~ ""))

test_normal_d_table<- ggtexttable(test_normal_d, theme = ttheme("light"),  cols = colnames(test_normal_d), rows = NULL)%>%
  tab_add_vline(at.column = 2:4, column.side = "left",
                from.row = 2, linetype = 2)%>%
  tab_add_title(text = "Table 1: Shapiro-Wilk normality test")

test_normal_d_table


f_a7 <-penguins %>%
  na.omit() %>% 
  filter(year == "2007" & sex == "female"& species== "Adelie")
m_a7 <- penguins %>%
  na.omit()%>% 
  filter(year == "2007" & sex == "male"& species== "Adelie")
b_a7 <- penguins %>%
  na.omit()%>% 
  filter(year == "2007" & species== "Adelie")
f_a8 <-penguins %>%
  na.omit()%>% 
  filter(year == "2008" & sex == "female"& species== "Adelie")
m_a8 <- penguins %>%
  na.omit()%>% 
  filter(year == "2008" & sex == "male"& species== "Adelie")
b_a8 <- penguins %>%
  na.omit()%>% 
  filter(year == "2008" & species== "Adelie")
f_a9 <-penguins %>%
  na.omit()%>% 
  filter(year == "2009" & sex == "female"& species== "Adelie")
m_a9 <- penguins %>%
  na.omit()%>% 
  filter(year == "2009" & sex == "male"& species== "Adelie")
b_a9 <- penguins %>%
  na.omit()%>% 
  filter(year == "2009" & species== "Adelie")
#
f_c7 <-penguins %>%
  na.omit()%>% 
  filter(year == "2007" & sex == "female"& species== "Chinstrap")
m_c7 <- penguins %>%
  na.omit()%>% 
  filter(year == "2007" & sex == "male"& species== "Chinstrap")
b_c7 <-penguins %>%
  na.omit()%>% 
  filter(year == "2007" & species== "Chinstrap")
f_c8 <-penguins %>%
  na.omit()%>% 
  filter(year == "2008" & sex == "female"& species== "Chinstrap")
m_c8 <- penguins %>%
  na.omit()%>% 
  filter(year == "2008" & sex == "male"& species== "Chinstrap")
b_c8 <-penguins %>%
  na.omit()%>% 
  filter(year == "2008" & species== "Chinstrap")
f_c9 <-penguins %>%
  na.omit()%>% 
  filter(year == "2009" & sex == "female"& species== "Chinstrap")
m_c9 <- penguins %>%
  na.omit()%>% 
  filter(year == "2009" & sex == "male"& species== "Chinstrap")
b_c9 <-penguins %>%
  na.omit()%>% 
  filter(year == "2009" & species== "Chinstrap")
#
f_g7 <-penguins %>%
  na.omit()%>% 
  filter(year == "2007" & sex == "female"& species== "Gentoo")
m_g7 <- penguins %>%
  na.omit()%>% 
  filter(year == "2007" & sex == "male"& species== "Gentoo")
b_g7 <- penguins %>%
  na.omit()%>% 
  filter(year == "2007" & species== "Gentoo")
f_g8 <-penguins %>%
  na.omit()%>% 
  filter(year == "2008" & sex == "female"& species== "Gentoo")
m_g8 <- penguins %>%
  na.omit()%>% 
  filter(year == "2008" & sex == "male"& species== "Gentoo")
b_g8 <- penguins %>%
  na.omit()%>% 
  filter(year == "2008" & species== "Gentoo")
f_g9 <-penguins %>%
  na.omit()%>% 
  filter(year == "2009" & sex == "female"& species== "Gentoo")
m_g9 <- penguins %>%
  na.omit()%>% 
  filter(year == "2009" & sex == "male"& species== "Gentoo")
b_g9 <- penguins %>%
  na.omit()%>% 
  filter(year == "2009" & species== "Gentoo")

var_test <- data.frame( "type vs type" = c("male_Adelie 2007 vs. 2008" ,
                                           "male_Adelie 2008 vs. 2009" ,
                                           "female_Adelie 2007 vs. 2008",
                                           "female_Adelie 2008 vs. 2009",
                                           "male_Chinstrap 2007 vs. 2008",
                                           "male_Chinstrap 2008 vs. 2009",
                                           "female_Chinstrap 2007 vs. 2008",
                                           "female_Chinstrap 2008 vs. 2009",
                                           "male_Gentoo 2007 vs. 2008",
                                           "male_Gentoo 2008 vs. 2009",
                                           "female_Gentoo 2007 vs. 2008",
                                           "female_Gentoo 2008 vs. 2009",
                                           "both_Adelie 2007 vs. 2008",
                                           "both_Adelie 2008 vs. 2009",
                                           "both_Chinstrap 2007 vs. 2008",
                                           "both_Chinstrap 2008 vs. 2009",
                                           "both_Gentoo 2007 vs. 2008",
                                           "both_Gentoo 2008 vs. 2009"),
                        "p-value" = c(var.test((m_a7$flipper_length_mm),(m_a8$flipper_length_mm))$p.value,
                                      var.test((m_a8$flipper_length_mm),(m_a9$flipper_length_mm))$p.value,
                                      var.test((f_a7$flipper_length_mm),(f_a8$flipper_length_mm))$p.value,
                                      var.test((f_a8$flipper_length_mm),(f_a9$flipper_length_mm))$p.value,
                                      var.test((m_c7$flipper_length_mm),(m_c8$flipper_length_mm))$p.value,
                                      var.test((m_c8$flipper_length_mm),(m_c9$flipper_length_mm))$p.value,
                                      var.test((f_c7$flipper_length_mm),(f_c8$flipper_length_mm))$p.value,
                                      var.test((f_c8$flipper_length_mm),(f_c9$flipper_length_mm))$p.value,
                                      "NA",
                                      var.test((m_g8$flipper_length_mm),(m_g9$flipper_length_mm))$p.value,
                                      "NA",
                                      "NA",
                                      var.test((b_a7$flipper_length_mm),(b_a8$flipper_length_mm))$p.value,
                                      var.test((b_a8$flipper_length_mm),(b_a9$flipper_length_mm))$p.value,
                                      var.test((b_c7$flipper_length_mm),(b_c8$flipper_length_mm))$p.value,
                                      var.test((b_c8$flipper_length_mm),(b_c9$flipper_length_mm))$p.value,
                                      "NA",
                                      "NA"
                                      
                        )
)
var_test$p.value <- as.numeric(var_test$p.value)
var_test <- var_test %>%
  mutate(">0.05" = case_when ( var_test$p.value >= "0.05" ~ "TRUE",
                               var_test$p.value < "0.05" ~ ""))

var_test_table<- ggtexttable(var_test, theme = ttheme("light"),  cols = colnames(var_test), rows = NULL)%>%
  tab_add_vline(at.column = 2:3, column.side = "left",
                from.row = 2, linetype = 2)%>%
  tab_add_title(text = "Table 2: Variance test") %>%
  tab_add_footnote(text = "*NA means one of the samples doesn't fit t-test.", size = 10, face = "italic")

var_test_table

Wilcox_result_year <- data.frame(
  "statistic" = c(t.test((m_a7$flipper_length_mm),(m_a8$flipper_length_mm),var.equal = TRUE)$statistic,
                  t.test((m_a8$flipper_length_mm),(m_a9$flipper_length_mm),var.equal = TRUE)$statistic,
                  t.test((f_a7$flipper_length_mm),(f_a8$flipper_length_mm),var.equal = TRUE)$statistic,
                  t.test((f_a8$flipper_length_mm),(f_a9$flipper_length_mm),var.equal = TRUE)$statistic,
                  t.test((m_c7$flipper_length_mm),(m_c8$flipper_length_mm),var.equal = TRUE)$statistic,
                  t.test((m_c8$flipper_length_mm),(m_c9$flipper_length_mm),var.equal = TRUE)$statistic,
                  t.test((f_c7$flipper_length_mm),(f_c8$flipper_length_mm),var.equal = TRUE)$statistic,
                  t.test((f_c8$flipper_length_mm),(f_c9$flipper_length_mm),var.equal = TRUE)$statistic,
                  wilcox.test((m_g7$flipper_length_mm),(m_g8$flipper_length_mm))$statistic,
                  t.test((m_g8$flipper_length_mm),(m_g9$flipper_length_mm),var.equal = TRUE)$statistic,
                  wilcox.test((f_g7$flipper_length_mm),(f_g8$flipper_length_mm))$statistic,
                  wilcox.test((f_g8$flipper_length_mm),(f_g9$flipper_length_mm))$statistic,
                  t.test((b_a7$flipper_length_mm),(b_a8$flipper_length_mm),var.equal = TRUE)$statistic,
                  t.test((b_a8$flipper_length_mm),(b_a9$flipper_length_mm),var.equal = TRUE)$statistic,
                  t.test((b_c7$flipper_length_mm),(b_c8$flipper_length_mm),var.equal = TRUE)$statistic,
                  t.test((b_c8$flipper_length_mm),(b_c9$flipper_length_mm),var.equal = TRUE)$statistic,
                  wilcox.test((b_g7$flipper_length_mm),(b_g8$flipper_length_mm))$statistic,
                  wilcox.test((b_g8$flipper_length_mm),(b_g9$flipper_length_mm))$statistic
  ),
  "p-value" = c(t.test((m_a7$flipper_length_mm),(m_a8$flipper_length_mm),var.equal = TRUE)$p.value,
                t.test((m_a8$flipper_length_mm),(m_a9$flipper_length_mm),var.equal = TRUE)$p.value,
                t.test((f_a7$flipper_length_mm),(f_a8$flipper_length_mm),var.equal = TRUE)$p.value,
                t.test((f_a8$flipper_length_mm),(f_a9$flipper_length_mm),var.equal = TRUE)$p.value,
                t.test((m_c7$flipper_length_mm),(m_c8$flipper_length_mm),var.equal = TRUE)$p.value,
                t.test((m_c8$flipper_length_mm),(m_c9$flipper_length_mm),var.equal = TRUE)$p.value,
                t.test((f_c7$flipper_length_mm),(f_c8$flipper_length_mm),var.equal = TRUE)$p.value,
                t.test((f_c8$flipper_length_mm),(f_c9$flipper_length_mm),var.equal = TRUE)$p.value,
                wilcox.test((m_g7$flipper_length_mm),(m_g8$flipper_length_mm))$p.value,
                t.test((m_g8$flipper_length_mm),(m_g9$flipper_length_mm),var.equal = TRUE)$p.value,
                wilcox.test((f_g7$flipper_length_mm),(f_g8$flipper_length_mm))$p.value,
                wilcox.test((f_g8$flipper_length_mm),(f_g9$flipper_length_mm))$p.value,
                t.test((b_a7$flipper_length_mm),(b_a8$flipper_length_mm),var.equal = TRUE)$p.value,
                t.test((b_a8$flipper_length_mm),(b_a9$flipper_length_mm),var.equal = TRUE)$p.value,
                t.test((b_c7$flipper_length_mm),(b_c8$flipper_length_mm),var.equal = TRUE)$p.value,
                t.test((b_c8$flipper_length_mm),(b_c9$flipper_length_mm),var.equal = TRUE)$p.value,
                wilcox.test((b_g7$flipper_length_mm),(b_g8$flipper_length_mm))$p.value,
                wilcox.test((b_g8$flipper_length_mm),(b_g9$flipper_length_mm))$p.value
  ),
  "test strategy" = c(rep("t-test",8), 
                      "Wilcox-test",
                      "t-test",
                      "Wilcox-test",
                      "Wilcox-test",
                      rep("t-test",4),
                      "Wilcox-test",
                      "Wilcox-test")
)


rownames(Wilcox_result_year) <- c("male_Adelie 2007 vs. 2008" ,
                                  "male_Adelie 2008 vs. 2009" ,
                                  "female_Adelie 2007 vs. 2008",
                                  "female_Adelie 2008 vs. 2009",
                                  "male_Chinstrap 2007 vs. 2008",
                                  "male_Chinstrap 2008 vs. 2009",
                                  "female_Chinstrap 2007 vs. 2008",
                                  "female_Chinstrap 2008 vs. 2009",
                                  "male_Gentoo 2007 vs. 2008",
                                  "male_Gentoo 2008 vs. 2009",
                                  "female_Gentoo 2007 vs. 2008",
                                  "female_Gentoo 2008 vs. 2009",
                                  "both_Adelie 2007 vs. 2008",
                                  "both_Adelie 2008 vs. 2009",
                                  "both_Chinstrap 2007 vs. 2008",
                                  "both_Chinstrap 2008 vs. 2009",
                                  "both_Gentoo 2007 vs. 2008",
                                  "both_Gentoo 2008 vs. 2009")


Wilcox_result_year <- Wilcox_result_year %>%
  mutate(sig = case_when (p.value > 0.05 ~ "", 
                          p.value <= 0.05 & p.value > 0.01 ~ "*", 
                          p.value <= 0.01 & p.value > 0.001 ~ "**", 
                          p.value <=0.001 ~ "***"))

Wilcox_result_year_table<-ggtexttable(Wilcox_result_year, theme = ttheme("blank"))

Wilcox_result_year_table%>%
  tab_add_hline(at.row = c(1, 2), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_hline(at.row = c(19), row.side = "bottom", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = 2:5, column.side = "left",
                from.row = 2, linetype = 2) %>%
  tab_add_title(text = "Table 3: Test whether the year has a significant impact on flipper size")%>%
  tab_add_footnote(text = "* = p-value <= 0.05 & > 0.01  
  ** = p-value <= 0.01 & > 0.001
  *** = p-value <= 0.001              ", size = 10, face = "italic")