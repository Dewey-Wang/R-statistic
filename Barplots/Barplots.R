library(ggplot2)
library(dplyr)

setwd("~/Desktop/LMU/2 - semester/Pretty Plots - Visualizing Statistical Data/Day 2/Data for barplots-20220511/")
#############Q1###############

raw_cancer <- read.csv(file = "Cancer_deaths_per100000_in_2016_WHO.csv",
                           header = TRUE)
raw_cancer <- raw_cancer %>% rename(year = Period,
                              sex = Dim1,
                              country = Location,
                              value = First.Tooltip)

cancer <- raw_cancer %>%
  filter(value != "No data")

cancer$value <- as.numeric(cancer$value) 

dev.off()
dev.new()
cancer %>% ggplot(aes(x = sex, y = value, fill = sex)) + 
  geom_boxplot()
#############Q2###############



raw_bmi_data <- read.csv("bmi_2016.csv",
                     header = TRUE)

bmi_data <- raw_bmi_data %>% 
  filter(!is.na(BMI))

bmi_data %>% ggplot(aes(y = reorder(COUNTRY, BMI), 
                                 x = BMI, colour = sex, shape = sex)) + 
  geom_point() +
  xlab("BMI")+
  theme(axis.title.y = element_blank(), axis.text.y =element_text(size=6)) + 
  facet_grid(rows = "REGION" , scale = "free", space = "free")


#############Q3###############
library(tidyverse)
library(readr)

cases <- read_delim("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", delim = ",")
deaths <- read_delim("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", delim = ",")
deaths.lf <- deaths %>%
  tidyr::pivot_longer(names_to = "date", values_to = "deaths",
                      -(1:4)) %>%
  dplyr::rename(country = "Country/Region") %>%
  dplyr::rename(province = "Province/State")
cases.lf <- cases %>%
  tidyr::pivot_longer(names_to = "date", values_to = "cases",
                      -(1:4)) %>%
  dplyr::rename(country = "Country/Region") %>%
  dplyr::rename(province = "Province/State")

all <- deaths.lf %>%
  inner_join(cases.lf, by = c("country", "province", "date",
                              "Lat", "Long"))

all.summarized <- all %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases), deaths = sum(deaths)) %>%
  ungroup()
  
all.summarized <- all.summarized %>%
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  arrange(date) %>%
  group_by(country) %>%
  mutate(newCases = cases - lag(cases), newDeaths = deaths -
           lag(deaths)) %>%
  filter(is.na(newCases) == F)
# saveRDS(all.summarized, file='covid2022.rds') #save
# tibble for day 2 exercises
canada_china <- all.summarized %>%
  filter(country %in% c("Canada", "China")) 



which(canada_china$cases >= 1000000)[1]
length(canada_china$date)


canada_china %>% ggplot(aes(y = cases, x = date, fill = country)) +
  geom_col()

ggplot(canada_china[871:1680,], aes(x = date, y = cases, fill=country)) + 
  geom_bar(position='dodge', stat = "identity")+
  labs(title='The graph start at the monent that cases more than one million.')


which(canada_china$date >= "2021-03-01")[1]

March_canada_china <- canada_china[807:1680,]
ggplot(March_canada_china, aes(x = date, y = cases, fill=country)) + 
  geom_bar(position='dodge', stat = "identity")+
  labs(title='The graph start at the monent that cases more than one million.')
