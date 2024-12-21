library(readr)
library(tidyverse)
library(dplyr)
cases <- read_delim("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", delim = ",")


cases<- cases %>% 
  select(-c(1,3,4)) %>% 
  ##the function between ## is to sum up the amount of case per day that group by the same country name
  group_by(`Country/Region`) %>% 
  summarise_all(funs(sum)) %>% 
  ##
  pivot_longer(names_to = "date", values_to = "cases",-1) %>%##to make a long column data into row data
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  arrange(date)%>%
  group_by(`Country/Region`) %>%
  mutate(new_cases = cases - lag(cases))

num <- 1

while(is.na(cases$new_cases[num])){
  cases$new_cases[num] <- cases$cases[num]
  num <- num+1
}


deaths <- read_delim("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", delim = ",")

deaths<- deaths %>% 
  select(-c(1,3,4)) %>% 
  ##the function between ## is to sum up the amount of deaths per day that group by the same country name
  group_by(`Country/Region`) %>% 
  summarise_all(funs(sum)) %>% 
  ##
  pivot_longer(names_to = "date", values_to = "deaths",-1) %>%##to make a long column data into row data
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  arrange(date)%>%
  group_by(`Country/Region`)%>%
  mutate(new_deaths = deaths - lag(deaths))

num <- 1

while(is.na(deaths$new_deaths[num])){
  deaths$new_deaths[num] <- deaths$deaths[num]
  num <- num+1
}

cases_deaths <- inner_join(cases, deaths, by = c("Country/Region", "date"))

cases_deaths<- rename(cases_deaths, country = "Country/Region")

recent_cases_deaths<- cases_deaths[(nrow(cases_deaths)-length(unique(cases_deaths$country))
*7+1):nrow(cases_deaths),]

recent_cases_deaths <- recent_cases_deaths%>%
  filter(new_cases >= 10 & new_deaths >= 10)

recent_cases_deaths %>% ggplot(aes(y = log(new_deaths), x = log(new_cases))) +
  geom_point() + 
  stat_smooth(method = "lm") +
  theme(legend.position = "none")+
  facet_grid(date~.)

########Q2########

last_date_cases_deaths <- cases_deaths[(nrow(cases_deaths)-length(unique(cases_deaths$country))+1):nrow(cases_deaths),]
last_date_cases_deaths <- last_date_cases_deaths%>%
  filter(new_cases >= 10 & new_deaths >= 10)

lm.last_date_cases_deaths <- lm( log(new_deaths)  ~ log(new_cases), last_date_cases_deaths) 
layout(matrix(1:4,2,2)) #divide your plotting window 
plot(lm.last_date_cases_deaths)

summary(lm.last_date_cases_deaths)

################Q3
library(broom)

last_date_cases_deaths <- cases_deaths[(nrow(cases_deaths)-length(unique(cases_deaths$country))+1):nrow(cases_deaths),]
last_date_cases_deaths <- last_date_cases_deaths%>%
  filter(new_cases >= 10 & new_deaths >= 10)

last_date_Diagnostics <- last_date_cases_deaths%>%
  select(country, new_deaths,new_cases)%>%
  nest() %>%
  mutate(linear_mod = map(data,  ~ tidy(lm(log(new_deaths) ~ log(new_cases),data=.)))) %>% 
  unnest(linear_mod)

mod <- lm(log(new_deaths) ~ log(new_cases), last_date_cases_deaths) # get the least squares fit
mod

b <- mod$coef

last_date_cases_deaths$CDest <- b[1] + b[2]*log(last_date_cases_deaths$new_cases)
m <- mean(log(last_date_cases_deaths$new_deaths))
SSM <- sum((m - last_date_cases_deaths$CDest)^2)
SST <- sum((m - log(last_date_cases_deaths$new_deaths))^2)
Rsquared <- SSM/SST
Rsquared

################Q4
last_date_cases_deaths$SR <- (log(last_date_cases_deaths$new_deaths) - last_date_cases_deaths$CDest)^2

SSE <- sum(last_date_cases_deaths$SR)
last_date_cases_deaths[order(last_date_cases_deaths$SR),]
ten_countries <- last_date_cases_deaths[order(last_date_cases_deaths$SR, decreasing = TRUE),]

ten_countries <- ten_countries[c(1:10),-c(2:7)]

ten_countries$SR[10]
################Q5


last_date_cases_deaths %>% ggplot(aes(y = log(new_deaths),label= country, x = log(new_cases))) +
  geom_point() + 
  stat_smooth(method = "lm") +
  geom_text(aes(label=ifelse(SR >= ten_countries$SR[10] ,as.character(country),'')),hjust=0,vjust=0)

text(labels=ifelse(last_date_cases_deaths$country == ten_countries[1]), col="red")







