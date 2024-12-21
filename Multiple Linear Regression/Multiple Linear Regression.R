library(GGally)
library(dplyr)
library(readr)

GHO <- readRDS("/Users/deweywang/Desktop/LMU/2\ -\ semester/Pretty\ Plots\ -\ Visualizing\ Statistical\ Data/Day\ 5\ Correlation/Data\ for\ Correlations-20220517/covid19_correlates_GHO_UPDATED.rds")
mortality <- read_csv("https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv")

mortality <- rename(mortality, COUNTRY = iso3c)

previous_years_mortality <- mortality %>% 
  filter(year < 2020) %>% 
  select(COUNTRY,country_name,deaths)
pandemic_year<- mortality %>% 
  filter(year >= 2020)%>% 
  select(COUNTRY,country_name,deaths)

mean_previous_years_mortality <- previous_years_mortality %>% 
  group_by(COUNTRY, country_name) %>% 
  summarise_all(mean, na.rm = TRUE)
mean_pandemic_year <- pandemic_year %>% 
  group_by(COUNTRY, country_name) %>% 
  summarise_at("deaths", mean, na.rm = TRUE)

excess_mortality2021 <- inner_join(mean_previous_years_mortality, mean_pandemic_year, by = c("COUNTRY", "country_name")) %>%
  rename( previous_death = deaths.x, pandemic_death = deaths.y)

excess_mortality2021$ratio <- excess_mortality2021$pandemic_death/excess_mortality2021$previous_death


excess_mortality2021 <- inner_join(excess_mortality2021, GHO[,c(2:3,5:7)], by = "COUNTRY")
excess_mortality2021 <- excess_mortality2021[,-(2:4)]

excess_mortality2021<-rename(excess_mortality2021,
       "excess_mort." =ratio,
       "over_65" = over_65, #pop over 65
       "urban" = `Percent Population living in urban areas`,
       "health_exp" = `Current health expenditure per capita in USD`)

excess_mortality2021 <- excess_mortality2021%>%
  filter(!is.na(health_exp), !is.na(over_65), !is.na(urban))
###################Q1########
mod <- lm( excess_mort.  ~ over_65 + urban + health_exp, data= excess_mortality2021 ) 

layout(matrix(1:4,2,2))

plot(mod)




excess_mortality2021<- excess_mortality2021[-70,]
mod <- lm( excess_mort.  ~ over_65 + urban + health_exp, data= excess_mortality2021 ) 
mod %>% filter(residuals > 0.2)
select(mod,residuals)
order(filter(mod$residuals > 0.2))
layout(matrix(1:4,2,2))
plot(mod)
###################Q2########
COVID <- tidycovid19::download_merged_data(silent = TRUE, cached = TRUE)
country_over_50k <- COVID %>% 
  filter(confirmed>50000)
  
country_over_50k <- unique( country_over_50k$country)
country_over_50k  

confirmed_cases <- COVID%>% 
  filter(country == "Taiwan")%>%
  ggplot(aes(y=confirmed, x=date)) + 
  geom_point() +
  stat_smooth(method=loess, se=F)

vaccinations <- COVID%>% 
  filter(country == "Taiwan")%>%
  ggplot(aes(y=total_vaccinations, x=date)) + 
  geom_point() +
  stat_smooth(method=loess, se=F)
positive_rate <- COVID%>% 
  filter(country == "Taiwan")%>%
  ggplot(aes(y=positive_rate, x=date)) + 
  geom_point() +
  stat_smooth(method=loess, se=F)

total_tests <- COVID%>% 
  filter(country == "Taiwan")%>%
  ggplot(aes(y=total_tests, x=date)) + 
  geom_point() +
  stat_smooth(method=loess, se=F)

cowplot::plot_grid(confirmed_cases, vaccinations,positive_rate,total_tests)
mod$residuals[70]
