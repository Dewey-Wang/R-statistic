############Q1########3
cor_GHO <- readRDS("/Users/deweywang/Desktop/LMU/2\ -\ semester/Pretty\ Plots\ -\ Visualizing\ Statistical\ Data/Day\ 5/Data\ for\ Correlations-20220517/covid19_correlates_GHO.rds")
mortality <- read_csv("https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv")

mortality <- rename(mortality, COUNTRY = iso3c)

previous_years_mortality <- mortality %>% 
  filter(year < 2020)
pandemic_year<- mortality %>% 
  filter(year >= 2020)

mean_previous_years_mortality <- previous_years_mortality %>% 
  group_by(COUNTRY) %>% 
  summarise_at("deaths", mean, na.rm = TRUE)
mean_pandemic_year <- pandemic_year %>% 
  group_by(COUNTRY) %>% 
  summarise_at("deaths", mean, na.rm = TRUE)

excess_mortality2021 <- inner_join(mean_previous_years_mortality, mean_pandemic_year, by = "COUNTRY") %>%
  rename( previous_death = deaths.x, pandemic_death = deaths.y)

excess_mortality2021$ratio <- excess_mortality2021$pandemic_death/excess_mortality2021$previous_death


excess_mortality2021 <- inner_join(excess_mortality2021, cor_GHO[,c(2:4,6)], by = "COUNTRY")

############Q2#######


library(GGally) 
ggpairs(excess_mortality2021,
        columns = c("ratio","previous_death", "pandemic_death","Current health expenditure per capita in USD","Percent Population living in urban areas",
                    "Percent Population over 60"), 
        upper = list(continuous = "cor"),
        lower = list(continuous = "smooth"))


############Q3#######



shapiro.test(excess_mortality2021$`Current health expenditure per capita in USD`)

excess_mortality2021_rank <- excess_mortality2021 %>%
  drop_na("Current health expenditure per capita in USD","ratio") %>% # exclude countries with NAs in any
  mutate_if(is.numeric, rank)
#if in doubt about the above syntax, consult the data-wrangling cheat-sheet

excess_mortality2021_rank %>% 
  na.omit() %>% 
  cor.test( ~`Current health expenditure per capita in USD` + `ratio`,
            data= . ,
            method = "spearman")

excess_mortality2021_rank %>% 
  na.omit() %>% 
  cor.test( ~`Current health expenditure per capita in USD` + `ratio`,
            data= . ,
            method=c("pearson", "kendall", "spearman"))
