library(zoo)

Germany_RKI <- read.csv("Desktop/LMU/2 - semester/Pretty Plots - Visualizing Statistical Data/final assignment/Data for termpaper-20220528/covid_19_RKI_Bundeslaender2022.csv")
colnames(Germany_RKI)[c(1,6)] <- c("state id", "state")

Germany_RKI <- Germany_RKI[,-(2:3)] ##remove dupolcated data

#Germany_RKI$`incidence rate` <- Germany_RKI$cases/Germany_RKI$population_state * 100000
Germany_RKI$date <- as.Date.factor(Germany_RKI$date)
Germany_RKI$`state id`<- as.character(Germany_RKI$`state id`)
Germany_RKI$`state id`<- factor(Germany_RKI$`state id`, 
                            levels = c(1:16))

Germany_after_Nov_first <- filter(Germany_RKI[,c(1:6)],  date >="2020-11-01")

length(unique(Germany_after_Nov_first$date)) == (max(Germany_after_Nov_first$date) - min(Germany_after_Nov_first$date)+1)

group_date_state <- Germany_after_Nov_first %>% 
  group_by(date,state, `state id`, population_state)%>% 
  summarise_all(sum) %>%
  arrange(`state id`)

group_date_state$`incidence rate` <- group_date_state$cases/group_date_state$population_state * 100000


state_vs_date <- group_date_state[,c(1:2,7)] %>% pivot_wider(id_cols = date, names_from = state, values_from = `incidence rate`) 
state_vs_date_matrix <- zoo(data.matrix(state_vs_date[,-1]), state_vs_date$date)
roll_matrix <- rollsum(state_vs_date_matrix, 7, align = c( "right"))

roll_state <- data.frame(roll_matrix)
roll_state$date <- rownames(roll_state)  
library("stringr")

colnames(roll_state)[c(1,5,7,8,13,15)] <-  c("SchleswigHolstein",
                                             "NordrheinWestfalen",
                                             "RheinlandPfalz"   ,    
                                             "BadenWürttemberg" ,
                                             "MecklenburgVorpommern",
                                             "SachsenAnhalt" )

roll_state_long<-roll_state %>% pivot_longer(names_to = "state",
                                             values_to = "incidence rate",
                                             -17) 
roll_state_long$date <- as.Date.factor(roll_state_long$date)

roll_state_long%>% 
  ggplot()+
  geom_line(aes(x = date, y = `incidence rate`))+
  facet_grid(rows = vars(state))+
  geom_hline(yintercept = 100, linetype="dashed",color = "red")+
  geom_hline(yintercept = 200, linetype="dashed",color = "red")+
  scale_x_continuous(breaks=c(0, 100,200, 500, 1000, 2000),
                     labels=c(0, 100,200, 500, 1000, 2000))+
  theme_linedraw()+
  labs(x ="Date", y = "Incidence rate")


roll_state_long$state[roll_state_long$state == "SchleswigHolstein"] <- "Schleswig-Holstein"
roll_state_long$state[roll_state_long$state == "NordrheinWestfalen"] <- "Nordrhein-Westfalen"
roll_state_long$state[roll_state_long$state == "RheinlandPfalz"] <- "Rheinland-Pfalz"
roll_state_long$state[roll_state_long$state == "BadenWürttemberg"] <- "Baden-Württemberg"
roll_state_long$state[roll_state_long$state == "MecklenburgVorpommern"] <- "Mecklenburg-Vorpommern"
roll_state_long$state[roll_state_long$state == "SachsenAnhalt"] <- "Sachsen-Anhalt"

roll_state_long$state <- factor(roll_state_long$state, 
                                levels = unique(roll_state_long$state))

over_one_hundred <- roll_state_long %>% filter(`incidence rate` >= 100)

count_100<- as.data.frame(table(over_one_hundred$state)) ## Get their frequency

count_100$Var1 <- as.character(count_100$Var1)

count_100_sort <- arrange(count_100,Freq)

count_100$Var1 <- factor(count_100$Var1, levels = count_100_sort$Var1)

count_100%>%
  ggplot()+
  geom_col(aes(x = x, y = freq))+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.title = element_text(face="bold",size = 15),
        axis.line = element_line(colour = "grey50"),
        panel.background = element_blank())+
  labs(x = "State", 
       y = "Number",
       title = "Figure 1",
       subtitle = "Count of incidence > 100 per 7 days per 100,000",
       caption = "*From 1. November 2020 to 22. May 2022")+
  geom_text(aes(label = freq, y = freq, x = x), vjust = -.5, size = 3) +
  guides( x= guide_axis(n.dodge = 2))


over_two_hundred <- roll_state_long %>% filter(`incidence rate` >= 200)

count_200<- as.data.frame(table(over_two_hundred$state)) ## Get their frequency

count_200$Var1 <- as.character(count_200$Var1)

count_200_sort <- arrange(count_200,Freq)

count_200$Var1 <- factor(count_200$Var1, levels = count_200_sort$Var1)## sort their frequency to let the plot be more readable.

count_200_plot <- count_200%>%
  ggplot()+
  geom_col(aes(x = Var1, y = Freq))+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.title = element_text(face="bold",size = 15),
        axis.line = element_line(colour = "grey50"),
        panel.background = element_blank())+
  labs(x = "State", 
       y = "Number",
       title = "Figure 2",
       subtitle = "Count of incidence > 200 per 7 days per 100,000",
       caption = "*From 1. November 2020 to 22. May 2022")+
  geom_text(aes(label = Freq, y = Freq, x = Var1), vjust = -.5, size = 3) +
  guides( x= guide_axis(n.dodge = 2))