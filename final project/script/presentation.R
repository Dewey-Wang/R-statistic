 library(jsonlite)
library(RCurl)  
library(dplyr)
library(ggplot2)

Sys.Date()
raw_date_list <- seq(as.Date("2022-01-01"), as.Date("2022-03-01"), by="days")
date_list <- vector ()

for (a in 1:length(raw_date_list)) {
  if (weekdays(raw_date_list[a]) != "Saturday" && weekdays(raw_date_list[a]) != "Sunday"){
    date_list <- append( date_list,as.character.Date(raw_date_list[a]))
  }
}
date_list

bio_index <- vector()
bio_date<- vector()
for (numdate in 1:length(date_list)) {
  aqi_url <- paste0("https://www.twse.com.tw/exchangeReport/MI_INDEX?response=json&date=",gsub("-", "",date_list[numdate]),"&type=ALL")
  aqi <- fromJSON(getURL(aqi_url, encoding = "latin1"))
  if (length(aqi)!=2){
    bio_date <- append(bio_date, date_list[numdate]) 
    bio_index <- append(bio_index, aqi$data1[27,2]) 
  }
} 

bio_industry <- data.frame(
  date = as.Date(bio_date),
  index = bio_index
)
before_COVID <- data.frame(
  bio_industry[(bio_industry$date< "2020-01-01"),],
  Time = rep(c("before_2020"), length(bio_industry[(bio_industry$date< "2020-01-01"),]))
)

after_COVID <- data.frame(
  bio_industry[(bio_industry$date>= "2020-01-01"),],
  Time = rep(c("after_2020"), length(bio_industry[(bio_industry$date>= "2020-01-01"),]))
)

bio_industry <- rbind(before_COVID,after_COVID)
bio_industry$index <- as.numeric(bio_industry$index)

################################
bio_industry <- read.csv("Desktop/LMU/2 - semester/Pretty Plots - Visualizing Statistical Data/presentation/bio_industry_index.csv")
bio_industry$date <- as.Date(bio_industry$date)
bio_industry$Time <- factor(bio_industry$Time, 
                            levels = c("before_2020", "after_2020"))

before_COVID <- data.frame(
  bio_industry[(bio_industry$date< "2020-01-01"),])
after_COVID <- data.frame(
  bio_industry[(bio_industry$date>= "2020-01-01"),])
############################
linear_plot <- bio_industry %>% ggplot(aes( x = as.Date(date),y = index)) +
  geom_line(aes(color = Time))+
  labs(x = "Date",
       y = "Bio-stock index",
       title = 'Change of Bio stock-index')+
  scale_x_continuous(breaks=c(as.Date("2020-01-21"),as.Date("2020-01-01"),
                              as.Date("2020-03-19"),as.Date("2020-06-07"),
                              as.Date("2021-07-19"), as.Date("2018-01-01"),
                              as.Date("2019-01-01")),
                     labels=c(as.Date("2020-01-21"),as.Date("2020-01-01"),
                              as.Date("2020-03-19"),as.Date("2020-06-07"),
                              as.Date("2021-07-19"),as.Date("2018-01-01"),
                              as.Date("2019-01-01"))
                     )+
  geom_vline(xintercept = as.Date("2020-01-01"), linetype="dashed",color = "black")+ # 台灣首例
  geom_vline(xintercept = as.Date("2020-01-21"), linetype="dashed",color = "black")+ # 高敦疫苗製造＆施打
  geom_vline(xintercept = as.Date("2020-06-07"), linetype="dashed",color = "black")+
  geom_vline(xintercept = as.Date("2020-03-19"), linetype="dashed",color = "black")+ # 臺灣全面禁止外籍人士入境
  geom_vline(xintercept = as.Date("2021-07-19"), linetype="dashed",color = "black")+
  theme(legend.position="top", plot.title=element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face="bold"),
        title = element_text(face="bold"),
        axis.text.x=element_text(size=5), 
        axis.line = element_line(colour = "grey50"),
        panel.background = element_blank())+ # 政府實施防疫鬆綁政策
  guides(x = guide_axis(angle = 90)) # 台灣首例  

  
box_plot <- ggplot(bio_industry, aes( y = index, x = Time, color = Time)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3)+
  labs(x = "Before and After pandemic",
       y = "Bio-stock index",
       title = 'Boxplot with points')+
  theme(legend.position="top", plot.title=element_text(hjust = 0.5),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        title = element_text(face="bold"),
        axis.line = element_line(colour = "grey50"),
        panel.background = element_blank())


ggplot(bio_industry, aes( y = index, x = Time, color = Time)) +
  geom_violin() +
  xlab("Before and After pandemic") +
  ylab("Bio-stock index")
#######################
summary(after_COVID$index)
After_2020 =
summary_index_before_after <-summary(after_COVID$index)
summary_index_before_after <-rbind.data.frame(summary(before_COVID$index),summary_index_before_after)                                   
row.names(summary_index_before_after) <- c("Before_2020", "After_2020")
library(ggpubr)

Interpret_boxplot<- ggtexttable(summary_index_before_after, theme = ttheme("light"))%>%
  tab_add_footnote(text = "*Interpret boxplot", size = 10, face = "italic")
library(patchwork)
layout <- "
AAAA
BBBBBB
"

box_plot/Interpret_boxplot + plot_layout(heights = c(6, 2))

###################

date_event <- data.frame(Date = c(as.Date("2020-01-01"),as.Date("2020-01-21"),
                                  as.Date("2020-03-19"),as.Date("2020-06-07"),
                                  as.Date("2021-07-19")),
                         Event = c("First confirmed COVID case in world.",
                                   "First case in Taiwan.",
                                   "Taiwan bans foreigners enter the border.",
                                   "Government loosen pandemic policy.",
                                   "Tawianese brand vaccine manufacturing."))
date_event_linear_lpot<- ggtexttable(date_event, theme = ttheme("light"))

linear_plot/date_event_linear_lpot + plot_layout(heights = c(7,5))

#We will use Shapiro-Wilk normality test here. If function shapiro.test shows the p-value > 0.05, it means it is normal distribution.

wilcox.test(after_COVID$index[1:486], before_COVID$index, paired = TRUE, alternative = "two.sided")
######################

bio_industry[(bio_industry$index<47),]
#######################
COVID <- tidycovid19::download_merged_data(silent = TRUE, cached = TRUE)%>% 
  filter(country == "Taiwan")
COVID<-COVID%>% arrange(date)

COVID$total_vaccinations[is.na(COVID$total_vaccinations)] <- 0
for (a in 426:length(COVID$total_vaccinations)) {
  if(COVID$total_vaccinations[a] == 0){
    COVID$total_vaccinations[a] <- COVID$total_vaccinations[a-1]
  }
}
confirmed_cases <- COVID%>%
  ggplot(aes(y=confirmed, x=date)) + 
  geom_line() 
vaccinations <- COVID%>% 
  filter(country == "Taiwan")%>%
  ggplot(aes(y=total_vaccinations, x=date)) + 
  geom_line() 

positive_rate <- COVID%>% 
  filter(country == "Taiwan")%>%
  ggplot(aes(y=positive_rate, x=date)) + 
  geom_line() 

total_tests <- COVID%>% 
  filter(country == "Taiwan")%>%
  ggplot(aes(y=total_tests, x=date)) + 
  geom_line() 

cowplot::plot_grid(confirmed_cases, vaccinations,positive_rate,total_tests)
###############

daily <- tidycovid19::download_merged_data(silent = TRUE, cached = TRUE)
twn<- daily %>% filter(iso3c == "TWN" ) %>% arrange(date) %>%
  mutate("New Cases" = confirmed - lag(confirmed),
         "New Deaths" = deaths - lag(deaths),
         "Daily vac." = case_when(is.na(total_vaccinations) ~0,
                                        T~total_vaccinations ),
         "Daily vaccination rate" = total_vaccinations/23215015,
          "Positive rate" = positive_rate,
          "Workplaces" = gcmr_workplaces,
          "Residential"= gcmr_residential
  )
library(RColorBrewer)
library(tidyr)

Q1 <-  twn %>% dplyr::select(date, `New Cases`, `New Deaths`, `Daily vac.`,
                             `Positive rate`, `Workplaces` , `Residential` ) %>%
  filter(date > "2020-01-01" & date < "2022-05-21") 
Q1[is.na(Q1)] <- 0
p1<-Q1 %>% 
  pivot_longer(names_to = "type", values_to = "number", -1) %>% 
  ggplot( aes(x=date, y=number,col=type)) + 
  geom_point(col='lightgrey')+
  stat_smooth(method=loess, method.args=list(span=0.1), se =F)+
  facet_grid(type ~., scales = "free")+ scale_color_brewer(palette = "Dark2")+
  theme_bw()+
  theme(legend.position = "None",
        plot.title=element_text(hjust = 0.5, size = 16, face = "bold"))+
  labs(x = "Date", 
       y = "Number",
       title = "COVID-19 in Taiwan",
       caption = "*Daily vac. = Daily vaccination")

p1
library(RColorBrewer) 


correlation_index %>% 
  pivot_longer(names_to = "type", values_to = "number", -1) %>% 
  ggplot( aes(x=date, y=number,col=type)) + 
  geom_point(col='lightgrey', size  = 0.5)+
  stat_smooth(method=loess, method.args=list(span=0.1), se =F)+
  facet_grid(type ~., scales = "free")+ 
  scale_color_manual(breaks = c("Bio-index","Daily vac.", 
                               "New Cases",
                               "New Deaths","Positive rate",
                               "Residential",
                               "Workplaces"),
                    values=c("#00BFC4","#619CFF","#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"))+
  theme_bw()+
  theme(legend.position = "None",
        plot.title=element_text(hjust = 0.5, size = 16, face = "bold"))+
  labs(x = "Date", 
       y = "Number",
       title = "COVID-19 in Taiwan",
       caption = "*Daily vac. = Daily vaccination")
 

library(Hmisc)  
library(broom) 
library(GGally) 
library(scales)
library(tidyverse)

ggpairs(Q1 %>% 
          select_if(is.numeric), upper = list(continuous = "cor"), lower = list(continuous = "smooth"), mapping = ggplot2::aes(alpha=0.5))

correlation_index <- Q1%>%
  inner_join(bio_industry, by = c("date"="date")) 
correlation_index <- correlation_index[,c(-8,-10)]
colnames(correlation_index)[8] <- "Bio-index"
ggpairs(correlation_index %>% 
          select_if(is.numeric), upper = list(continuous = "cor"), lower = list(continuous = "smooth"), mapping = ggplot2::aes(alpha=0.5))

df <- correlation_index %>% select_if(is.numeric)
cor_return <- df[,sort(colnames(df))] %>%
  as.matrix() %>% # rcorr cannot deal with tibbles 
  rcorr(type = "spearman") %>% # do the correlation
  tidy() %>%
  mutate(sig = case_when (is.na(p.value) ~ "",
                          p.value > 0.1 ~"",
                          p.value <= 0.1 & p.value > 0.05 ~ ".", 
                          p.value <= 0.05 & p.value > 0.01 ~ "*", 
                          p.value <= 0.01 & p.value > 0.001 ~ "**", 
                          p.value <0.001 ~ "***"),
         lab = ifelse(is.na(estimate), "", paste(round(estimate, digits = 2), sig))) %>%
  arrange(column1,column2)

index_cor_inter <- cor_return %>% filter( column1=="Bio-index" | column2=="Bio-index" ) 

col_names <- unique( c(cor_return$column1, cor_return$column2)) 
dummy <- data.frame(column1 = col_names,
                    column2 = col_names, estimate = NA, p.value = NA)
df2 <- cor_return %>% dplyr::select(-n) %>%
  bind_rows(dummy)

df2 %>% ggplot(aes(x = column1, y = column2 )) +
  geom_tile(aes(fill = estimate)) + 
  geom_text(aes(label = lab)) + scale_fill_gradient2(low = muted("blue"),
                                                     mid = "white",
                                                     high = muted("red")) +
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.text = element_text(size = 12,
                                 family = "serif"),
        legend.position = "bottom", # move legend to bottom
        axis.ticks = element_blank() )
df2 <- df2%>%arrange(estimate)
index_cor_plot <- df2 %>% filter(column1=="Bio-index" | column2 == "Bio-index") %>% 
  na.omit %>% 
  ggplot(aes(x=paste(column1,"\nvs.\n",column2),y=estimate))+ 
  geom_col(aes(fill=estimate))+
  geom_text(aes(label=sig),size=6)+
  ylab(expression(rho))+ coord_flip()+
  scale_fill_gradient2(low = "#00BFC4", high = "#00BFC4",
                       name = "Estimate",
                       guide = guide_colorbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(100, units = "mm"),
                         draw.ulim = FALSE,
                         title.position = "top",
                         title.hjust = 0.5,
                         title.vjust = 0.5 )
                       )+ 
  theme_bw()+
  labs(title = "Correlation between Bio-index and other factors",
       caption = "* => p-value > 0.05 & <0 .1    \n** => p-value > 0.001 & < 0.01\n*** => p-value < 0.001              ")+
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        plot.title=element_text(hjust = 0.5, size = 16, face = "bold"))
  
index_cor_inter <- index_cor_inter[c(6:1),]
index_cor_inter <- index_cor_inter[, c(-4,-6,-7)]
index_cor_inter$p.value <- as.character(index_cor_inter$p.value)
index_cor_inter[c(1,4),4] <- "<0.001"
index_cor_inter[2,4] <- "0.189"
index_cor_inter[3,4] <- "0.016"
index_cor_inter[5,4] <- "0.167"
index_cor_inter[6,4] <- "0.008"
index_cor_inter[6,3]<- round(index_cor_inter[6,3], digits = 3)
date_event_linear_lpot<- ggtexttable(index_cor_inter, theme = ttheme("light"))
par(mfrow=c(1,2))
layout <- "
AAAA
######
BBBBBB
"
index_cor_plot/date_event_linear_lpot + plot_layout(heights = c(10,1,2),design = layout)
