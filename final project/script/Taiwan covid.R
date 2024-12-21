library(dplyr)
library(ggmap)
library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
library(maptools)
library(knitr)
library(RColorBrewer) #配色用brewer.pal( 9 , "Reds" )
library(mapproj)
library(viridis)
library(ggthemes)
library(gganimate) 
library(ggplot2)
library(gifski)
library(tidyr)
library(rgdal)
library(magick)

cases_Taiwan_Covid<-read.csv("Ding Yang Wang/Taiwan_COVID_cases.csv")
cases_Taiwan_Covid$date <- as.Date.factor(cases_Taiwan_Covid$date)
test_accum <- cases_Taiwan_Covid

#####accumulate
test_accum<-cases_Taiwan_Covid %>% pivot_wider(id_cols = county, names_from = date, values_from = cases) 

test_accum[is.na(test_accum)] <- 0
for (a in 3:length(test_accum)) {
  for (b in 1:22) {
    if(test_accum[b,(a-1)] > test_accum[b,a]){
      test_accum[b,a] <- test_accum[b,(a-1)]
    }
  }
}

test_accum<-test_accum %>%pivot_longer(names_to = "date", values_to = "cases",
                                       -1) 
test_accum$date <- as.Date.factor(test_accum$date)

test_accum <- test_accum %>% arrange(date)
#####
taiwan_shp <- readOGR("Ding Yang Wang/gadm40_TWN_2.shp")
taiwan_map <- fortify(taiwan_shp)

chinese_name <- c("金門縣", "連江縣", "高雄市","新北市", "台中市", "台南市", "台北市",
                  "彰化縣", "嘉義市", "嘉義縣", "新竹市",
                  "新竹縣", "花蓮縣", "基隆市", "苗栗縣",
                  "南投縣", "澎湖縣", "屏東縣", "台東縣",
                  "桃園市", "宜蘭縣", "雲林縣")
mydata <- data.frame(NAME_1=taiwan_shp$NAME_2,
                     NAME_2= chinese_name,
                     id=1:22)
unique(taiwan_shp$NAME_2)

taiwan_map$id <- as.character(as.integer(taiwan_map$id)+1)
final.plot<-merge(taiwan_map,mydata,by="id",all.x=T)


test_name_lon<- final.plot %>% 
  group_by(NAME_2) %>%
  right_join(test_accum, by= c("NAME_2"=  "county"))

Taiwan_map_plot <- ggplot(taiwan_map, aes(x = long, y = lat, group=group)) +
  geom_polygon(fill="white", colour="black")+
  coord_map("mercator")
#############
test <- test_name_lon[test_name_lon$date > as.Date("2022-05-23"),]
test <- test %>% arrange(date)

test$date <- as.Date.factor(test$date)
unique(test$date)
test_animate <- test %>%
  ggplot(aes(x = long, y = lat, group = group,fill = cases))+
  geom_polygon( color = "black", size = 0.25) +#維持地圖比例   
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"),
                       name = "Confirmed COVID cases",
                       guide = guide_colorbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(100, units = "mm"),
                         draw.ulim = FALSE,
                         title.position = "top",
                         title.hjust = 0.5,
                         title.vjust = 0.5 )
  )+
  transition_manual(date )+
  labs(x = NULL, 
       y = NULL,
       title = "COVID-19 cases in Taiwan",
       subtitle = "Date: {current_frame}", 
       caption = "By Ding Yang Wang" )+
  theme(plot.title=element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(size = 14,
                                    margin = margin(t = 0.2,b = 0,unit = "cm"), 
                                    color = "#cfcfc6"),
        legend.position = "bottom",
        panel.background = element_rect(fill = "aliceblue"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  ) +
  coord_map(xlim = c(118, 122.1), ylim = c(21.8,25.7))


tmp <- animate(test_animate,  end_pause = 20, fps = 15)
anim_save("my3.gif", tmp)

