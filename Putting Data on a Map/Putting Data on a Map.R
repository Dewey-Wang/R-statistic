library(CoordinateCleaner)
library(leaflet)

covid_summary <- tidycovid19::download_merged_data(silent = TRUE, cached = TRUE)

last_day <- covid_summary[(covid_summary$date == "2021-05-23"),]
last_day <- last_day[,1:4] %>% na.omit()

lat_lon_last_day<- countryref %>% 
  group_by(iso3) %>%
  summarise(Long = mean(centroid.lon),
            Lat = mean(centroid.lat)) %>%
  right_join(last_day, by= c("iso3"="iso3c")) 
  

pal <- colorNumeric(palette = "plasma",
                    domain = lat_lon_last_day$confirmed)
lat_lon_last_day %>% leaflet() %>%
  addTiles() %>% addCircles(lng = ~Long,
                            lat = ~Lat,
                            radius = ~ confirmed*0.05, # unfortunately leaflet does not automatically 
                            popup = ~ country,
                            color = ~ pal(confirmed),
                            fillOpacity = 0.5,
                            stroke = FALSE) %>%
  addLegend("bottomright", pal = pal, values = ~ confirmed, title = "Cases",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.5) 


early_day <- covid_summary[(covid_summary$date == "2020-12-31"),]
early_day <- early_day[,1:4] %>% na.omit() %>% filter(confirmed !=0)

lat_lon_early_day<- countryref %>% 
  group_by(iso3) %>%
  summarise(Long = mean(centroid.lon),
            Lat = mean(centroid.lat)) %>%
  right_join(early_day, by= c("iso3"="iso3c")) 


pal_early_day <- colorNumeric(palette = "plasma",
                    domain = lat_lon_early_day$confirmed)
lat_lon_early_day %>% leaflet() %>%
  addTiles() %>% addCircles(lng = ~Long,
                            lat = ~Lat,
                            radius = ~ confirmed*0.05, # unfortunately leaflet does not automatically 
                            popup = ~ country,
                            color = ~ pal_early_day(confirmed),
                            fillOpacity = 0.5,
                            stroke = FALSE) %>%
  addLegend("bottomright", pal = pal_early_day, values = ~ confirmed, title = "Cases",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.5)
#################Q2
library(mapproj)
library(maps)
library(countrycode)

athaliana <-read.table("~/ppdata/swedishAthaliana.txt", sep="\t", head=T) # not that for geom_path the corrdinates need to be ordered
# so that the correct points are being connected
world_map <- map_data("world") %>% 
  filter(region != "Antarctica")%>% 
  mutate(iso3c = countrycode(region,
                             origin = "country.name",
                             destination = "iso3c"), 
         continent = countrycode(iso3c,
                                 origin ="iso3c",
                                 destination="continent")) 

scandinavia <-c ("Sweden", "Finland", "Norway", "Denmark") 
map.scan <- world_map %>% filter( region %in% scandinavia )
library(ggplot2)    
library(ggmap)

map.scan %>% arrange(group,order) %>%
  ggplot(aes(x=long, y=lat))+
  geom_path(aes(group=group))+
  ylim(54,71)+
  xlim(5,31)+
  geom_jitter(data=athaliana,aes(x=Longitude, y=Latitude), color="darkgreen", alpha=0.02)+
  coord_map(projection = "mercator")+
  theme_void()
    
    
map.scan %>% arrange(group,order) %>%
  ggplot(aes(x=long, y=lat))+
  geom_path(aes(group=group))+
  ylim(54,71)+
  xlim(5,31)+
  geom_hex(data = athaliana, mapping = aes(x = Longitude, y = Latitude), bins = 30)+
  theme_void()


