library(tidyverse)
library(skimr)
library(ggplot2)
library(tidygeocoder)
library(leaflet)

# import data
rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

skim(rent)

# select the postings with lat/long/addre
rent_geo <- rent %>% 
  select(post_id, address, lat, lon, price, beds, year, details) %>% 
  drop_na(lat, lon) 
  #group_by(year) %>% 
  #count()  #this is to look at how many listings are there each year

rent_geo_2015 <- rent_geo %>% 
  filter(year==2015,
         beds == 1)


# actuallly don't geocode until examine address, they have to be right format
#and this dataset happens to have very bad address info, incomplete
# rent_geo_diy <- rent_geo %>% 
#   geocode(address = address,
#           method = 'arcgis',
#           long = lon,
#           lat = lat)

skim(rent_geo)

pal <- colorNumeric(
  palette = c("orange","navy"),
  domain = rent_geo_2015$price
)

leaflet(data = rent_geo_2015 %>% arrange(price)) %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  #add map markers
  addCircles(lat = ~lat,
             lng = ~lon,
             color = ~pal(price),
             label = ~price,
             stroke = T,
             weight = 10,
             fillOpacity = 0.2) %>% 
  addLegend("bottomright",
            pal = pal,
            values = ~price,
            title = paste("Rent", "(2015)"),
            labFormat = labelFormat(prefix = "$"),
            opacity = 1) %>% 
  setView(lng = mean(rent_geo_2015$lon),
          lat = mean(rent_geo_2015$lat),
          zoom = 8)
             
map_rent_year_beds <- function(year, beds){
  rent_geo_map <- rent_geo %>% 
    filter(year==year,
           beds == beds) %>% 
    arrange(price)
  
  pal <- colorNumeric(
    palette = c("orange","navy"),
    domain = rent_geo_map$price
  ) 
  
  leaflet(data = rent_geo_map) %>% 
    addProviderTiles(providers$CartoDB.Voyager) %>% 
    addCircles(lat = ~lat,
               lng = ~lon,
               color = ~pal(price),
               label = ~price,
               stroke = T,
               weight = 10,
               fillOpacity = 0.2) %>% 
    addLegend("bottomright",
              pal = pal,
              values = ~price,
              title = paste0("Rent"," (", year,")"),
              labFormat = labelFormat(prefix = "$"),
              opacity = 1) %>% 
    setView(lng = mean(rent_geo_map$lon),
            lat = mean(rent_geo_map$lat),
            zoom = 8)
}


map_rent_year_beds(2010, 1)


## graveyard code
leaflet(data = quakes[1:20,]) %>% 
  addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))

leaflet(data = slice(rent_geo,1:50)) %>% 
  addTiles() %>%
  addCircles(~lon, ~lat)

