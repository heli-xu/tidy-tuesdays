library(tidyverse)
library(skimr)
library(ggplot2)
#library(tidygeocoder)
library(leaflet)
library(sf)

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

# set color palette for circles
pal <- colorNumeric(
  palette = c("orange","navy"),
  domain = rent_geo_2015$price
)

# map for 2015, 1 bed
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

# creating a function to look at different year/bed  
# given you have data rent_geo
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


# get zip code info from shapefile
unzip("Bay Area Counties.zip",
      exdir = "bay_county_shapefile", junkpaths = T,
      overwrite = F)

bay_county <- st_read("bay_county_shapefile/geo_export_62cf50c7-6bad-453b-a1f1-701b46d43fd2.shp")
# Geodetic CRS: WGS84 (its EPSG code = 4236)


##very important
##generate a column "geometry" based on lat and lon that can be recognized later on
rent_geometry <- st_as_sf(rent_geo,
                         coords = c(x = "lon", y = "lat"),
                         remove = FALSE, #keep the coords columns for leaflet
                         crs = st_crs(4326)) ##matching your shp file

# adding zipcode to each point in data (based on "geometry")
rent_geo_county <- st_join(rent_geometry, bay_county, join = st_within)

#define epsg code if it's not defined
# st_crs(x)$epsg --> NA in this case
# st_crs(x)$proj4string  --> "+proj=longlat +ellps=WGS84 +no_defs"
# CRS may have both but may only need one,
# for joining, you need matching crs code, so if undefined, need assign manually
st_crs(bay_county) <- 4326
#then join (as above)



skim(rent_geo_county) #mostly looking at the generated zip column

rent_geo_county2 <- rent_geo_county %>% 
  drop_na(county) %>% 
  group_by(county, year, beds) %>% 
  summarise(median = median(price),
            count = n(),
            .groups = "drop") %>% 
  ungroup() %>% 
  left_join(bay_county, by = "county") %>% 
  st_as_sf() %>% 
  filter(year == 2015,
         beds == 1)

##looking at 2015, 1 bed

pal2 <- colorNumeric(
  palette = c("orange", "navy"),
  domain = rent_geo_county2$median
)

library(glue)

county_label = glue("<h3 style='margin: 0px'>{rent_geo_county2$county}</h3>
                    Median rent $: {rent_geo_county2$median}") %>%
  map(~HTML(.x))

leaflet(rent_geo_county2) %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  addPolygons(color = "white", 
              weight = 2,
              smoothFactor = 0.5,
              opacity = 1,
              fillColor = ~pal2(median),
              fillOpacity = 0.8,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.8,
                bringToFront = TRUE),
              label = county_label,
              labelOptions = labelOptions(
                style = list(
                  "font-family" = "Fira Sans, sans-serif",
                  "font-size" = "1.2em"
                ))
              ) %>% 
  addLegend("bottomleft",
            pal = pal2,
            values = ~median,
            title = "Rent \n(2015, 1 bedroom)",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1)

    