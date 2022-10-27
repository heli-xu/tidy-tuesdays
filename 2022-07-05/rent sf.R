library(tidyverse)
library(skimr)
library(ggplot2)

# import data
rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

#load fonts with sysfonts package
sysfonts::font_add_google("Merriweather")

sysfonts::font_add_google("Fuzzy Bubbles")
sysfonts::font_add_google("Oswald")
#to automatically render fonts, use showtext_auto()
showtext::showtext_auto()




## check data
skim(rent)
# complete rate and value range for each column

rent_year <- rent %>%
  select(post_id, year, nhood, beds, price) %>%
  drop_na() %>%
  group_by(year, nhood, beds) %>%
  summarise(median = median(price),
            count = n(),
            .groups = "drop") %>%
  ungroup() %>% 
  filter(count>=10) %>% #added later on, single listing in some year for a nhood really shows off the viz
  arrange(desc(count)) 

skim(rent_year)
#there's 12 beds? weird

rent_year %>% 
  group_by(year, nhood) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))
#be careful group_by something already summarised, 
#this will tell you how many different "beds" in one nhood, not listing count 



top10_nhood_allyear <- rent_year %>%
  group_by(nhood)%>%
  summarise(sum=sum(count)) %>% 
  arrange(desc(sum)) %>%
  ungroup() %>%
  slice_head(n = 10) %>%
  pull(nhood)
#this is each nhood how many listings (count from rent_year)
#do not count() again, that'll give how many rows in each condition, but will lose the listing count
#so we'll take first 10 row, since we'd like to plot by nhood, x axis as time scale, facet beds
#you should always ungroup() before taking top something
# you should also check how many beds are there



#### PLOT of listings ####

color_pal_10 <- c("#003a52", "#005f73", "#0a9396", "#94d2bd", "#b4922d", 
                  "#d16a19", "#f1887e", "#9a2672", "#d3363b", '#792701')

listing_beds <- rent_year %>% 
  filter(nhood %in% top10_nhood_allyear)
#different beds has different number of listings,

skim(listing_beds)

listing_beds %>% 
  filter(beds==6)
#there's only data in one year..not much use 

#adding a facet label
beds_label <- c("studio","1 beds", "2 beds", "3 beds", "4 beds", "5 beds")
names(beds_label) <- c("0","1","2","3","4","5")



listing_beds %>%
  filter(beds<=5) %>% 
  #filter(beds==4) %>% 
  ggplot(aes(x = year, y = count))+
  geom_line(aes(color = nhood), size=0.8)+
  scale_color_manual(values = color_pal_10)+
  theme_minimal()+
  facet_wrap(~beds,
             labeller = labeller(beds = beds_label))+
 # theme_minimal()+
  labs(title = "Rental listings in Bay Area: 2000-2018",
       subtitle= "10 neighborhood with most listings",
       caption = "Data viz @Heli_Xu\n Data source: Data from Pennington, Kate (2018). \nBay Area Craigslist Rental Housing Posts, 2000-2018.",
       x= "year",
       y= "Posts count",
       color = "neighborhood")+
  theme(
    strip.background = element_rect(fill = "#f4c2c2", color = 'white'),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(family = "Merriweather", size = 13),
    plot.title = element_text(family = "Oswald", size = 16, hjust = 0, face = "bold"),
    plot.subtitle = element_text(family = "Oswald", color = "#4D4948", size = 14, hjust = 0),
    plot.caption = element_text(color = "#EE7060", size = 8),
    axis.title = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 13)
  )
#anything above 4beds is kinda messy  

#### PLOT for price change ####
##1. base plot

#so we take no more than 4 beds data 
rent_year_top10 <- rent_year %>% 
  filter(nhood %in% top10_nhood_allyear) %>% 
  filter(beds <= 4)


#adding a facet label
# beds_label <- c("studio","1 beds", "2 beds", "3 beds", "4 beds")
# names(beds_label) <- c("0","1","2","3","4")
# named string can be longer than actual faceted param.

rent_plot1 <- 
  rent_year_top10  %>% 
  ggplot()+
  geom_line(aes(x = year, y = median, color= nhood), size = 1) +
  scale_color_manual(values = color_pal_10)+
  facet_wrap(~beds, scales = "free",
             labeller = labeller(beds = beds_label))+
  theme_minimal()+
  labs(title = "Rising Rents in Bay Area (2000-2018)",
       subtitle = "10 Neighborhoods with most listings on Craigslist",
       caption = "Data viz @Heli_Xu | #tidytuesday \nData from Pennington, Kate (2018). \nBay Area Craigslist Rental Housing Posts, 2000-2018.",
       x= "year",
       y= "Median price (dollars)",
       color = "Neighborhood")+
  theme(
    strip.background = element_rect(fill = "#f4c2c2", color = 'white'),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(family = "Merriweather", size = 13),
    plot.title = element_text(family = "Oswald", size = 16, hjust = 0, face = "bold"),
    plot.subtitle = element_text(family = "Oswald", color = "#4D4948", size = 14, hjust = 0),
    plot.caption = element_text(color = "#EE7060", size = 8),
    axis.title = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 13)
  )


rent_year %>% 
  filter(year==2007, beds==4) %>% 
  arrange(desc(median))
# there are indeed some crazy listings
# for 2007, SOMA/south beach only has one 4-bed listing but price is crazy
# I want to filter the nhood that have more than 50 listing in all years
##going back to adjust rent_year


##2.  add annotation
# #plot1_text <- data.frame(
#   label = c("","","","",
#             "Different price range (y axis) \nwas set for each plot \nto better visualize the trend."),
#   beds = c(0,1,2,3,4)
# )
#below works the same, but better, because if you have y set 8000, it'll change y limit for all plots

plot1_text <- data.frame(
  label = "Most expensive areas are \nSOMA/south beach \n(esp. compact types) \n& mountain view/palo alto.",
  beds = 4
)

rent_plot1_text <- 
  rent_plot1 + geom_text(
    # inherit.aes = F,
    data = plot1_text,
    aes(x= 2022, y = 5000, label = label),
    color = "#4D4948",
    family = "Merriweather",
    hjust = 0,
    size = 4)+
  coord_cartesian(xlim = c(2000,2018), # This focuses the x-axis on the range of interest
                  clip = 'off')   # This keeps the labels from disappearing
    

##3. add arrows
seg1 <- data.frame(
  x= 2030, y= 7000, xend=2030, yend=8000,
  beds = 4
)

curve1 <- data.frame(
  x= 2025, xend=2022, y=7000, yend=7500,
  beds = 4
)
#rent_plot1_text_arrow <- 
  rent_plot1_text + 
    geom_segment(
    data = seg1, aes(x=x, y=y, xend=xend, yend=yend),
    inherit.aes = FALSE,
    color="#d3363b",
    size=1.2,
    arrow = arrow(length = unit(0.1,"inch")))+
    geom_curve(
      data = curve1, aes(x=x, y=y, xend=xend, yend=yend),
      inherit.aes = FALSE,
      color= "#003a52",
      curvature = 0.5,
      size=1.2,
      arrow = arrow(length = unit(0.1,"inch"))
    )

  
plotly::ggplotly(rent_plot1)  
#mess some themes up, but cool features to interactively look at plots








