library(tidyverse)
library(skimr)
library(ggplot2)

# import data
rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

#load fonts with sysfonts package
sysfonts::font_add_google("Merriweather")

sysfonts::font_add_google("Fuzzy Bubbles")
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
  arrange(desc(count)) 

rent_year %>% 
  group_by(year, nhood) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))
#be careful group_by something already summarised, 
#this will tell you how many different "beds" in one nhood, not listing 

top10_nhood <- rent_year %>%
  group_by(nhood) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  slice_head(n = 10) %>%
  pull(nhood)
#this is each nhood how many rows (year x beds)
#so we'll take first 10 row, since we'd like to plot by nhood, x axis as time scale, facet beds
#you should always ungroup() before taking top something
# you should also check how many beds are there

skim(rent_year)
#there's 12 beds? weird



listing_beds <- rent %>% 
  group_by(year, beds, nhood) %>% 
  count() %>%  
  ungroup() %>% 
  filter(nhood %in% top10_nhood) %>% 
  drop_na() 
#different beds has different number of listings,

listing_beds %>% 
 # filter(beds==1) %>% 
  ggplot(aes(x = year, y = n))+
  geom_line(aes(color = nhood))+
  facet_wrap(~beds)+
 # theme_minimal()+
  labs(title = "Rental listings in Bay Area: 2000-2018",
       subtitle= "(0 - 9 bedrooms)",
       caption = "Data from Pennington, Kate (2018). \nBay Area Craigslist Rental Housing Posts, 2000-2018.",
       x= "year",
       y= "Posts count",
       color = "neighborhood")+
  theme(
    strip.background = element_rect(fill = "#f4c2c2"),
    panel.grid.minor = element_blank(),
    text = element_text(family = "Merriweather", size = 10),
    plot.title = element_text(family = "Fuzzy Bubbles", size = 25, hjust = 0.5),
    plot.subtitle = element_text(family = "Fuzzy Bubbles", size = 18, hjust = 0.5),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 18)
  )
#anything above 4beds is kinda messy  

#so we take no more than 4 beds data
rent_year_top10 <- rent_year %>% 
  filter(nhood %in% top10_nhood) %>% 
  filter(beds <= 4)


rent_plot1 <- 
  rent_year_top10  %>% 
  ggplot()+
  geom_line(aes(x = year, y = median, color= nhood), size = 1) +
  facet_wrap(~beds, scales = "free")+
  theme_minimal()+
  labs(title = "Rents in Bay Area: 2000-2018",
       subtitle = "(0 - 4 beds)",
       caption = "Data from Pennington, Kate (2018). \nBay Area Craigslist Rental Housing Posts, 2000-2018.",
       x= "year",
       y= "Median price (dollars)",
       color = "Neighborhood")+
  theme(
    strip.background = element_rect(fill = "#f4c2c2"),
    panel.grid.minor = element_blank(),
    text = element_text(family = "Merriweather", size = 10),
    plot.title = element_text(family = "Fuzzy Bubbles", size = 25, hjust = 0.5),
    plot.subtitle = element_text(family = "Fuzzy Bubbles", size = 18, hjust = 0.5),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 18)
  )


rent_year_top10 %>% 
  filter(beds==4) %>% 
  arrange(desc(median))
# there are indeed some crazy listings

# #plot1_text <- data.frame(
#   label = c("","","","",
#             "Different price range (y axis) \nwas set for each plot \nto better visualize the trend."),
#   beds = c(0,1,2,3,4)
# )
#below works the same, but better, because if you have y set 8000, it'll change y limit for all plots
plot1_text <- data.frame(
  label = "Different price range (y axis) \nwas set for each plot \nto better visualize the trend.",
  beds = 4
)

#rent_plot1_text <- 
  rent_plot1 + geom_text(
    # inherit.aes = F,
    data = plot1_text,
    aes(x= 2022, y = 8000, label = label),
    #color = "black",
    family = "Fuzzy Bubbles",
    hjust = 0,
    size = 3)+
  coord_cartesian(xlim = c(2000,2018), # This focuses the x-axis on the range of interest
                  clip = 'off')   # This keeps the labels from disappearing
    
  
    
#mind where you drop_na, maybe dropping whole rows for some columns you don't need
rent %>%
  group_by(year, nhood, beds) %>%
  drop_na() %>%
  summarise(mean = mean(price),
            count = n(),
            .groups = "drop") %>%
  arrange(desc(count))





