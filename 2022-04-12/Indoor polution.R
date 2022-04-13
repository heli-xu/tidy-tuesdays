library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2022-04-12')

indoor_pollution <- tuesdata$indoor_pollution

fuel_gdp <- tuesdata$fuel_gdp

death_timeseries <- tuesdata$death_timeseries

##6 dataframe in the list, some overlapping
##indoor pollution has death rate as a percentage
##just realized continent is among the "Entity" with the countries
##so we'll just look at death rate across 30 years in different continent

indoor_pollution_cont <- indoor_pollution %>% 
  filter(Entity%in% c("Europe","Asia","Oceania","Africa",
                      "North America","South America")) %>% 
  rename(death_rate= 4)
         
# 4 is column number, or you can start typing 'death' and it'll auto complete
#`Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Percent)`)

indoor_pollution_cont %>% 
  ggplot(aes(x=Year, y=death_rate), color=Entity)+
  geom_line(aes(color=Entity))+
  theme_bw()+
  labs(title = "Death related to indoor pollution from 1990-2019",
       y="% death of all causes",
       caption="TidyTuesday week 15")+
  scale_color_discrete(name ='Continent')

##Oceania weirdly high, with Australia and New Zealand very low

fuel_gdp %>% 
  filter(Entity%in% c("Europe","Asia","Oceania","Africa",
                      "North America","South America")) %>% 
  filter(Year>1989)
##why is there negative in YEar??? and 100, 200???
##maybe change line thickness based on population?

 