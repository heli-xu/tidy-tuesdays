library(tidyverse)
library(janitor)

tuesdata <- tidytuesdayR::tt_load('2022-04-12')

indoor_pollution <- tuesdata$indoor_pollution

fuel_gdp <- tuesdata$fuel_gdp

death_timeseries <- tuesdata$death_timeseries

##6 dataframe in the list, some overlapping
##indoor pollution has death rate as a percentage
##just realized continent is among the "Entity" with the countries; tried "filter", some missing info
###ACTUALLY, there's a code to add continent mutate(continent = countrycode::countrycode(Code, "iso3c", "continent"))

##so we'll just look at death rate across 30 years in different continent

death_gdp <- indoor_pollution %>% 
  left_join(fuel_gdp,by = c("Entity","Code","Year")) %>% 
  clean_names() %>% 
  mutate(continent=countrycode::countrycode(code, "iso3c", "continent")) %>% 
  rename(death_rate= 4,
         gdp = 6) %>% 
  drop_na()
         
# 4 is column number, or you can start typing 'death' and it'll auto complete
#`Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Percent)`)

death_gdp %>% 
  filter(year %in% c(2000,2016)) %>% 
  ggplot(aes(x=continent, y=death_rate), fill=entity)+
  geom_point(aes(size=gdp, color=continent))+
  theme_bw()+
  facet_wrap(~year)
  labs(title = "Death related to indoor pollution in 2000 vs 2016",
       y="% death of all causes",
       caption="TidyTuesday week 15")+
  scale_color_discrete(name ='Continent')



fuel_gdp %>% 
  filter(Entity%in% c("Europe","Asia","Oceania","Africa",
                      "North America","South America")) %>% 
  filter(Year>1989)
##why is there negative in YEar??? and 100, 200???
##maybe change line thickness based on population?

 