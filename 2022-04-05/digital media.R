library(tidyverse)
library(MetBrewer)

tuesdata <- tidytuesdayR::tt_load('2022-04-05')
news_orgs <- tuesdata$news_orgs


##originally wanted to do revenue from direct sold advertising vs editorial budget
##but the number of publications with advertising as largest revenue, 
##out of the number of publications with other largest revenue stream doesn't make sense
##Below idea from @Mitsuoxv on twitter, editorial budget vs for/non profit

tax_budget <- news_orgs %>% 
  group_by(budget_percent_editorial, tax_status_current) %>% 
  count() %>% 
  ungroup() %>% 
  drop_na() %>%
  mutate(profit = if_else(tax_status_current %in% c("For Profit", "LLC", 
                                                    "Partnership", "S Corp", 
                                                    "Sole Proprietor/no specific tax status"), 
                          "For Profit", "Non-Profit")) %>%
  group_by(profit) %>% 
  mutate(total_pubs=sum(n),
         pct_pubs=n/total_pubs*100)
  

ggplot(tax_budget, aes(x=budget_percent_editorial, y= pct_pubs, fill=profit))+
  geom_col(position = "dodge")+
  theme_bw()+
  scale_fill_manual(name = "current tax status",
                    values = met.brewer("Navajo",2))+
  labs(title = "Editorial budget in For/Non-Profit Publications",
       x = "% budget for editorial",
       y="% publications in each tax status",
       caption = "TidyTuesdays week 14 2022")


##Ben burnley's plot, year vs number publication, color by tax status
##histogram quite cool this way, no need to count()
##some fonts and theme stuff to make plot pretty at the end (try with and without)
ggplot(data = news_orgs, aes(x = year_founded, fill = tax_status_current))+
  geom_histogram(position = "stack")+
  scale_fill_manual(values = met.brewer("Hiroshige", type = "discrete", direction = -1), name = "Tax Status")+
  labs(x = "Year Founded",
       y = "Number of Publications Founded",
       title = "The Rise and Fall of Local News Online",
       subtitle = "The plot below shows the number of digitally focused, local news organizations started in each year.\nPublications started prior to the internet are included if they are now digital-native.",
       caption = "Source:  Project Oasis and UNC Hussman School of Journalism | #TidyTuesday W14 2022 | @ben_burnley")+
  theme_minimal()+
  theme(
    text = element_text(family = "mono"),
    plot.title = element_text(size = 36),
    plot.caption = element_text(hjust = 1),
    plot.subtitle = element_text(size = 10, hjust = 0),
    legend.position = c(.15,.6),
    legend.background = element_rect(color = "gray"),
    plot.background = element_rect(fill = "#F5F5F5"))

###trying out some tips on twitter, change background color in ggsave!!

ggsave(filename = "plots by benburnley.png", height = 10, width = 9, 
       bg = "#a7c3cf", dpi = 400)

    
    
    
