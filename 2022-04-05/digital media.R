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
       y="% publications in each tax status")


