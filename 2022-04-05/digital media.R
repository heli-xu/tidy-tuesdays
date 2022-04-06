library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2022-04-05')
news_orgs <- tuesdata$news_orgs

ad_rev_budget <- news_orgs %>% 
  group_by(budget_percent_editorial, revenue_stream_largest) %>% 
  count() %>% 
  group_by(budget_percent_editorial) %>% 
  mutate(pct_rev=n/sum(n)*100) %>% 
  filter(revenue_stream_largest=="Direct sold advertising") %>% 
  drop_na()

ggplot(ad_rev_budget, aes(x=budget_percent_editorial, y= pct_rev))+
  geom_col(position = "identity")+
  theme_bw()+
  labs(title = "Direct sold advertising vs Editorial budget",
       subtitle="Direct sold advertising as largest revenue stream \n among publications with different budget for editorial",
       y="% of largest revenue stream")
