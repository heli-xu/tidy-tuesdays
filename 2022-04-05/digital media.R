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

ggplot()