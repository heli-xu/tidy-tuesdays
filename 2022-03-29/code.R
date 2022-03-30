## Get data
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load('2022-03-29')[[1]]


#How much money is made and spent between men and women
#check with arrange(desc(year))to see most recent year
tuesdata_2019 <- tuesdata %>% 
  select(year,institution_name, sports,sum_partic_men, sum_partic_women,
         rev_women,rev_men, total_rev_menwomen,
         exp_men, exp_women, total_exp_menwomen) %>%
  filter(year=="2019")

tuesdata_2019_all_school <- tuesdata_2019 %>% 
  group_by(sports) %>% 
  summarise(all_partic_men=sum(sum_partic_men),
        all_partic_women=sum(sum_partic_women),
         all_exp_men=sum(exp_men,na.rm = T),
         all_exp_women=sum(exp_women, na.rm = T),
         all_rev_men=sum(rev_men, na.rm = T),
         all_rev_women=sum(rev_women, na.rm = T))

tuesdata_2019_all_school %>% 
  mutate(exp_per_men=all_exp_men/all_partic_men,
         exp_per_women=all_exp_women/all_partic_women)
