library(tidyverse)
library(ggplot2)
library(MetBrewer)

tuesdata <- tidytuesdayR::tt_load('2022-03-29')

sports_rev <- tuesdata[[1]] %>% 
  group_by(sports) %>% 
  mutate(diff_rev=rev_men-rev_women) %>% 
  summarise(diff_mean=mean(diff_rev, na.rm = T)) %>% 
  mutate(pos = diff_mean >= 0) %>% 
  na.omit()

ggplot(sports_rev, aes(x=diff_mean, y = reorder(sports_rev, -diff_mean), fill = pos))+
  geom_col(position = "identity") +
  scale_x_continuous(labels = scales::comma) +
  theme_bw() +
  theme(text=element_text(family="Merriweather"),
        axis.text = element_text(size = 12),
        legend.position = "top",
        legend.justification = "left",
        plot.caption.position = "plot",
        plot.caption=element_text(hjust=0, size=8.3, color="grey20"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=12),
        plot.margin = margin(t = 20,
                             r = 50,
                             b = 40,
                             l = 20))+
  scale_fill_manual(name = "",
                    labels = c("Women's team makes more","Men's team makes more"),
                    values = met.brewer("Greek",2))+
  labs(x="Annual Revnue Difference (in USD)",
       y="",
       caption = "\n#TidyTuesday week 13",
       title = "College revnue differential for sports that have both men's and women's teams")
