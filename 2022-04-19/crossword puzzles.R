{ # 0. Setup ----
  ## Load Dependencies
  library(tidyverse)
  library(ggplot2)
  library(skimr)
  library(forcats) ## very useful for ordering categorical variables with ggplot
  
  ## Load Data
  tuesdata <- tidytuesdayR::tt_load('2022-04-19')
  big_dave <- tuesdata$big_dave
  times <- tuesdata$times
  
  ## EDA
  skim(big_dave)
  head(big_dave)
  head(times)
  skim(times)
  
  ###barely done any crossword puzzles before..not much interest
  ###why would people want to compile this data indeed?
  ##I assumed it's good to practice str detect with 
  ##took me a while to figure out why is there two datasets..just source difference I guess
  ##saw people doing letter# in the answer, the more letters the more difficult
  #I'm gonna try to see the most popular words in puzzles in different puzzles?...?how is that useful idk
  ## YES FOR COMMENTS. RL
}
{ # 1. Data ----
  ans_pct_dave <- big_dave %>% 
    mutate(puzzle_name=str_replace(puzzle_name, "[^[a-zA-Z]]","") %>% str_extract(., "[a-zA-Z]+")) %>% 
    ##remove random dashes!!! and numbers to keep only newspaper name
    ##this took me better part of the day! potentially str_remove [^[a-zA-Z]]
    group_by(puzzle_name, answer) %>% 
    count() %>% 
    rename(count=n) %>% 
    ungroup() %>% 
    drop_na() %>% 
    group_by(puzzle_name) %>% 
    mutate(total_ans=sum(count),
           pct_occr=count/total_ans*100) %>% 
    ungroup() 
  #arrange(desc(pct_occr))
  ##sometimes because total count is low, high pct_occr could be count of 1
  
  ##actually took a few seconds to run
  
  ##to look at highest in each newspaper
  
  max_anw_dave <- ans_pct_dave %>% 
    group_by(puzzle_name) %>% 
    filter(str_length(answer)>1,
           count>2,
           count == max(count)) %>% 
    ungroup()
  
  ##newspaper with a bunch of 1s can't be filtered out the max!!!
  ##originally only going to do big_dave, but repeated ones are so rare...
  
  ans_pct_times <- times %>% 
    mutate(puzzle_name=str_remove(puzzle_name, "[^[a-zA-Z]]+") %>% str_extract(., "[a-zA-Z]+")) %>% 
    ##remove random dashes!!! and numbers to keep only newspaper name
    ##this took me better part of the day! potentially str_remove [^[a-zA-Z]]
    group_by(puzzle_name, answer) %>% 
    count() %>% 
    rename(count=n) %>% 
    ungroup() %>% 
    drop_na() %>% 
    group_by(puzzle_name) %>% 
    mutate(total_ans=sum(count),
           pct_occr=count/total_ans*100) %>% 
    ungroup() 
  
  max_ans_times <- ans_pct_times %>% 
    group_by(puzzle_name) %>% 
    filter(str_length(answer)>1, ##single letters turn out to be answers a lot
           count>2,
           count==max(count)) %>% 
    ungroup()
  
  max_ans <- bind_rows(max_anw_dave, max_ans_times) 
  
}
{ # 2. Plot ----
  
  { # 2.1 Heli ----
    ggplot(max_ans, aes(x=puzzle_name, y=count), fill=puzzle_name)+
      geom_col(position = "identity")
  }
  
  { # 2.2 Ran ----
    max_ans %>% 
      ggplot(aes(y= fct_reorder(puzzle_name, count), x=count, fill = puzzle_name))+
      geom_col(position = "identity")+
      theme_bw()+
      theme(legend.position = 'none')
  }
  
}