# Author: Andrew Nalundasan
# For: OMSBA 5300, Seattle University
# Date: 2/22/2021
# Week 7 class materials (Difference in Differences)

library(tidyverse)

# DID Example from slides: 
df <- read.csv('http://nickchk.com/eitc.csv') %>%
  mutate(after = year >= 1994,
         treated = children > 0)
df %>% 
  group_by(after, treated) %>%
  summarize(proportion_working = mean(work))

means <- df %>% 
  group_by(after, treated) %>%
  summarize(proportion_working = mean(work)) %>%
  pull(proportion_working)
(means[4] - means[2]) - (means[3] - means[1]) # = 0.047

# regress
# after*treated results in 'after', 'treated' and 'after*treated'
lm(work ~ after*treated, data = df) %>%
  export_summs(digits = 3)

# quick example data to show this, where first treated period == period 7
# and treated groups being 1 and 9, and a true effect of 3
did_data <- tibble(group = sort(rep(1:10, 10)),
                   time = rep(1:10, 10)) %>%
  mutate(CurrentlyTreated  = group %in% c(1,9) & time >= 7) %>%
  mutate(Outcome = group + time + 3*CurrentlyTreated + rnorm(100))
did_data

# look at results: 
lm_robust(Outcome ~ CurrentlyTreated, fixed_effects = ~group + time, data = did_data) %>%
  export_summs(statistics = c(N = 'nobs'))