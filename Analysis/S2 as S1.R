load("Data/Data.rda")
library(tidyverse)
library(magrittr)

d = refbias.charter

# Strudent i, school s, year t
# Grit_IST = Achievement_ist + achievement_others_ist+ school_s + year_t + error_i,
# b-ist, where -i indicates the average academic performance of students sharing a school and class with each student i, exclusive of student i 


d = d %>% 
  select(
    school = schoolID,
    Grad4 = GRAD_Any_in_4YRS,
    Grad6 = GRAD_Any_in_6YRS,
    Grit = grit.c.Z,
    SelfControl = sc.c.Z,
    ADT = Tmath.c.Z,
    Ravens = ravens.strict.c.Z
  )

# Calculating the school average excluding one person.
d %>% 
  group_by(school) %>% 
  arrange(school) %>% 
  mutate(Grad_s = mean(Grad4)) %>% 
  mutate(Grad_d = Grad4 - Grad_s,
         school_n = n(),
         Grad_sum = Grad_s*school_n) %>% 
  ungroup() %>% 
  mutate(Grad_ex = (Grad_sum-Grad4)/(school_n-1)) %>% 
  lm(formula = Grit~ Grad4 + Grad_ex+factor(school)) %>% 
  lm.beta::lm.beta() %>% 
  summary()
         
