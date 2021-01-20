load("Data/Data.rda")
library(tidyverse)
library(magrittr)
library(broom)
library(lm.beta)
library(lfe)

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
  ) %>% 
  rownames_to_column()



# Adding ADT residuals after substracting Ravens
resids = d %>% lm(ADT ~ Ravens,.) %>% 
  resid %>% enframe() %>% rename(rowname = name, ADTresid = value)

d = left_join(d,resids)

# Running FELM model
models = d %>% 
  gather(Years,Graduated,-rowname,,-school,-c(Grit:ADTresid)) %>%  
  gather(Outcome, Value, -c(rowname,school,Years,Graduated)) %>% 
  group_by(school,Years,Outcome) %>% 
  arrange(school) %>% 
  mutate(Grad_s = mean(Graduated)) %>% 
  mutate(Grad_d = Graduated - Grad_s,
         school_n = n(),
         Grad_sum = Grad_s*school_n) %>% 
  ungroup() %>% 
  mutate(Grad_ex = (Grad_sum-Graduated)/(school_n-1)) %>% 
  group_by(Years,Outcome) %>% 
  nest() %>% 
  mutate(lm = map(data,~lm(Value ~ Graduated + Grad_ex + factor(school),data=.)),
         lmb = map(lm,lm.beta::lm.beta),
         tidy = map(lmb,tidy),
         glance = map(lmb,glance),
         felm = map(data,~felm(Value ~ Graduated + Grad_ex | 0 | 0 | school,data=.)))


felm = models$felm[[1]] %>% tidy
for (i in 2:10){
  felm = rbind(felm,models$felm[[i]] %>% tidy)
}
felm = cbind(models[rep(seq_len(nrow(models)), each=3),1:2],felm)
felm %>% 
  mutate_at(4:6,round,2) %>% 
  mutate_at(7,round,3) %T>% 
  Ben::write.clip()

# Running FELM model, controlling for school
models2 = d %>% 
  gather(Years,Graduated,-rowname,,-school,-c(Grit:ADTresid)) %>%  
  gather(Outcome, Value, -c(rowname,school,Years,Graduated)) %>% 
  group_by(school,Years,Outcome) %>% 
  arrange(school) %>% 
  mutate(Grad_s = mean(Graduated)) %>% 
  mutate(Grad_d = Graduated - Grad_s,
         school_n = n(),
         Grad_sum = Grad_s*school_n) %>% 
  ungroup() %>% 
  mutate(Grad_ex = (Grad_sum-Graduated)/(school_n-1)) %>% 
  group_by(Years,Outcome) %>% 
  nest() %>% 
  mutate(lm = map(data,~lm(Value ~ Graduated + Grad_ex + factor(school),data=.)),
         lmb = map(lm,lm.beta::lm.beta),
         tidy = map(lmb,tidy),
         glance = map(lmb,glance),
         felm = map(data,~felm(Value ~ Graduated + Grad_ex | factor(school) | 0 | school,data=.)))


felm2 = models2$felm[[1]] %>% tidy
for (i in 2:10){
  felm2 = rbind(felm2,models$felm[[i]] %>% tidy)
}
felm2 = cbind(models[rep(seq_len(nrow(models)), each=3),1:2],felm2)
felm2 %>% 
  mutate_at(4:6,round,2) %>% 
  mutate_at(7,round,3) %T>% 
  Ben::write.clip()

#No clusterred errors
models %>% 
  unnest(tidy) %>% 
  select(Years,Outcome, term,std_estimate,std.error,p.value) %>% 
  filter(term %in% c('Graduated','Grad_ex')) %>% 
  #Just keep formated estimates
  mutate(beta = Ben::formatest(std_estimate,p.value)) %>% 
  select(Years,Outcome,term,beta) 
  
#No Clustered errors
models %>% 
  unnest(tidy) %>% 
  filter(term  %in% c( "Grad_ex")  ,Outcome != "Ravens") %>% 
  ggplot(aes(Outcome,std_estimate,shape = p.value<.05,col = term))+
  geom_point(size = 3,position = position_dodge(width = .55))+
  geom_line(aes(group = Years))+
  facet_wrap(~Years)+
  labs(title = "Do your classmates graduation rate affect how you rate your grit",
       x = 'Outcome',
       y = 'Beta')+
  Ben::theme_ang()




