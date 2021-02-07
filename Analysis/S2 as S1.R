load("Data/Data with GPA 210128.rda")
library(tidyverse)
library(magrittr)
library(broom)
library(lm.beta)
library(lfe)
library(psych)

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
    Ravens = ravens.strict.c.Z,
    SAT = SAT_meanTotScore_1600_new,
    GPA = GPA_all_sen
  ) %>% 
  rownames_to_column()



# Adding ADT residuals after substracting Ravens
resids = d %>% lm(ADT ~ Ravens,.) %>% 
  resid %>% enframe() %>% rename(rowname = name, ADTresid = value)

d = left_join(d,resids)

d %>% select(-rowname) %>% 
  select(Ravens,Grad4,Grad6,SAT,GPA) %>% Ben::HARcor()


d %>% select(-rowname) %>% 
  select(Ravens,Grad4,Grad6,SAT,GPA) %>% 
  mutate(Grad4 = as.factor(Grad4),Grad6 = as.factor(Grad6)) %>% 
  polycor::hetcor() %>% 
  `$`(correlations) %>% 
  psych::principal(nfactors = 2) %>% 
  print.psych(cut = .4,sort=T,rotate="varimax")

# Calculating Variables and nesting data ####
nesteddata = 
  d %>% 
  gather(Predictor,PValue,-rowname,,-school,-c(Grit:ADT,ADTresid)) %>%  
  gather(Outcome, OValue, -c(rowname,school,Predictor,PValue)) %>% 
  group_by(school,Predictor,Outcome) %>% 
  arrange(school) %>% 
  mutate(Pred_s = mean(PValue,na.rm=T)) %>% 
  mutate(Pred_d = PValue - Pred_s,
         school_n = n(),
         Pred_sum = Pred_s*school_n) %>% 
  ungroup() %>% 
  mutate(Pred_ex = (Pred_sum-PValue)/(school_n-1)) %>% 
  select(-school_n,-Pred_sum,-Pred_d,-Pred_s) %>% 
  group_by(Predictor,Outcome) %>% 
  mutate_at(c("PValue","Pred_ex"),scale) %>% #Exclude for nonstandardized estimates
  nest() 


# Spreading nesteddata
nesteddata$data[[1]]

d %>% 
  mutate_at(3:11,scale) %>% 
  group_by(school) %>% 
  arrange(school) %>% 
  mutate(mgirt = mean(Grit,na.rm=T),
    Grit_ex = ((mean(Grit,na.rm=T)*n())-Grit)/n()-1)
  

models = nesteddata %>% 
  mutate(#lm = map(data,~lm(OValue ~ PValue + Pred_ex + factor(school),data=.)),
         #lmb = map(lm,lm.beta::lm.beta),
         #tidy = map(lmb,tidy),
        # glance = map(lmb,glance),
         felm = map(data,~felm(OValue ~ PValue + Pred_ex | 0 | 0 | school,data=.)))



tidy.felm = function(felm){
  coef = felm$coefficients %>% as.data.frame() %>% rownames_to_column("term")
  p = felm$pval %>% enframe("term","p") 
  se = felm$se %>% enframe("term","SE")
  t = felm$tval %>% enframe("term","t")
  tidy = left_join(coef,se) %>% left_join(t)%>%  left_join(p)
  return(tidy)
}

models %>% 
  mutate(tidy = map(felm,tidy.felm)) %>% 
  unnest(tidy) %>%  
  mutate_at(c("OValue","SE","t"),round,2) %>% 
  mutate_at("p",Ben::formatps) %>% 
  select(-data,-felm) %T>% 
  Ben::write.clip()

# Running FELM model, controlling for school
models2 = nesteddata %>% 
  mutate(felm = map(data,~felm(OValue ~ PValue + Pred_ex | (school) | 0 | school,data=.)))


models2 %>% 
  mutate(tidy = map(felm,tidy.felm)) %>% 
  unnest(tidy) %>%  
  mutate_at(c("OValue","SE","t"),round,2) %>% 
  mutate_at("p",Ben::formatps) %>% 
  select(-data,-felm) %T>% 
  Ben::write.clip()

#Table
models %>% 
  mutate(tidy = map(felm,tidy.felm)) %>% 
  unnest(tidy) %>% 
  select(Predictor,Outcome, term,OValue,SE,p) %>% 
  filter(term %in% c('PValue','Pred_ex')) %>% 
  #Just keep formated estimates
  mutate(beta = Ben::formatest(OValue,p)) %>% 
  select(Predictor,Outcome,term,beta) %>% 
  spread(term,beta) %>% 
  unite(B,Pred_ex,PValue,sep = "|") %>% 
  spread(Outcome,B)
  
#No Clustered errors
models %>% 
  mutate(tidy = map(felm,tidy.felm)) %>% 
  unnest(tidy) %>% 
  filter(term  %in% c( "Pred_ex","PValue")  ,
         #Predictor %in% c("Grad4","Grad6"),
         #Outcome != "Ravens"
         ) %>% 
  mutate(Term = case_when(term == "PValue" ~ "Own",
                          term == "Pred_ex" ~ "Peer")) %>% 
  ggplot(aes(Outcome,OValue,shape = p<.05,col = Term))+
  geom_point(size = 3)+
  geom_line(aes(group = Term))+
  facet_wrap(~Predictor,nrow = 1)+
  labs(title = "Classmate quality and objective and self-reported grit",
       x = 'Outcome',
       y = 'Beta')+
  geom_hline(yintercept = 0,size = .2)+
  geom_vline(xintercept = 2.5,size = .2)+
  Ben::theme_ang()




