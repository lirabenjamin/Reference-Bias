# Study 1 as Study 2
# MLM model on ENLACE data
# Model: Random intercept: ENLACE or GPA ~ L1 Grit + L2 Grit + (1|School ID)

library(tidyverse)
library(Ben)
library(lme4)
library(lmerTest)
library(magrittr)
library(lm.beta)

e = read_csv("Data/2021-01-30 ENLACE subsample for multilevel model.csv")

#missingness
e %>% 
  mutate(mg = is.na(grit)) %>% 
  group_by(schoolid) %>% 
  summarise(mg = mean(mg) %>% round(2)) %>% pull(mg) %>% 
  table()

# Generating estimates for L1 and L2 grit. L2 grit is the school mean, L1 grit is the residual for each person - school mean.
ne = e %>% 
  mutate_at(3:18,scale) %>% #standardizing everything
  group_by(schoolid,year) %>% 
  mutate(L2grit = mean(grit,na.rm=T), #swap grit for stdgrit to use full-data standarization.
         L1grit = grit - L2grit) %>% 
  select(schoolid,matches("grit"),own_reading,own_math,own_gpa_hig,own_gpa_mat,own_gpa_lan,-grit,-stdgrit) %>% 
  #Nesting the dataset
  gather(Outcome,Value,-c(year:L1grit)) %>% 
  group_by(Outcome) %>% 
  nest()

#Function to extract info from LME object
tidylmer = function(x){
  coefs = x%>% summary %>% coefficients() %>% as.data.frame() %>% rownames_to_column("term") %>% rename(p = `Pr(>|t|)`) %>% select(-df)
  vc = x %>% VarCorr() %>% as.data.frame() %>% select(grp,sdcor) %>% mutate(`Std. Error` = NA, `t value` = NA, p = NA) %>% rename(term = grp,Estimate = sdcor) %>% filter(term != "Residual") %>% mutate(term = "Intercept (SD)")
  coefs = coefs %>% rbind(vc)  %>% rename(t = `t value`, SE  = `Std. Error`, B = Estimate)
  return(coefs)
}

#Calculating all models
models = ne %>% 
         # Null model
  mutate(nm = map(data,function(x){x %>%  lmer(Value ~ 1 + (1|schoolid),data=.)}),
         # ICC computation from null model
         icc = map(nm,function(x){x %>% VarCorr() %>% as.data.frame() %>% select(grp,vcov) %>% spread(grp,vcov) %>% transmute(ICC = schoolid/(Residual+schoolid)) %>% return()})) %>% 
  unnest(icc) %>% 
         # Complete model
  mutate(fm = map(data,function(x){x %>% lmer(Value ~ L1grit + L2grit + factor(year) + (1|schoolid),data=.)}),
         coefs = map(fm, tidylmer)) %>% 
  #Adding predicted values
  mutate(pred = map(fm,predict)) %>% 
  mutate(pred = map(pred, enframe)) %>% 
  mutate(data = map(data, rownames_to_column,"name")) %>% 
  #Copying predicted values to data
  mutate(data = map2(data,pred,left_join))

#Generating Table
models %>% 
  rowwise() %>% 
  unnest(coefs) %>% 
  select(Outcome, ICC, term,B,SE,t,p) %>% 
  mutate_at(c(2,4,5,6),numformat) %>% 
  mutate(p = numformat(p,3),
         p = ifelse(p == ".000", "<.001",p)) %>% 
  mutate_all(function(x){str_replace_all(x,"NA","")}) %>% 
  mutate_all(function(x){str_replace_na(x,"")}) %T>% 
  write.clip()

#Plot
models %>% 
  unnest(data) %>% 
  filter(!is.na(value)) %>% 
  #filter(schoolid < 11) %>%            #Uncomment this line for the code to run faster on only 10 schools.
  ggplot(aes(L1grit + L2grit, value,col=factor(schoolid)))+
  geom_line(alpha = .5,size = .2)+
  #coord_cartesian(xlim = c(-1,1))+     #Uncomment this line to focus on the school level means
  geom_point(data = . %>% group_by(schoolid,year,Outcome) %>% summarise_all(mean,na.rm=T),aes(x = L2grit),alpha = .5)+
  geom_smooth(data = . %>% group_by(schoolid,year,Outcome) %>% summarise_all(mean,na.rm=T),aes(x = L2grit,y = value),method="lm",color="black")+
  facet_grid(year~Outcome)+
  theme_ang()+
  theme(legend.position = "none")

#Appendix: Running as mean level lms
e %>% 
  select(!matches("classmates")) %>% 
  group_by(schoolid,year) %>% summarise_all(mean,na.rm=T) %>% # comment this line to run at student level
  gather(Outcome, Value,-c(1:4)) %>% 
  group_by(Outcome) %>% 
  nest() %>% 
  mutate(lm = map(data,~lm(Value ~ grit + year, data=.)),
         lm = map(lm,lm.beta),
         tidy = map(lm,broom::tidy)) %>% 
  unnest(tidy) %>% 
  select(-data,-lm) %>% 
  mutate(beta = formatest(std_estimate,p.value)) %>% 
  select(Outcome, term,beta) %>% 
  spread(Outcome,beta) %>% 
  filter(term != "(Intercept)") %T>% 
  write.clip()

models %>% 
  unnest(coefs) %>% 
  mutate(beta = formatest(B,p,2)) %>%
  select(Outcome,term,beta) %>% 
  spread(term,beta) %>% 
  select(!matches("Intercept")) %T>% 
  write.clip()
      
