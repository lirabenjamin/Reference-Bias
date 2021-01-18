require(tidyverse)
require(DescTools)
require(broom)
require(broom.mixed)
require(knitr)
require(sjstats)
#devtools::install_github("jmobrien/PsychMisc")
require(PsychMisc)
require(openxlsx)
require(lme4)
require(performance)
#devtools::install_github("lirabenjamin/Ben")
library(Ben)

# Load in the data from the cleanup file 
load("2020-Graduation.Analysis.Data.4-6yr.rda")

# Function definitions ----------------------------------------------------

# Format table: This creates a nicely structured pre-formatted table from the tidy output of a tidied model:
FormatTable <- function(tidymod, lookuptable){
  tidymod$effect <- NULL
  tidymod$group <- NULL
  tidymod$term <- lookuptable[tidymod$term]
  tidymod %>% 
    modify_at(
      .at = c("estimate", "std.error",  "statistic", 
              "OR", "OR.high", "OR.low"),
      .f = ~Format(.x, digits = 2, align = "\\r", na.form = "")
    ) %>% 
    modify_at(
      .at = "p.value",
      .f = ~Format(.x, digits = 3, leading = "drop", , align = "\\r", na.form = "") %>% 
            {ifelse(. == ".000", "< .001", .)}
    ) %>% 
    modify_at(
      .at = c("Prop.low", "Prop.neut", "Prop.high"),
      .f = ~Format(.x, fmt = "%", digits = 1, align = "\\r", na.form = "")
    ) %>% 
    setNames(c("Term", "b", "SE", "z", "p", "OR", "Odds,\n-1 SD", "Odds,\n+1 SD", "Prob,\n-1 SD", "Prob,\n0 SD", "Prob,\n+1 SD"))
  
}

# ConstructTable Formats a tidy outputted model into something more useful:
ConstructTable <- function(tidymod){
  tidymod$OR <- exp(tidymod$estimate)
  tidymod$OR.low <- exp(tidymod$estimate[1] - tidymod$estimate)
  tidymod$OR.low[1] <- NA
  tidymod$OR.high <- exp(tidymod$estimate[1] + tidymod$estimate)
  tidymod$OR.high[1] <- NA
  tidymod$Prop.low <- tidymod$OR.low/(tidymod$OR.low + 1)
  tidymod$Prop.low[1] <- NA
  tidymod$Prop.neut <- tidymod$OR[1]/(tidymod$OR[1] + 1)
  tidymod$Prop.high <- tidymod$OR.high/(tidymod$OR.high + 1)
  tidymod$Prop.high[1] <- NA
  
  # Remove the extra info for the intercept variance component: 
  tidymod[grep("schoolID", tidymod$term, ignore.case = TRUE), 3:12] <- NA
  
  tidymod
}

# table writing function:
tableWrite <- 
  function(x, filename){x %>% unclass %>% as_tibble(rownames = " ") %>% 
      write_csv(path = paste0("./Study1/Tables/", filename))}



# Basic correlations ------------------------------------------------------
refbias.charter %>%
  select(grit.c.Z,L1.eb.grit.c.Z,L2.eb.grit.c.Z) %>% 
  rowwise() %>% 

# Correlations overall:
predictor.table <-
  refbias.charter %>%
  select(
    Grad4years = GRAD_Any_in_4YRS,
    Grad6years = GRAD_Any_in_6YRS,
    Grit = grit.c.Z,
    `Self-control` = sc.c.Z,
    #`Self-regulation` = selfreg.f.c.Z,
    `ADT (sqrt)`= Tmath.c.Z, 
    Ravens = ravens.strict.c.Z
  ) %>% 
  Ben::HARcor() 

#controlling for ravens
refbias.charter %>%
  select(
    Grit = grit.c.Z,
    `Self-control` = sc.c.Z,
    #`Self-regulation` = selfreg.f.c.Z,
    `ADT (sqrt)`= Tmath.c.Z, 
    Ravens = ravens.strict.c.Z
  ) %>% 
  corrtrol(df = .,
           vars = colnames(.),
           control.vars = "Ravens",
           copy = T)

# Within-school (L1) correlations:
L1.predictor.table <-
  refbias.charter %>%
  select(
    Grad4years = GRAD_Any_in_4YRS,
    Grad6years = GRAD_Any_in_6YRS,
    Grit = L1.eb.grit.c.Z,
    `Self-control` = L1.eb.sc.c.Z,
    #`Self-regulation` = L1.eb.selfreg.f.c.Z,
    `ADT (sqrt)`= L1.eb.Tmath.c.Z, 
    Ravens = ravens.strict.c.Z
  ) %>% 
  mutate(Ravens = EBcenter(refbias.charter, ravens.strict.c.Z, grouping = schoolID)) %>% 
  Ben::HARcor() 

refbias.charter %>%
  select(
    Grit = L1.eb.grit.c.Z,
    `Self-control` = L1.eb.sc.c.Z,
    #`Self-regulation` = L1.eb.selfreg.f.c.Z,
    `ADT (sqrt)`= L1.eb.Tmath.c.Z, 
    Ravens = ravens.strict.c.Z
  ) %>% 
  mutate(Ravens = EBcenter(refbias.charter, ravens.strict.c.Z, grouping = schoolID)) %>% 
  corrtrol(df = .,
           vars = colnames(.),
           control.vars = "Ravens",
           copy = T)

# Between-school (L2) correlations
L2.predictor.table <-
  refbias.charter %>%
  arrange(schoolID) %>% 
  select(
    schoolID,
    Grad4years = GRAD_Any_in_4YRS,
    Grad6years = GRAD_Any_in_6YRS,
    Grit = L2.eb.grit.c.Z,
    `Self-control` = L2.eb.sc.c.Z,
    `Self-regulation` = L2.eb.selfreg.f.c.Z,
    `ADT (sqrt)`= L2.eb.Tmath.c.Z, 
    Ravens = ravens.strict.c.Z
  ) %>% 
  mutate(L2.Ravens = EBmeans(refbias.charter, ravens.strict.c.Z, grouping = schoolID)) %>% 
  group_by(schoolID) %>% 
  dplyr::summarize(
    Grad4years = mean(Grad4years, na.rm=T),
    Grad6years = mean(Grad6years, na.rm=T),
    Grit = mean(Grit),
    `Self-control` = mean(`Self-control`),
    `Self-regulation` = mean(`Self-regulation`),
    `ADT (sqrt)`= mean(`ADT (sqrt)`), 
    Ravens = mean(L2.Ravens)
  ) %>% 
    select(-schoolID,-`Self-regulation`) %>% 
  HARcor()


refbias.charter %>%
  arrange(schoolID) %>% 
  select(
    schoolID,
    Grit = L2.eb.grit.c.Z,
    `Self-control` = L2.eb.sc.c.Z,
    `Self-regulation` = L2.eb.selfreg.f.c.Z,
    `ADT (sqrt)`= L2.eb.Tmath.c.Z, 
    Ravens = ravens.strict.c.Z
  ) %>% 
  mutate(L2.Ravens = EBmeans(refbias.charter, ravens.strict.c.Z, grouping = schoolID)) %>% 
  group_by(schoolID) %>% 
  dplyr::summarize(
    Grit = mean(Grit),
    `Self-control` = mean(`Self-control`),
    `Self-regulation` = mean(`Self-regulation`),
    `ADT (sqrt)`= mean(`ADT (sqrt)`), 
    Ravens = mean(L2.Ravens)
  ) %>% 
  select(-schoolID,-`Self-regulation`) %>% 
  corrtrol(df = .,
           vars = colnames(.),
           control.vars = "Ravens",
           copy = T)
  
# Table of counts of individuals who graduated in 4/6 years
persist.grad.table <- 
  table(
    refbias.charter$GRAD_Any_in_4YRS %>% factor(0:1, c("4yr-No", "4yr-Yes")), 
    refbias.charter$GRAD_Any_in_6YRS %>% factor(0:1, c("6yr-No", "6yr-Yes"))
    , dnn = c("4yr", "6yr"))

# Table of proportions of individuals who graduated in 4/6 years
persist.grad.table.prop <- 
  Format(prop.table(
    table(
      refbias.charter$GRAD_Any_in_4YRS %>% factor(0:1, c("4yr-No", "4yr-Yes")), 
      refbias.charter$GRAD_Any_in_6YRS %>% factor(0:1, c("6yr-No", "6yr-Yes"))
      , dnn = c("4yr", "6yr"))) 
    , fmt = "%", digits = 1)


# Write out tables:
predictor.table %>% tableWrite("2020.predictor.correlation.table.csv")
L1.predictor.table %>% tableWrite("2020.L1.predictor.correlation.table.csv")
L2.predictor.table %>% tableWrite("2020.L2.predictor.correlation.table.csv")
persist.grad.table %>% tableWrite("2020.4-6yr.graduation.table.csv")
persist.grad.table.prop %>% tableWrite("2020.4-6yr.table.proportion.csv")


# Models with maximal available sample (self-report), Math MD's --------


# ICC's -------------------------------------------------------------------

# By outcome and predictor:

ICCs <- 
list(
  n1278.4yr = glmer(GRAD_Any_in_4YRS ~ (1 | schoolID), 
                    family=binomial(link="logit"), refbias.charter),
  n1166.4yr = glmer(GRAD_Any_in_4YRS ~ (1 | schoolID), 
                    family=binomial(link="logit"), refbias.charter.selfreport.nomiss),
  n802.4yr = glmer(GRAD_Any_in_4YRS ~ (1 | schoolID), 
                   family=binomial(link="logit"), refbias.charter.all.nomiss),

  n1278.6yr = glmer(GRAD_Any_in_6YRS ~ (1 | schoolID), 
                    family=binomial(link="logit"), refbias.charter),
  n1166.6yr = glmer(GRAD_Any_in_6YRS ~ (1 | schoolID), 
                    family=binomial(link="logit"), refbias.charter.selfreport.nomiss),
  n802.6yr = glmer(GRAD_Any_in_6YRS ~ (1 | schoolID), 
                   family=binomial(link="logit"), refbias.charter.all.nomiss),
  
  n1278.grit = lmer(grit.c ~ (1 | schoolID), refbias.charter),
  
  n1166.grit = lmer(grit.c ~ (1 | schoolID), refbias.charter.selfreport.nomiss),
  n802.grit = lmer(grit.c ~ (1 | schoolID), refbias.charter.all.nomiss),
  
  n1278.sc = lmer(sc.c ~ (1 | schoolID), refbias.charter),
  n1166.sc = lmer(sc.c ~ (1 | schoolID), refbias.charter.selfreport.nomiss),
  n802.sc = lmer(sc.c ~ (1 | schoolID), refbias.charter.all.nomiss),
  
  n1278.selfreg = lmer(selfreg.f.c ~ (1 | schoolID), refbias.charter),
  n1166.selfreg = lmer(selfreg.f.c ~ (1 | schoolID), refbias.charter.selfreport.nomiss),
  n802.selfreg = lmer(selfreg.f.c ~ (1 | schoolID), refbias.charter.all.nomiss),
  
  n1278.Tmath = lmer(Tmath ~ (1 | schoolID), refbias.charter),
  n1166.Tmath = lmer(Tmath ~ (1 | schoolID), refbias.charter.selfreport.nomiss),
  n802.Tmath = lmer(Tmath ~ (1 | schoolID), refbias.charter.all.nomiss)
) %>% 
  map_dbl(~icc(.x) %>% 
            pluck("ICC_adjusted")
          ) %>% 
  matrix(ncol = 6, 
         dimnames = list(c("n1278", "n1166", "n802"), c("4-year", "6-year", "Grit", "Self-Control", "Self-Regulation", "ADT"))
  )

ICCs %>% tableWrite("2020.outcome.predictor.ICC.table.csv")

# EVERYONE ----------------------------------------------------------------



# 4-year graduation -----------------------------------------


#GRIT

#grit random intercept and slope with nAGQ
# OVERALLSTD - standardized to data w/out respect for level 
# allows cross-level coefficient comparison
grit.L1L2.4yrgrad <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.grit.c.Z_MR + L2.eb.grit.c.Z + L1.eb.grit.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1| schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

grit.overall.4yrgrad <- 
  glmer(GRAD_Any_in_4YRS ~ 
          grit.c.Z_MR + grit.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

# 6 year graduation:

grit.L1L2.6yrgrad <- 
  glmer(GRAD_Any_in_6YRS ~ 
          L1.eb.grit.c.Z_MR + L2.eb.grit.c.Z + L1.eb.grit.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1| schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

grit.overall.6yrgrad <- 
  glmer(GRAD_Any_in_6YRS ~ 
          grit.c.Z_MR + grit.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )



#ADT (square root)
# NOTE: here and below, there is a missing dummy for ADT (Tmath
# This is so that the grit and math models use the same data
# For comparison in the opposite direction, see below.

# 4 year graduation:
Tmath.L1L2.4yrgrad <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.Tmath.c.Z_MR + L2.eb.Tmath.c.Z + L1.eb.Tmath.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

Tmath.overall.4yrgrad <- 
  glmer(GRAD_Any_in_4YRS ~ 
          Tmath.c.Z_MR + Tmath.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

# 6 year graduation:

Tmath.L1L2.6yrgrad <- 
  glmer(GRAD_Any_in_6YRS ~ 
          L1.eb.Tmath.c.Z_MR + L2.eb.Tmath.c.Z + L1.eb.Tmath.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

Tmath.overall.6yrgrad <- 
  glmer(GRAD_Any_in_6YRS ~ 
          Tmath.c.Z_MR + Tmath.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )




# Output section: ---------------------------------------------------------

# Make a list of all the models just run, construct associated names:
model.list <-
  list(
    grit.L1L2.4yrgrad,
    grit.L1L2.6yrgrad,
    Tmath.L1L2.4yrgrad,
    Tmath.L1L2.6yrgrad,
    
    grit.overall.4yrgrad,
    grit.overall.6yrgrad,
    Tmath.overall.4yrgrad,
    Tmath.overall.6yrgrad
    
  ) %>% 
  
  setNames(                                      # Quick way to add names to above
    construct.all(c("Grit-", "TMath-"), # Outcome
                  c("4yr-", "6yr-"),             # 5 year or 1 year graduation
                  c("L1L2", "overall"),
                  cycle.order = c(2, 1, 3)) 
  )

# "Tidy" all models so we can use them / put them into a table: 
suppressWarnings(
  model.list.tidy <- 
    model.list %>% map(tidy)
)

# List of all variables:
tidy.varlist <-
  map(model.list.tidy, pull, term) %>% # Get the terms from all models
  unlist %>% # Make into a vector of all unique ones.
  unique %>% 
  sort

# This (kind of inelegant) bit makes the headers for the tables
table.varlist <-
  c("Intercept",

    # Level 1:
    "Grit, Overall Individual",
    "Grit Missing Dummy",
    "Grit, Within-School ", 
    "Math Missing Dummy",
    "# Math Problems, Within-School", 
    
    # Level 2:
    "Grit, Between-Schools",
    "# Math Problems, Between-Schools",
    "Cognitive Missing Dummy",
    "Cognitive Ability (Raven's)",
    "School Random Intercept SD",
    "# Math Problems, Overall Individual"
  )

# Checking
length(table.varlist) == length(tidy.varlist)


# Combine the two above into 
# a lookup vector for use below, to convert names:
lookup <- 
  setNames(table.varlist, tidy.varlist)


# Standard review of model results w/summary ------------------------------

# Just to look at things if you want
# map(model.list, summary)
# map(model.list, glance)
map_int(model.list, nobs) # All 1278

# Table construction ----------------------------------------------


# Using the ConstructTable tool defined above, 
# makes a complete table for displaying the data:

# Make the tables (with OR's, probabilities, etc.)
model.list.calc <- 
  model.list.tidy %>% map(ConstructTable)
# Make the tables look nice:
model.list.format <- 
  model.list.calc %>% map(FormatTable, lookup)


write.xlsx(model.list.format, 
           file = "./Study1/Tables/2020.Model.Tables.FINAL.L1L2.OVERALL.xlsx")




# Formatted tables --------------------------------------------------------

# This creates tables quickly so we don't have to manually copy them.  Uses an existing template:

template <- loadWorkbook("./Study1/Tables/2020.model.template.xlsx")



params.table <- 
  tibble(
    x = model.list.format,
    sheet = rep(1:4, each = 2),
    startRow = rep(c(7, 20), 4)
  )

notes <-
  tibble(
    x = "n = 1278 - Includes all sampled students in school, with missing dummies for individual grit (n = 112) and ADT (n = 476).  Estimates L2 effect for those students based on others.",
    sheet = rep(1:4, 2),
    startRow = 2
  )


# Update the tables:
pwalk(params.table, writeData, template, colNames = FALSE) 
pwalk(notes, writeData, template, colNames = FALSE) 

saveWorkbook(template, "./Study1/Tables/2020 - Model.Tables.Combined.n1278.xlsx", overwrite = TRUE)


