require(tidyverse)
require(DescTools)
require(broom)
require(broom.mixed)
require(knitr)
require(sjstats)
# require(ADRGtools)
require(openxlsx)
require(lme4)
require(performance)

# Load in the data from the cleanup file 
load("./Study1/Data/R.Datafiles/2020-Graduation.Analysis.Data.4-6yr.rda")

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
      .f = ~Format(.x, digits = 2, sci = NA, align = "\\r", na.form = "")
    ) %>% 
    modify_at(
      .at = "p.value",
      .f = ~Format(.x, digits = 3, leading = "drop", sci = NA, , align = "\\r", na.form = "") %>% 
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



# Basic correlations ------------------------------------------------------

predictor.table <-
  refbias.charter %>%
  select(
    Grit = grit.c.Z,
    `Self-control` = sc.c.Z,
    `Self-regulation` = selfreg.f.c.Z,
    `ADT (sqrt)`= Tmath.c.Z, 
    Ravens = ravens.strict.c.Z
  ) %>% 
  cor(use = "pairwise.complete") %>% 
  round(3) %>% 
  Format(sci = NA, leading = "drop", digits = 3)

L1.predictor.table <-
  refbias.charter %>%
  select(
    Grit = L1.eb.grit.c.Z,
    `Self-control` = L1.eb.sc.c.Z,
    `Self-regulation` = L1.eb.selfreg.f.c.Z,
    `ADT (sqrt)`= L1.eb.Tmath.c.Z, 
    Ravens = ravens.strict.c.Z
  ) %>% 
  mutate(Ravens = EBcenter(refbias.charter, ravens.strict.c.Z, grouping = schoolID)) %>% 
    cor(use = "pairwise.complete") %>% 
    round(3) %>% 
    Format(sci = NA, leading = "drop", digits = 3)

L2.predictor.table <-
  refbias.charter %>%
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
  summarize(
    Grit = unique(Grit),
    `Self-control` = unique(`Self-control`),
    `Self-regulation` = unique(`Self-regulation`),
    `ADT (sqrt)`= unique(`ADT (sqrt)`), 
    Ravens = unique(L2.Ravens)
  ) %>% 
    select(-schoolID) %>% 
    cor(use = "pairwise.complete") %>% 
    round(3) %>% 
    Format(sci = NA, leading = "drop", digits = 3)


persist.grad.table <- 
  table(
    refbias.charter$GRAD_Any_in_4YRS %>% factor(0:1, c("4yr-No", "4yr-Yes")), 
    refbias.charter$GRAD_Any_in_6YRS %>% factor(0:1, c("6yr-No", "6yr-Yes"))
    , dnn = c("4yr", "6yr"))

persist.grad.table.prop <- 
  Format(prop.table(
    table(
      refbias.charter$GRAD_Any_in_4YRS %>% factor(0:1, c("4yr-No", "4yr-Yes")), 
      refbias.charter$GRAD_Any_in_6YRS %>% factor(0:1, c("6yr-No", "6yr-Yes"))
      , dnn = c("4yr", "6yr"))) 
    , fmt = "%", digits = 1)


tableWrite <- function(x, filename){x %>% unclass %>% as_tibble(rownames = " ") %>% write_csv(path = paste0("./Study1/Tables/", filename))}

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
grit.eb.model.4yrgrad.charter.overallstd <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.grit.c.Z_MR + L2.eb.grit.c.Z + L1.eb.grit.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

# LEVELSTD - standardized within-level, coefficients are like ß's 
# (not quite b/c it's logistic)
grit.eb.model.4yrgrad.charter.levelstd <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.grit.c.ZZ_MR + L2.eb.grit.c.ZZ + L1.eb.grit.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

#Self-control

sc.eb.model.4yrgrad.charter.overallstd <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.sc.c.Z_MR + L2.eb.sc.c.Z + L1.eb.sc.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )


sc.eb.model.4yrgrad.charter.levelstd <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.sc.c.ZZ_MR + L2.eb.sc.c.ZZ + L1.eb.sc.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

#Self-regulation

selfreg.eb.model.4yrgrad.charter.overallstd <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.selfreg.f.c.Z_MR + L2.eb.selfreg.f.c.Z + L1.eb.selfreg.f.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

selfreg.eb.model.4yrgrad.charter.levelstd <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.selfreg.f.c.ZZ_MR + L2.eb.selfreg.f.c.ZZ + L1.eb.selfreg.f.c.Z_MD
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
Tmath.eb.model.4yrgrad.charter.overallstd <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.Tmath.c.Z_MR + L2.eb.Tmath.c.Z + L1.eb.Tmath.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

Tmath.eb.model.4yrgrad.charter.levelstd <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.Tmath.c.ZZ_MR + L2.eb.Tmath.c.ZZ + L1.eb.Tmath.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )


# ADT (untransformed):

math.eb.model.4yrgrad.charter.overallstd <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.math.c.Z_MR + L2.eb.math.c.Z + L1.eb.math.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

math.eb.model.4yrgrad.charter.levelstd <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.math.c.ZZ_MR + L2.eb.math.c.ZZ + L1.eb.math.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

# 6-year graduation -----------------------------------------


#GRIT

#grit random intercept and slope with nAGQ
# OVERALLSTD - standardized to data w/out respect for level 
# allows cross-level coefficient comparison
grit.eb.model.6yrgrad.charter.overallstd <- 
  glmer(GRAD_Any_in_6YRS ~ 
          L1.eb.grit.c.Z_MR + L2.eb.grit.c.Z + L1.eb.grit.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

# LEVELSTD - standardized within-level, coefficients are like ß's 
# (not quite b/c it's logistic)
grit.eb.model.6yrgrad.charter.levelstd <- 
  glmer(GRAD_Any_in_6YRS ~ 
          L1.eb.grit.c.ZZ_MR + L2.eb.grit.c.ZZ + L1.eb.grit.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

#Self-control

sc.eb.model.6yrgrad.charter.overallstd <- 
  glmer(GRAD_Any_in_6YRS ~ 
          L1.eb.sc.c.Z_MR + L2.eb.sc.c.Z + L1.eb.sc.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )


sc.eb.model.6yrgrad.charter.levelstd <- 
  glmer(GRAD_Any_in_6YRS ~ 
          L1.eb.sc.c.ZZ_MR + L2.eb.sc.c.ZZ + L1.eb.sc.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

#Self-regulation

selfreg.eb.model.6yrgrad.charter.overallstd <- 
  glmer(GRAD_Any_in_6YRS ~ 
          L1.eb.selfreg.f.c.Z_MR + L2.eb.selfreg.f.c.Z + L1.eb.selfreg.f.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

selfreg.eb.model.6yrgrad.charter.levelstd <- 
  glmer(GRAD_Any_in_6YRS ~ 
          L1.eb.selfreg.f.c.ZZ_MR + L2.eb.selfreg.f.c.ZZ + L1.eb.selfreg.f.c.Z_MD
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
Tmath.eb.model.6yrgrad.charter.overallstd <- 
  glmer(GRAD_Any_in_6YRS ~ 
          L1.eb.Tmath.c.Z_MR + L2.eb.Tmath.c.Z + L1.eb.Tmath.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

Tmath.eb.model.6yrgrad.charter.levelstd <- 
  glmer(GRAD_Any_in_6YRS ~ 
          L1.eb.Tmath.c.ZZ_MR + L2.eb.Tmath.c.ZZ + L1.eb.Tmath.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )


# ADT (untransformed):

math.eb.model.6yrgrad.charter.overallstd <- 
  glmer(GRAD_Any_in_6YRS ~ 
          L1.eb.math.c.Z_MR + L2.eb.math.c.Z + L1.eb.math.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

math.eb.model.6yrgrad.charter.levelstd <- 
  glmer(GRAD_Any_in_6YRS ~ 
          L1.eb.math.c.ZZ_MR + L2.eb.math.c.ZZ + L1.eb.math.c.Z_MD
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
    grit.eb.model.4yrgrad.charter.levelstd,
    sc.eb.model.4yrgrad.charter.levelstd,
    selfreg.eb.model.4yrgrad.charter.levelstd,
    math.eb.model.4yrgrad.charter.levelstd,
    Tmath.eb.model.4yrgrad.charter.levelstd,
    
    grit.eb.model.4yrgrad.charter.overallstd,
    sc.eb.model.4yrgrad.charter.overallstd,
    selfreg.eb.model.4yrgrad.charter.overallstd,
    math.eb.model.4yrgrad.charter.overallstd,
    Tmath.eb.model.4yrgrad.charter.overallstd,
    
    grit.eb.model.6yrgrad.charter.levelstd,
    sc.eb.model.6yrgrad.charter.levelstd,
    selfreg.eb.model.6yrgrad.charter.levelstd,
    math.eb.model.6yrgrad.charter.levelstd,
    Tmath.eb.model.6yrgrad.charter.levelstd,
    
    grit.eb.model.6yrgrad.charter.overallstd,
    sc.eb.model.6yrgrad.charter.overallstd,
    selfreg.eb.model.6yrgrad.charter.overallstd,
    math.eb.model.6yrgrad.charter.overallstd,
    Tmath.eb.model.6yrgrad.charter.overallstd
    
  ) %>% 
  
  setNames(                                      # Quick way to add names to above
    construct.all(c("Grit-", "SelfControl-", "SelfReg-",  "Math-", "TMath-"), # Outcome
                  c("4yr-", "6yr-"),             # 4 year or 6 year graduation
                  c("LevelStd", "OverallStd"),   # Standardized 
                  cycle.order = c(1, 3, 2)) 
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
    # Both standardizations:
    # Grit:
    "Grit Missing Dummy",
    rep("Grit, Level 1 (Within-School)", 2), 
    # non-transformed math, missing dummy code and 2 variables:
    "Math Missing Dummy",
    rep("# Math Problems, Level 1 (Within-School)", 2), 
    # Self-control:
    "Self-Control Missing Dummy",
    rep("Self-Control, Level 1 (Within-School)", 2), 
    # Self-regulation:
    "Self-Regulation Missing Dummy",
    rep("Self-Regulation, Level 1 (Within-School)", 2), 
    # Transformed math, same:
    "Math Missing Dummy",
    rep("# Math Problems, Level 1 (Within-School)", 2), 
    
    # Level 2:
    #Grit 
    rep("Grit, Level 2 (Between-Schools)", 2),
    # Math
    rep("# Math Problems, Level 2 (Between-Schools)", 2),
    # Self-control
    rep("Self-Control, Level 2 (Between-Schools)", 2), 
    # Self-regulation:
    rep("Self-Regulation, Level 2 (Between-Schools)", 2),
    # Transformed Math:
    rep("# Math Problems, Level 2 (Between-Schools)", 2),
    "Cognitive Missing Dummy",
    "Cognitive Ability (Raven's)",
    "School Random Intercept SD"
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
           file = "./Study1/Tables/2020.Model.Tables.4-6yr.xlsx")



# NO MISSING SELF-REPORT --------------------------------------------------

# 4-year graduation -----------------------------------------


#GRIT

#grit random intercept and slope with nAGQ
# OVERALLSTD - standardized to data w/out respect for level 
# allows cross-level coefficient comparison
grit.eb.model.4yrgrad.charter.overallstd.ALLSR <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.grit.c.Z + L2.eb.grit.c.Z
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.selfreport.nomiss, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

# LEVELSTD - standardized within-level, coefficients are like ß's 
# (not quite b/c it's logistic)
grit.eb.model.4yrgrad.charter.levelstd.ALLSR <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.grit.c.ZZ + L2.eb.grit.c.ZZ
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.selfreport.nomiss, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

#Self-control

sc.eb.model.4yrgrad.charter.overallstd.ALLSR <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.sc.c.Z + L2.eb.sc.c.Z
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.selfreport.nomiss, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )


sc.eb.model.4yrgrad.charter.levelstd.ALLSR <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.sc.c.ZZ + L2.eb.sc.c.ZZ
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.selfreport.nomiss, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

#Self-regulation

selfreg.eb.model.4yrgrad.charter.overallstd.ALLSR <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.selfreg.f.c.Z + L2.eb.selfreg.f.c.Z
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.selfreport.nomiss, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

selfreg.eb.model.4yrgrad.charter.levelstd.ALLSR <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.selfreg.f.c.ZZ + L2.eb.selfreg.f.c.ZZ
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.selfreport.nomiss, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

#ADT (square root)
# NOTE: here and below, there is a missing dummy for ADT (Tmath
# This is so that the grit and math models use the same data
# For comparison in the opposite direction, see below.
Tmath.eb.model.4yrgrad.charter.overallstd.ALLSR <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.Tmath.c.Z_MR + L2.eb.Tmath.c.Z + L1.eb.Tmath.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.selfreport.nomiss, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

Tmath.eb.model.4yrgrad.charter.levelstd.ALLSR <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.Tmath.c.ZZ_MR + L2.eb.Tmath.c.ZZ + L1.eb.Tmath.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.selfreport.nomiss, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )


# ADT (untransformed):

math.eb.model.4yrgrad.charter.overallstd.ALLSR <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.math.c.Z_MR + L2.eb.math.c.Z + L1.eb.math.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.selfreport.nomiss, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

math.eb.model.4yrgrad.charter.levelstd.ALLSR <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.math.c.ZZ_MR + L2.eb.math.c.ZZ + L1.eb.math.c.Z_MD
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.selfreport.nomiss, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )



# Output section: ---------------------------------------------------------

# Make a list of all the models just run, construct associated names:
model.list.ALLSR <-
  list(
    grit.eb.model.4yrgrad.charter.levelstd.ALLSR,
    sc.eb.model.4yrgrad.charter.levelstd.ALLSR,
    selfreg.eb.model.4yrgrad.charter.levelstd.ALLSR,
    math.eb.model.4yrgrad.charter.levelstd.ALLSR,
    Tmath.eb.model.4yrgrad.charter.levelstd.ALLSR,
    
    grit.eb.model.4yrgrad.charter.overallstd.ALLSR,
    sc.eb.model.4yrgrad.charter.overallstd.ALLSR,
    selfreg.eb.model.4yrgrad.charter.overallstd.ALLSR,
    math.eb.model.4yrgrad.charter.overallstd.ALLSR,
    Tmath.eb.model.4yrgrad.charter.overallstd.ALLSR
   
  ) %>% 
  
  setNames(                                      # Quick way to add names to above
    construct.all(c("Grit-", "SelfControl-", "SelfReg-",  "Math-", "TMath-"), # Outcome
                  c("4yr-", "6yr-"),             # 4 year or 6 year graduation
                  c("LevelStd", "OverallStd"),   # Standardized 
                  cycle.order = c(1, 2, 3)) 
  )

# "Tidy" all models so we can use them / put them into a table: 
suppressWarnings(
  model.list.tidy.ALLSR <- 
    model.list.ALLSR %>% map(tidy)
)

# List of all variables:
tidy.varlist.ALLSR <-
  map(model.list.tidy.ALLSR, pull, term) %>% # Get the terms from all models
  unlist %>% # Make into a vector of all unique ones.
  unique %>% 
  sort

# This (kind of inelegant) bit makes the headers for the tables
table.varlist.ALLSR <-
  c("Intercept",
    # Both standardizations:
    # Grit:
    rep("Grit, Level 1 (Within-School)", 2), 
    # non-transformed math, missing dummy code and 2 variables:
    "Math Missing Dummy",
    rep("# Math Problems, Level 1 (Within-School)", 2), 
    # Self-control:
    rep("Self-Control, Level 1 (Within-School)", 2), 
    # Self-regulation:
    rep("Self-Regulation, Level 1 (Within-School)", 2), 
    # Transformed math, same:
    "Math Missing Dummy",
    rep("# Math Problems, Level 1 (Within-School)", 2), 
    
    # Level 2:
    #Grit 
    rep("Grit, Level 2 (Between-Schools)", 2),
    # Math
    rep("# Math Problems, Level 2 (Between-Schools)", 2),
    # Self-control
    rep("Self-Control, Level 2 (Between-Schools)", 2), 
    # Self-regulation:
    rep("Self-Regulation, Level 2 (Between-Schools)", 2),
    # Transformed Math:
    rep("# Math Problems, Level 2 (Between-Schools)", 2),
    "Cognitive Missing Dummy",
    "Cognitive Ability (Raven's)",
    "School Random Intercept SD"
  )

# Checking
length(table.varlist.ALLSR) == length(tidy.varlist.ALLSR)


# Combine the two above into 
# a lookup vector for use below, to convert names:
lookup.ALLSR <- 
  setNames(table.varlist.ALLSR, tidy.varlist.ALLSR)






# Standard review of model results w/summary ------------------------------

# Just to look at things if you want
# map(model.list.ALLSR, summary)
# map(model.list.ALLSR, glance)
map(model.list.ALLSR, nobs) # All 1166

# Table construction ----------------------------------------------


# Using the ConstructTable tool defined above, 
# makes a complete table for displaying the data:

# Make the tables (with OR's, probabilities, etc.)
model.list.calc.ALLSR <- 
  model.list.tidy.ALLSR %>% map(ConstructTable)
# Make the tables look nice:
model.list.format.ALLSR <- 
  model.list.calc.ALLSR %>% map(FormatTable, lookup.ALLSR)


write.xlsx(model.list.format.ALLSR, 
           file = "./Study1/Tables/2019.Model.Tables.4yr.allSelfReport.xlsx")



# NO MISSING ANY KEY PREDICTOR --------------

# 5-year graduation ----------------------------------------

# Grit

grit.eb.model.4yrgrad.charter.overallstd.NOMD <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.grit.c.Z + L2.eb.grit.c.Z
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.all.nomiss,
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

grit.eb.model.4yrgrad.charter.levelstd.NOMD <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.grit.c.ZZ + L2.eb.grit.c.ZZ
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.all.nomiss, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

# Self-Control

sc.eb.model.4yrgrad.charter.overallstd.NOMD <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.sc.c.Z + L2.eb.sc.c.Z
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.all.nomiss,
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

sc.eb.model.4yrgrad.charter.levelstd.NOMD <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.sc.c.ZZ + L2.eb.sc.c.ZZ
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.all.nomiss, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

# Self-Regulation

selfreg.eb.model.4yrgrad.charter.overallstd.NOMD <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.selfreg.f.c.Z + L2.eb.selfreg.f.c.Z
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.all.nomiss,
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

selfreg.eb.model.4yrgrad.charter.levelstd.NOMD <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.selfreg.f.c.ZZ + L2.eb.selfreg.f.c.ZZ
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.all.nomiss, 
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )


# ADT transformed

Tmath.eb.model.4yrgrad.charter.overallstd.NOMD <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.Tmath.c.Z + L2.eb.Tmath.c.Z
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.all.nomiss,
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

Tmath.eb.model.4yrgrad.charter.levelstd.NOMD <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.Tmath.c.ZZ + L2.eb.Tmath.c.ZZ
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.all.nomiss,
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )


# ADT untransformed

math.eb.model.4yrgrad.charter.overallstd.NOMD <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.math.c.Z + L2.eb.math.c.Z
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.all.nomiss,
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

math.eb.model.4yrgrad.charter.levelstd.NOMD <- 
  glmer(GRAD_Any_in_4YRS ~ 
          L1.eb.math.c.ZZ + L2.eb.math.c.ZZ
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter.all.nomiss,
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )




# Output section: ---------------------------------------------------------

model.list.NOMD <-
  list(
    grit.eb.model.4yrgrad.charter.levelstd.NOMD,
    sc.eb.model.4yrgrad.charter.levelstd.NOMD,
    selfreg.eb.model.4yrgrad.charter.levelstd.NOMD,
    math.eb.model.4yrgrad.charter.levelstd.NOMD,
    Tmath.eb.model.4yrgrad.charter.levelstd.NOMD,
    
    grit.eb.model.4yrgrad.charter.overallstd.NOMD,
    sc.eb.model.4yrgrad.charter.overallstd.NOMD,
    selfreg.eb.model.4yrgrad.charter.overallstd.NOMD,
    math.eb.model.4yrgrad.charter.overallstd.NOMD,
    Tmath.eb.model.4yrgrad.charter.overallstd.NOMD
  ) %>% 
  
  
  setNames(                                      # Quick way to add names to above
    construct.all(c("Grit-", "SelfCont-", "SelfReg-",  "Math-", "TMath-"), # Outcome
                  c("4yr-"),             # 5 year or 1 year graduation
                  c("LevelStd", "OverallStd")   # Standardized 
    ))

# Check, good:
# model.list.NOMD %>% names

# Tidy models:
suppressWarnings(
model.list.tidy.NOMD <- 
  model.list.NOMD %>% map(tidy)
)

# Variable list:
tidy.varlist.NOMD <-
  map(model.list.tidy.NOMD, pull, term) %>% 
  unlist %>% unique %>% sort

table.varlist.NOMD <-
  c("Intercept",
    # Grit, 2 variables:
    rep("Grit, Level 1 (Within-School)", 2), 
    # non-transformed math, 2 variables:
    rep("# Math Problems, Level 1 (Within-School)", 2), 
    # Self-control, 2 variables:
    rep("Self-Control, Level 1 (Within-School)", 2), 
    # Self-Regulation
    rep("Self-Regulation, Level 1 (Within-School)", 2), 
    # Transformed math, same:
    rep("# Math Problems, Level 1 (Within-School)", 2), 
    # Level 2:
    rep("Grit, Level 2 (Between-Schools)", 2),
    rep("# Math Problems, Level 2 (Between-Schools)", 2),
    rep("Self-Control, Level 2 (Between-Schools)", 2), 
    rep("Self-Regulation, Level 2 (Between-Schools)", 2), 
    rep("# Math Problems, Level 2 (Between-Schools)", 2),
    "Cognitive Missing Dummy",
    "Cognitive Ability (Raven's)",
    "School Random Intercept SD"
  )

length(table.varlist.NOMD) == length(tidy.varlist.NOMD)


# Create a lookup vector for use below, to convert names:
lookup.NOMD <- setNames(table.varlist.NOMD, tidy.varlist.NOMD)

# Standard review of model results w/summary ------------------------------

# Just to look at things if you want
# map(model.list.NOMD, summary)
map(model.list.NOMD, nobs) # all 802

# Table construction ----------------------------------------------


# Using the ConstructTable tool defined above, 
# makes a complete table for displaying the data:

# Make the tables (with OR's, probabilities, etc.)
model.list.calc.NOMD <- 
  model.list.tidy.NOMD %>% map(ConstructTable)
# Make the tables look nice:
model.list.format.NOMD <- 
  model.list.calc.NOMD %>% map(FormatTable, lookup.NOMD)


write.xlsx(model.list.format.NOMD, 
           file = "./Study1/Tables/2019.Model.Tables.4yr.strict.nomissing.xlsx")


# Formatted tables --------------------------------------------------------

# This creates tables quickly so we don't have to manually copy them.  Uses an existing template:

n1278 <- loadWorkbook("./Study1/Tables/2019.Model.Table.Template.4yr.only.xlsx")
n1166 <- loadWorkbook("./Study1/Tables/2019.Model.Table.Template.4yr.only.xlsx")
n802 <- loadWorkbook("./Study1/Tables/2019.Model.Table.Template.4yr.only.xlsx")


params.n1278 <- 
  tibble(
    x = model.list.format,
    sheet = rep(1:5, 2),
    startRow = rep(c(7, 20), each = 5)
  )

params.n1166 <- 
  tibble(
    x = model.list.format.ALLSR,
    sheet = rep(1:5, 2),
    startRow = rep(c(7, 20), each = 5)
  )

params.n802 <- 
  tibble(
    x = model.list.format.NOMD,
    sheet = rep(1:5, 2),
    startRow = rep(c(7, 20), each = 5)
  )

n.n1278 <-
  tibble(
    x = "n = 1278 - Includes all sampled students in school, with missing dummies for individual grit (n = 112) and ADT (n = 476).  Estimates L2 effect for those students based on others.",
    sheet = rep(1:5, 2),
    startRow = 2
  )

n.n1166 <-
  tibble(
    x = "n = 1166 - Includes all students w/self-report data, with missing dummies for ADT (n = 364).  Estimates L2 effect for those students based on others.",
    sheet = rep(1:5, 2),
    startRow = 2
  )

n.n802 <-
  tibble(
    x = "n = 802 - Includes all students w/both self-report and ADT data. No missing dummies for either.  Cleanly compares the groups, but drops some non-overlapping data (n = 364 grit cases).",
    sheet = rep(1:5, 2),
    startRow = 2
  )




# Update the tables:
pwalk(params.n1278, writeData, n1278, colNames = FALSE) 
pwalk(params.n1166, writeData, n1166, colNames = FALSE)
pwalk(params.n802, writeData, n802, colNames = FALSE)
pwalk(n.n1278, writeData, n1278, colNames = FALSE) 
pwalk(n.n1166, writeData, n1166, colNames = FALSE)
pwalk(n.n802, writeData, n802, colNames = FALSE)

saveWorkbook(n1278, "./Study1/Tables/2019 - Model.Tables.Combined.4yr.n1278.xlsx", overwrite = TRUE)
saveWorkbook(n1166, "./Study1/Tables/2019 - Model.Tables.Combined.4yr.n1166.xlsx", overwrite = TRUE)
saveWorkbook(n802, "./Study1/Tables/2019 - Model.Tables.Combined.4yr.n802.xlsx", overwrite = TRUE)

# OVERALL --------------

grit.overall.4yrgrad <- 
  glmer(GRAD_Any_in_4YRS ~ 
          grit.c.Z + 
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter,
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

grit.overall.6yrgrad <- 
  glmer(GRAD_Any_in_6YRS ~ 
          grit.c.Z + 
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter,
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

Tmath.overall.4yrgrad <- 
  glmer(GRAD_Any_in_4YRS ~ 
          Tmath.c.Z + 
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter,
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )

Tmath.overall.6yrgrad <- 
  glmer(GRAD_Any_in_6YRS ~ 
          Tmath.c.Z + 
        + ravens.strict.c.Z_MR + ravens.strict.c.Z_MD 
        + (1 | schoolID), 
        data = refbias.charter,
        family=binomial(link="logit"),
        nAGQ=1,
        control = glmerControl(optimizer = "Nelder_Mead")
  )





# Output section: ---------------------------------------------------------

model.list.NOMD <-
  list(
    grit.overall.4yrgrad,
    grit.overall.6yrgrad,
    Tmath.overall.4yrgrad,
    Tmath.overall.6yrgrad
  ) %>%
  setNames(                                      # Quick way to add names to above
    construct.all(c("Grit-", "TMath-"), # Outcome
                  c("4yr-", "6yr-"),             # 5 year or 1 year graduation
                  c("LevelStd", "OverallStd")   # Standardized 
    ))

# Check, good:
# model.list.NOMD %>% names

# Tidy models:
suppressWarnings(
  model.list.tidy.NOMD <- 
    model.list.NOMD %>% map(tidy)
)

# Variable list:
tidy.varlist.NOMD <-
  map(model.list.tidy.NOMD, pull, term) %>% 
  unlist %>% unique %>% sort

table.varlist.NOMD <-
  c("Intercept",
    # Grit, 2 variables:
    rep("Grit, Level 1 (Within-School)", 2), 
    # non-transformed math, 2 variables:
    rep("# Math Problems, Level 1 (Within-School)", 2), 
    # Self-control, 2 variables:
    rep("Self-Control, Level 1 (Within-School)", 2), 
    # Self-Regulation
    rep("Self-Regulation, Level 1 (Within-School)", 2), 
    # Transformed math, same:
    rep("# Math Problems, Level 1 (Within-School)", 2), 
    # Level 2:
    rep("Grit, Level 2 (Between-Schools)", 2),
    rep("# Math Problems, Level 2 (Between-Schools)", 2),
    rep("Self-Control, Level 2 (Between-Schools)", 2), 
    rep("Self-Regulation, Level 2 (Between-Schools)", 2), 
    rep("# Math Problems, Level 2 (Between-Schools)", 2),
    "Cognitive Missing Dummy",
    "Cognitive Ability (Raven's)",
    "School Random Intercept SD"
  )

length(table.varlist.NOMD) == length(tidy.varlist.NOMD)


# Create a lookup vector for use below, to convert names:
lookup.NOMD <- setNames(table.varlist.NOMD, tidy.varlist.NOMD)

# Standard review of model results w/summary ------------------------------

# Just to look at things if you want
# map(model.list.NOMD, summary)
map(model.list.NOMD, nobs) # all 802

# Table construction ----------------------------------------------


# Using the ConstructTable tool defined above, 
# makes a complete table for displaying the data:

# Make the tables (with OR's, probabilities, etc.)
model.list.calc.NOMD <- 
  model.list.tidy.NOMD %>% map(ConstructTable)
# Make the tables look nice:
model.list.format.NOMD <- 
  model.list.calc.NOMD %>% map(FormatTable, lookup.NOMD)


write.xlsx(model.list.format.NOMD, 
           file = "./Study1/Tables/2019.Model.Tables.4yr.strict.nomissing.xlsx")

