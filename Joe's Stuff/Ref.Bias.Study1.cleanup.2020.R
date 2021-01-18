####This file takes the output SPSS data and computes all the new math variables/demogs

# Last edited  Mar 11, 2020

# 0 PRELOAD ----------------------------------------------------
# 0.1 Library calls -------------------------------------------------------


# install.packages(c("car","reshape", "foreign", "Hmisc", "lme4", "HLMdiag", "aprstable", "MASS", "plyr", "mi", "sjPlot", "lmerTest", "memisc", "VGAM"))


require(car)        #Various regression functions

require(foreign)    #Import/export
require(haven)
require(openxlsx)


#MLM modeling
require(lme4)
# SEM
require(lavaan)

# General purpose tools:
require(tidyverse)
require(PsychMisc)

# 0.1 Function Definitions ------------------------------------------------


# 1 DATA IMPORT -----------------------------------------------------------
# 1.1 Load ----------------------------------------------------------------


refbias.load <- suppressWarnings(read.spss("./Study1/Data/20140928/refbias.20140928.FINAL.sav", 
                     trim.factor.names=T, to.data.frame=T, use.value.labels=F))

# refbias.load <- suppressWarnings(read.spss("/Users/jmo2276/Box Sync/Gates Project 1&2 NSC Data/Y2P3_n=1479_20140728.sav", 
                     # trim.factor.names=T, to.data.frame=T, use.value.labels=F))

# 1.2 school variables (numeric)  ----------------------------------------

#sets public schools for validation study:
x <- c("1", "2"); names(x) <- c("NEHS", "UDHS")

#Gets names and numbers from imported SPSS labels and recodes into ordered numbers:
attributes(refbias.load$School)$value.labels <- 
  c(x, attributes(refbias.load$School)$value.labels)
y <- as.numeric(attributes(refbias.load$School)$value.labels)
names(y) <- names(attributes(refbias.load$School)$value.labels)

school.ref <- 1

# for each number that is the school ID in School:
for (i in unique(refbias.load$School)){
  # Take the set of Campuses that are available:
  for (j in unique(refbias.load$Campus[refbias.load$School==i])){
    # Write a sequentially increasing number for that school:
    refbias.load$schoolID[refbias.load$School==i & refbias.load$Campus==j] <- school.ref
    # Paste together the contents of the school name and campus name (if applicable):
    refbias.load$schoolID.name[refbias.load$School==i & refbias.load$Campus==j] <- paste0(names(y)[y==i], " - ", j)
    # Increment the school number
    school.ref <- school.ref + 1    
  }
}

# table(refbias.load$schoolID.name)
# table(refbias.load$Campus)
# unique(refbias.load$schoolID.name)

# 1.2.2 Merge in 5 year data ----------------------------------------------------

# This data provides a cross-walk to the 5-year data being merged in.
# y2.references <-         
#   read.xlsx("~/Box Sync/Project IDs/Y2P3_Yeager_Gates_Project_IDs.xlsx")
# # Get rid of duplicated column
# y2.references$Check <- NULL

y2.references2 <-         
  read_dta("./Study1/2020 followup data/Y2P3_Project_IDs_for_NSC_Merge.dta")

# This provides the 5-year graduation data:
dat.6year <- 
  read_dta("./Study1/2020 followup data/Gates_Joseph_UT_Y2P3.dta") %>% 
  select(Gates_Cohort, ChildID,
         Grad_24yr_Coll_ANY, Grad_ANY, 
         GRAD_Any_in_4YRS, GRAD_Any_in_5YRS, GRAD_Any_in_6YRS,
         GRAD_AssocBach_in_4YRS, GRAD_AssocBach_in_5YRS, GRAD_AssocBach_in_6YRS,
         GRAD_Bach_in_4YRS, GRAD_Bach_in_5YRS, GRAD_Bach_in_6YRS, Grad_24yr_Coll_ANY,
         Earn_Assoc_or_Bach_100P, Earn_Assoc_or_Bach_125P, Earn_Assoc_or_Bach_150P, Earn_Assoc_or_Bach_200P)

# nrow(refbias.load)
# addmargins(table(refbias.load$contenr_s14, useNA = "ifany")) # 2393 cases with 1-year outcome data

# Merge, Keep only those for whom we have 5-year outcome data:
refbias.load <- 
  merge(
    refbias.load %>% select(-ChildID),
    y2.references2 %>% select(user_id_perts, ChildID),
    by.x = "user_id_perts_nsc",
    by.y = "user_id_perts"
    
  )

# nrow(refbias.load) # Now 1470 (back to Y2P3?)

refbias.load <-
  merge(
  refbias.load,
  dat.6year,
  by = "ChildID"
  # ,all.x = TRUE
  )

# nrow(refbias.load) # Still 1470


# 1.3 charter schools -----------------------------------------------------


# unique(refbias.load$schoolID.name);length(.Last.value)
refbias.load$charter <- ifelse(refbias.load$schoolID %in% c(1, 2, 18, 19), 0, 1)
charters <- refbias.load$charter==1



# 1.3 Other prework (for mplus) -------------------------------------------

# No longer needed as lavaan used to calculate latent factors.
## create reference number in main dataset
# refbias.load$case.num <- 1:nrow(refbias.load)
refbias.load <-
  refbias.load %>% 
  select(-matches("name"), -matches("Campus"))



# save(refbias.load, charters, file = "./Study1/Data/R.Datafiles/2020-4to6year.Graduation.Analysis.Rawdata.rda")
load("./Study1/Data/R.Datafiles/2020-4to6year.Graduation.Analysis.Rawdata.rda")

# 2 MAIN VARIABLE CREATION -------------------------------------------------
# 2.1 create dataset ------------------------------------------------------

refbias <- refbias.load

# 2.2 Math (ADT) variable creation ----------------------------------------
# 2.2.1 Initial calculation -----------------------------------------------

# CHECKS COMMENTED OUT SO CAN RUN AS SCRIPT
# Some people are missing the ADT entirely:
# with(refbias, any(is.na(NumCorr_1) & is.na(NumCorr_2)))
# but everyone who has one block has the second block, too:
# with(refbias, any(is.na(NumCorr_1) + is.na(NumCorr_2) == 1))

## Math, number correct from 2 trials.  (don't need to account for NA's due to the above)
refbias$math <- with(refbias, NumCorr_1/2 + NumCorr_2/2)

## data was pre-fixed for single block missingness (non-compliance), but still have 533 missing cases
# length(with(refbias, which(is.na(NumCorr_1)&is.na(NumCorr_2)))) # n=533


## Square Root Transformed 2 trials:
refbias$Tmath <- sqrt(refbias$math+1)



## Z-scored versions of math:
# refbias$math.Z <- zScore(refbias$math)
refbias[charters, "math.c.Z"] <- zScore(refbias[charters, "math"])
# refbias$Tmath.Z  <- zScore(refbias$Tmath)
refbias[charters, "Tmath.c.Z"] <- zScore(refbias[charters, "Tmath"])



# 2.2.2 L1/L2 variable creation ---------------------------------------------------

## Group centering, Full Data (uses custom EBmeans function from ADRGtools)

# refbias$L2.eb.math.Z <- EBmeans(refbias, math.Z, schoolID)
# refbias$L2.eb.Tmath.Z <- EBmeans(refbias, Tmath.Z, schoolID)

# refbias$L2.math.Z <- GroupMeans(refbias, math.Z, schoolID)
# refbias$L2.Tmath.Z <- GroupMeans(refbias, Tmath.Z, schoolID)

# Group Centering, charters only

refbias$L2.eb.math.c.Z[charters] <- 
  EBmeans(refbias[charters,], math.c.Z, schoolID)
refbias$L2.eb.Tmath.c.Z[charters] <- 
  EBmeans(refbias[charters,], Tmath.c.Z, schoolID)

refbias$L2.math.c.Z[charters] <- 
  GroupMeans(refbias[charters,], math.c.Z, schoolID)
refbias$L2.Tmath.c.Z[charters] <- 
  GroupMeans(refbias[charters,], Tmath.c.Z, schoolID)


#L1 Variable Creation (uses EBcenter - centers data by group at EB mean)

# refbias$L1.eb.math.Z <- EBcenter(refbias, math.Z, schoolID)
# refbias$L1.eb.Tmath.Z <- EBcenter(refbias, Tmath.Z, schoolID)

# refbias$L1.math.Z <- GroupCenter(refbias, math.Z, schoolID)
# refbias$L1.Tmath.Z <- GroupCenter(refbias, Tmath.Z, schoolID)


refbias$L1.eb.math.c.Z[charters] <- 
  EBcenter(refbias[charters,], math.c.Z, schoolID)
refbias$L1.eb.Tmath.c.Z[charters] <- 
  EBcenter(refbias[charters,], Tmath.c.Z, schoolID)

refbias$L1.math.c.Z[charters] <- 
  GroupCenter(refbias[charters,], math.c.Z, schoolID)
refbias$L1.Tmath.c.Z[charters] <- 
  GroupCenter(refbias[charters,], Tmath.c.Z, schoolID)
  


# Level 2 Z-scoring (re-standardizing so can estimate ß-like coefficient):
# Uses zScoreLong - z-scores variables at the group level in long-form data where they are repeated:
# refbias$L2.eb.math.ZZ <- zScoreLong(refbias, L2.eb.math.Z, schoolID)
# refbias$L2.eb.Tmath.ZZ <- zScoreLong(refbias, L2.eb.Tmath.Z, schoolID)
# refbias$L2.math.ZZ <- zScoreLong(refbias, L2.math.Z, schoolID)
# refbias$L2.Tmath.ZZ <- zScoreLong(refbias, L2.Tmath.Z, schoolID)

refbias$L2.eb.math.c.ZZ <- zScoreLong(refbias, L2.eb.math.c.Z, schoolID)
refbias$L2.eb.Tmath.c.ZZ <- zScoreLong(refbias, L2.eb.Tmath.c.Z, schoolID)
refbias$L2.math.c.ZZ <- zScoreLong(refbias, L2.math.c.Z, schoolID)
refbias$L2.Tmath.c.ZZ <- zScoreLong(refbias, L2.Tmath.c.Z, schoolID)


# Level 1 Z-scoring (Basic):

# refbias$L1.eb.math.ZZ <- zScore(refbias$L1.eb.math.Z)
# refbias$L1.eb.Tmath.ZZ <- zScore(refbias$L1.eb.Tmath.Z)
# refbias$L1.math.ZZ <- zScore(refbias$L1.math.Z)
# refbias$L1.Tmath.ZZ <- zScore(refbias$L1.Tmath.Z)

refbias$L1.eb.math.c.ZZ <- zScore(refbias$L1.eb.math.c.Z)
refbias$L1.eb.Tmath.c.ZZ <- zScore(refbias$L1.eb.Tmath.c.Z)
refbias$L1.math.c.ZZ <- zScore(refbias$L1.math.c.Z)
refbias$L1.Tmath.c.ZZ <- zScore(refbias$L1.Tmath.c.Z)


# 2.3 Self-report creation (in R) ---------------------------------------------------
# 2.3.1 Initial creation-------------------------------------------------------


# all(refbias$Grit == makevar(refbias, paste0("grit", 1:4)), na.rm=T)
# refbias$Grit #already calculated
refbias$grit.c[charters] <- refbias$Grit[charters]

# all(refbias$SelfControlWork == makevar(refbias, paste0("selfcont", 1:4)), na.rm=T)
# refbias$SelfControlWork #already calculated
refbias$sc.c[charters] <- refbias$SelfControlWork[charters]

# Just going with the three since those are the perseverence items, avoiding the 
# conversation about the other facet + the fact that we have only 1 item for that portion
grit.mod.spec <- "grit.f =~ grit1 + grit2 + grit4"
grit.mod.full.spec <- "grit.f =~ grit1 + grit2 + grit3 + grit4"

sc.mod.spec <- "sc.f =~ selfcont1 + selfcont2 + selfcont3 + selfcont4"

selfreg.mod.spec <- 
  "
selfreg =~ a*grit.f + a*sc.f
   grit.f =~ grit1 + grit2 + grit4
   sc.f =~ selfcont1 + selfcont2 + selfcont3 + selfcont4"

selfreg.mod.full.spec <- 
  "
selfreg =~ a*grit.f + a*sc.f
   grit.f =~ grit1 + grit2 + grit3 + grit4
   sc.f =~ selfcont1 + selfcont2 + selfcont3 + selfcont4"

# (with(refbias, is.na(grit1) + is.na(grit2) + is.na(grit3) + is.na(grit4)) == 4) %>% table 
# 116 cases where all grit missing

# Warnings suppressed as they only tell that there are missing cases:
# suppressWarnings(
# # Calculate the grit score based on the 3-item model above 
# grit.mod <-
# sem(grit.mod.spec,
#     data = refbias,
#     missing = "fiml",
#     std.lv = TRUE, estimator = "MLR"
# ))

# charter-only version:
suppressWarnings(
grit.mod.c <-
sem(grit.mod.spec,
    data = refbias %>% filter(charters),
    missing = "fiml",
    std.lv = TRUE, estimator = "MLR"
))

# # summary(grit.mod)
# # fitmeasures(grit.mod, c("rmsea", "tli")) #irrelevant since it's a saturated model
# summary(grit.mod.c)
# fitmeasures(grit.mod.c, c("rmsea", "tli"))

# charter-only version:
suppressWarnings(
grit.mod.full.c <-
sem(grit.mod.full.spec,
    data = refbias %>% filter(charters),
    missing = "fiml",
    std.lv = TRUE, estimator = "MLR"
))

# summary(grit.mod.full.c, fit.measures = TRUE)
# fitmeasures(grit.mod.full.c, c("rmsea", "tli")) # .000, 1.005


# sc.mod <-
# sem(sc.mod.spec,
#     data = refbias,
#     missing = "fiml",
#     std.lv = TRUE, estimator = "MLR"
# )

suppressWarnings(
sc.mod.c <-
sem(sc.mod.spec,
    data = refbias %>% filter(charters),
    missing = "fiml",
    std.lv = TRUE, estimator = "MLR"
))

# # summary(sc.mod)
# # fitmeasures(sc.mod, c("rmsea", "tli")) # .050 .979
# # modindices(sc.mod, sort. = TRUE)
# summary(sc.mod.c)
# fitmeasures(sc.mod.c, c("rmsea", "tli")) # .050 .980

# selfreg.mod <-
# sem(selfreg.mod.spec,
#     data = refbias 
#     , missing = "fiml"
#     # , std.lv = TRUE
#     
# )

suppressWarnings(
selfreg.mod.c <-
sem(selfreg.mod.spec,
    data = refbias %>% filter(charters)
    , missing = "fiml"
    # , std.lv = TRUE
    
))

# # summary(selfreg.mod)
# # fitmeasures(selfreg.mod, c("rmsea", "tli")) #.067, .949
# summary(selfreg.mod.c)
# fitmeasures(selfreg.mod.c, c("rmsea", "tli")) #.063, .956

suppressWarnings(
selfreg.mod.full.c <-
sem(selfreg.mod.full.spec,
    data = refbias %>% filter(charters)
    , missing = "fiml"
    # , std.lv = TRUE
    
))


# summary(selfreg.mod.full.c)
# fitmeasures(selfreg.mod.full.c, c("rmsea", "tli")) #.057, .960

# Alpha check:
# psych::alpha(refbias %>% select(matches("grit[1-4]")))
# psych::alpha(refbias %>% select(matches("grit[1|3]")))
# psych::alpha(refbias %>% select(matches("grit[124]")))
# fsnip(paste0("grit", 1:4))
# psych::alpha(refbias %>% select(matches("selfcont[1-4]")))
# psych::alpha(refbias %>% select(matches("selfcont[1-4]|grit[1-4]")))


# Write estimated factor scores to data:
# refbias$grit.f <- lavPredict(grit.mod)[,1]
refbias$grit.f.c[charters] <- lavPredict(grit.mod.full.c)[,1]

# refbias$sc.f <-  lavPredict(sc.mod)[,1]
refbias$sc.f.c[charters] <- lavPredict(sc.mod.c)[,1]

# refbias$selfreg.f <-  lavPredict(selfreg.mod)[,1]
refbias$selfreg.f.c[charters] <- lavPredict(selfreg.mod.full.c)[,1]

# construct zScores
refbias[#c("grit.f.Z", "sc.f.Z", "selfreg.f.Z",
          c("grit.c.Z", "sc.c.Z", "grit.f.c.Z", "sc.f.c.Z", "selfreg.f.c.Z")] <-
  refbias %>% 
  select(#c("grit.f", "sc.f", "selfreg.f",
          c("grit.c", "sc.c", "grit.f.c", "sc.f.c", "selfreg.f.c")) %>% 
  map(~zScore(.x))
  

# 2.2.3 arithmetic calc group centering ----------------------------------------------


## Group centering, Full Data

# refbias$L2.eb.grit.f.Z <- EBmeans(refbias, grit.f.Z, schoolID)
# refbias$L2.eb.sc.f.Z <- EBmeans(refbias, sc.f.Z, schoolID)

# refbias$L2.grit.f.Z <- GroupMeans(refbias, grit.f.Z, schoolID)
# refbias$L2.sc.f.Z <- GroupMeans(refbias, sc.f.Z, schoolID)

# Group Centering, charters only

refbias$L2.eb.grit.c.Z[charters] <- 
  EBmeans(refbias[charters,], grit.c.Z, schoolID)
refbias$L2.eb.sc.c.Z[charters] <- 
  EBmeans(refbias[charters,], sc.c.Z, schoolID)

refbias$L2.eb.grit.f.c.Z[charters] <- 
  EBmeans(refbias[charters,], grit.f.c.Z, schoolID)
refbias$L2.eb.sc.f.c.Z[charters] <- 
  EBmeans(refbias[charters,], sc.f.c.Z, schoolID)
refbias$L2.eb.selfreg.f.c.Z[charters] <- 
  EBmeans(refbias[charters,], selfreg.f.c.Z, schoolID)

# refbias$L2.grit.f.c.Z[charters] <- 
#   GroupMeans(refbias[charters,], grit.f.c.Z, schoolID)
# refbias$L2.sc.f.c.Z[charters] <- 
#   GroupMeans(refbias[charters,], sc.f.c.Z, schoolID)
# refbias$L2.selfreg.f.c.Z[charters] <- 
#   GroupMeans(refbias[charters,], selfreg.f.c.Z, schoolID)


#L1 Variable Creation

# refbias$L1.eb.grit.f.Z <- EBcenter(refbias, grit.f.Z, schoolID)
# refbias$L1.eb.sc.f.Z <- EBcenter(refbias, sc.f.Z, schoolID)
# 
# refbias$L1.grit.f.Z <- GroupCenter(refbias, grit.f.Z, schoolID)
# refbias$L1.sc.f.Z <- GroupCenter(refbias, sc.f.Z, schoolID)

refbias$L1.eb.grit.c.Z[charters] <- 
  EBcenter(refbias[charters,], grit.c.Z, schoolID)
refbias$L1.eb.sc.c.Z[charters] <- 
  EBcenter(refbias[charters,], sc.c.Z, schoolID)

refbias$L1.eb.grit.f.c.Z[charters] <- 
  EBcenter(refbias[charters,], grit.f.c.Z, schoolID)
refbias$L1.eb.sc.f.c.Z[charters] <- 
  EBcenter(refbias[charters,], sc.f.c.Z, schoolID)
refbias$L1.eb.selfreg.f.c.Z[charters] <- 
  EBcenter(refbias[charters,], selfreg.f.c.Z, schoolID)

# refbias$L1.grit.f.c.Z[charters] <- 
#   GroupCenter(refbias[charters,], grit.f.c.Z, schoolID)
# refbias$L1.sc.f.c.Z[charters] <- 
#   GroupCenter(refbias[charters,], sc.f.c.Z, schoolID)
# refbias$L1.selfreg.f.c.Z[charters] <- 
#   GroupCenter(refbias[charters,], selfreg.f.c.Z, schoolID)
  


# Level 2 Z-scoring:

# refbias$L2.eb.grit.f.ZZ <- zScoreLong(refbias, L2.eb.grit.f.Z, schoolID)
# refbias$L2.eb.sc.f.ZZ <- zScoreLong(refbias, L2.eb.sc.f.Z, schoolID)
# refbias$L2.grit.f.ZZ <- zScoreLong(refbias, L2.grit.f.Z, schoolID)
# refbias$L2.sc.f.ZZ <- zScoreLong(refbias, L2.sc.f.Z, schoolID)

refbias$L2.eb.grit.c.ZZ <- zScoreLong(refbias, L2.eb.grit.c.Z, schoolID)
refbias$L2.eb.sc.c.ZZ <- zScoreLong(refbias, L2.eb.sc.c.Z, schoolID)

refbias$L2.eb.grit.f.c.ZZ <- zScoreLong(refbias, L2.eb.grit.f.c.Z, schoolID)
refbias$L2.eb.sc.f.c.ZZ <- zScoreLong(refbias, L2.eb.sc.f.c.Z, schoolID)
refbias$L2.eb.selfreg.f.c.ZZ <- zScoreLong(refbias, L2.eb.selfreg.f.c.Z, schoolID)

# refbias$L2.grit.f.c.ZZ <- zScoreLong(refbias, L2.grit.f.c.Z, schoolID)
# refbias$L2.sc.f.c.ZZ <- zScoreLong(refbias, L2.sc.f.c.Z, schoolID)


# Level 1 Z-scoring:

# refbias$L1.eb.grit.f.ZZ <- zScore(refbias$L1.eb.grit.f.Z)
# refbias$L1.eb.sc.f.ZZ <- zScore(refbias$L1.eb.sc.f.Z)
# refbias$L1.grit.f.ZZ <- zScore(refbias$L1.grit.f.Z)
# refbias$L1.sc.f.ZZ <- zScore(refbias$L1.sc.f.Z)

refbias$L1.eb.grit.c.ZZ <- zScore(refbias$L1.eb.grit.c.Z)
refbias$L1.eb.sc.c.ZZ <- zScore(refbias$L1.eb.sc.c.Z)

refbias$L1.eb.grit.f.c.ZZ <- zScore(refbias$L1.eb.grit.f.c.Z)
refbias$L1.eb.sc.f.c.ZZ <- zScore(refbias$L1.eb.sc.f.c.Z)
refbias$L1.eb.selfreg.f.c.ZZ <- zScore(refbias$L1.eb.selfreg.f.c.Z)

# refbias$L1.grit.f.c.ZZ <- zScore(refbias$L1.grit.f.c.Z)
# refbias$L1.sc.f.c.ZZ <- zScore(refbias$L1.sc.f.c.Z)
# refbias$L1.selfreg.f.c.ZZ <- zScore(refbias$L1.selfreg.f.c.Z)





# 3 COVARIATES - cognition ----------------------------------------------
# 3.1 Ravens ------------------------------------------------------------

# Ravens average:
# refbias$ravens <-
#   refbias %>% 
#   select(c("s2.ravens.10", "s2.ravens.11", "s2.ravens.12", "s2.ravens.13", 
#                      "s2.ravens.14", "s2.ravens.15", "s2.ravens.16", 
#                      "s2.ravens.17", "s2.ravens.18", "s2.ravens.7", 
#                      "s2.ravens.8", "s2.ravens.9")) %>% 
#   rowMeans(na.rm=T)

# Ravens average, treating missing items as failures (i.e., skipped b/c didn't know answer)
refbias$ravens.strict <- 
  apply(refbias %>% 
          select(c("s2.ravens.10", "s2.ravens.11", "s2.ravens.12", "s2.ravens.13", 
                     "s2.ravens.14", "s2.ravens.15", "s2.ravens.16", 
                     "s2.ravens.17", "s2.ravens.18", "s2.ravens.7", 
                     "s2.ravens.8", "s2.ravens.9")),
        1, function(x){ifelse(all(is.na(x)),
                              NA,
                              sum(x, na.rm=T)/12
        )})
  
## center variables:
# refbias$ravens.Z <- zScore(refbias$ravens)
# refbias$ravens.strict.Z <- zScore(refbias$ravens.strict)
# refbias[charters, "ravens.c.Z"] <- zScore(refbias[charters, "ravens"])
refbias[charters, "ravens.strict.c.Z"] <- 
  zScore(refbias[charters, "ravens.strict"])

# ravens.mod.spec <-
#   "ravens =~ ravens7 + ravens8 + ravens9 + ravens10 + ravens11 + 
#              ravens12 + ravens13 + ravens14 + ravens15 + ravens16 + 
#              ravens17 + ravens18
# "
# 
# # Doesn't work due to missingness:
# ravens.mod <-
#   sem(
#     ravens.mod.spec,
#     data = refbias,
#     # missing = "fiml",
#     ordered = c("ravens7", "ravens8", "ravens9", "ravens10", 
#                 "ravens11", "ravens12", "ravens13", "ravens14", 
#                 "ravens15", "ravens16", "ravens17", "ravens18"),
#     std.lv = TRUE)
#     

# 3.2 Mplus cognition ---------------------------------------------------

# No longer used


# 4 MISSINGNESS -----------------------------------------------------------


# 4.1 missing-removed variables -------------------------------------------


allvars <- c(
  # Makes all the predictor variables:
  construct.all(c("L1."), 
                # c("", "eb."), 
                c("eb."), 
                c("math.", "Tmath.", "grit.", "sc.", "grit.f.", "sc.f.", "selfreg.f."),
                # c("", "c."),
                c("c."),
                c("Z", "ZZ")
), # Adds the ravens items:
c(
  # "ravens.Z", "ravens.strict.Z", 
  # "ravens.c.Z", 
  "Tmath.c.Z",
  "grit.c.Z",
  "sc.c.Z",
  "selfreg.f.c.Z",
  "ravens.strict.c.Z")
)
              


refbias <-
  refbias %>% 
  mutate_at(
    .vars = vars(allvars),
    .funs = funs(MR = ifelse(is.na(.), 0, .),
                 MD = ifelse(is.na(.), 1, 0))
  )
                       
    

# 4.2 Charter dataset ---------------------------------------------------------

refbias.charter <- 
  refbias %>% 
  filter(charters) %>% 
  mutate(schoolID = schoolID - 2)


refbias.charter <-
  refbias.charter %>% 
  select(schoolID,
         contenr_s14,
         Grad_24yr_Coll_ANY, Grad_ANY, 
         GRAD_Any_in_4YRS, GRAD_Any_in_5YRS, GRAD_Any_in_6YRS,
         GRAD_AssocBach_in_4YRS, GRAD_AssocBach_in_5YRS, GRAD_AssocBach_in_6YRS,
         GRAD_Bach_in_4YRS, GRAD_Bach_in_5YRS, GRAD_Bach_in_6YRS, Grad_24yr_Coll_ANY,
         Earn_Assoc_or_Bach_100P, Earn_Assoc_or_Bach_125P, Earn_Assoc_or_Bach_150P, Earn_Assoc_or_Bach_200P,
         Tmath, math, ravens.strict,
         contains("grit.f.c"), contains("grit.c"), 
         contains("sc.f.c"), contains("sc.c"), 
         contains("selfreg.f.c"),
         contains("ravens.strict"),
         contains("math.c"), 
         # grit.c, sc.c, selfreg.f.c, 
         # grit.f.c, sc.f.c, selfreg.f.c, 
         # grit.c.Z, sc.c.Z, 
         # grit.f.c.Z, sc.f.c.Z, selfreg.f.c.Z, Tmath.c.Z, math.c.Z, ravens.strict.c.Z,
         # ravens.strict.c.Z_MR, ravens.strict.c.Z_MD,   
         # L1.eb.grit.c.Z_MR, L2.eb.grit.c.Z,
         # L1.eb.grit.c.ZZ_MR, L2.eb.grit.c.ZZ, L1.eb.grit.c.Z_MD,
         # L1.eb.sc.c.Z_MR, L2.eb.sc.c.Z, 
         # L1.eb.sc.c.ZZ_MR, L2.eb.sc.c.ZZ, L1.eb.sc.c.Z_MD,
         # L1.eb.grit.f.c.Z_MR, L2.eb.grit.f.c.Z,
         # L1.eb.grit.f.c.ZZ_MR, L2.eb.grit.f.c.ZZ, L1.eb.grit.f.c.Z_MD,
         # L1.eb.sc.f.c.Z_MR, L2.eb.sc.f.c.Z, 
         # L1.eb.sc.f.c.ZZ_MR, L2.eb.sc.f.c.ZZ, L1.eb.sc.f.c.Z_MD,
         # L1.eb.selfreg.f.c.Z_MR, L2.eb.selfreg.f.c.Z,
         # L1.eb.selfreg.f.c.ZZ_MR, L2.eb.selfreg.f.c.ZZ, L1.eb.selfreg.f.c.Z_MD,
         # L1.eb.Tmath.c.Z_MR, L2.eb.Tmath.c.Z,
         # L1.eb.Tmath.c.ZZ_MR, L2.eb.Tmath.c.ZZ, L1.eb.Tmath.c.Z_MD,
         # L1.eb.math.c.Z_MR, L2.eb.math.c.Z,
         # L1.eb.math.c.ZZ_MR, L2.eb.math.c.ZZ, L1.eb.math.c.Z_MD,
         # L1.eb.grit.f.c.Z, L2.eb.grit.f.c.Z,   
         # L1.eb.grit.f.c.ZZ, L2.eb.grit.f.c.ZZ,
         # L1.eb.sc.f.c.Z, L2.eb.sc.f.c.Z, 
         # L1.eb.sc.f.c.ZZ, L2.eb.sc.f.c.ZZ,   
         # L1.eb.selfreg.f.c.Z, L2.eb.selfreg.f.c.Z,  
         # L1.eb.selfreg.f.c.ZZ, L2.eb.selfreg.f.c.ZZ,
         # L1.eb.Tmath.c.Z, L2.eb.Tmath.c.Z,  
         # L1.eb.Tmath.c.ZZ, L2.eb.Tmath.c.ZZ,    
         # L1.eb.math.c.Z, L2.eb.math.c.Z,  
         # L1.eb.math.c.ZZ, L2.eb.math.c.ZZ
  )



refbias.charter.selfreport.nomiss <- 
  refbias.charter %>% 
  filter(!is.na(grit.f.c))

refbias.charter.all.nomiss <- 
  refbias.charter %>% 
  filter(!is.na(grit.f.c), !is.na(math))



save(refbias.charter, refbias.charter.selfreport.nomiss, refbias.charter.all.nomiss,
     file = "./Study1/Data/R.Datafiles/2020-Graduation.Analysis.Data.4-6yr.rda")
# save(refbias.charter, refbias.charter.selfreport.nomiss, refbias.charter.all.nomiss,
#      file = "./2020 4yr only/Data/2019-4yr.Graduation.Analysis.Data.rda")

nrow(refbias.charter) # 1278
nrow(refbias.charter.selfreport.nomiss) # 1166 w/grit score - 112
nrow(refbias.charter.all.nomiss) # 802 w/everything

# # Merge in 5 year data ----------------------------------------------------
# 
# y2.references <-         
#   read.xlsx("~/Box Sync/Project IDs/Y2P3_Yeager_Gates_Project_IDs.xlsx")
# # Get rid of duplicated column
# y2.references$Check <- NULL
# 
# dat.5year <- 
#   read_dta("./Study1/2018 followup data/Oct_2018_Charter_AllSchools_Dataset.dta")
# 
# 
# refbias.charter$user_id_perts_nsc <- 
#   as.character(refbias.charter$user_id_perts_nsc)
# 
# nrow(refbias.charter)
# refbias.charter.gradids <- 
#   merge(
#     refbias.charter,
#     y2.references %>% select(user_id_perts, NSC_idnum),
#     by.x = "user_id_perts_nsc",
#     by.y = "user_id_perts",
#     all.x = TRUE
#   )
# 
# 
# nrow(refbias.charter.gradids)
# 
# refbias.charter.grad <-
#   merge(
#   refbias.charter.gradids,
#   dat.5year,
#   by.x = "NSC_idnum",
#   by.y = "ChildID"
#   ,all.x = TRUE
#   )
# 
# nrow(refbias.charter.grad)
# 
# y2.references$user_id_perts
# 
# duplicate.IDs <-
# table(refbias.charter.grad$user_id_perts)[table(refbias.charter.grad$user_id_perts) >1] %>% names
# View(refbias.charter.grad[which(refbias.charter.grad$user_id_perts %in% duplicate.IDs),]
# 
# 
# # Same:
# duplicate.IDs.nsc <-
# table(refbias.charter$user_id_perts_nsc)[table(refbias.charter$user_id_perts_nsc) >1] %>% names
# which(refbias.charter$user_id_perts_nsc %in% duplicate.IDs)
# 
# head(refbias.charter$user_id_perts_nsc)
# 
# levels(refbias.charter$user_id_perts_nsc)
# 
# which(duplicated(refbias.charter.grad$user_id_perts))
# which(duplicated(refbias.charter.grad$user_id_perts_nsc))
# which(is.na(refbias.charter.grad$NSC_idnum))
# 
# 
# testref <-
# refbias.charter.grad[which(is.na(refbias.charter.grad$NSC_idnum)),] %>% 
#   arrange(user_id_perts_nsc)
# 
#                 
# View(testref)

# Diagnostic plots (not used) -------------------------------------------------------

# # Checking
# old.par  <- par(no.readonly=TRUE)
# pdf("./Plots/DiagnosticPlots/Outliers.pdf")
# 
# par(mfrow = c(2,2))
# 
# 
# plot(refbias$NumCorr_1, main = "Outlier plot, block 1")
# abline(h=math.mean2.1 + 3*math.sd2.1)
# 
# plot(refbias$NumCorr_2, main = "Outlier plot, block 2")
# abline(h=math.mean2.2 + 3*math.sd2.2)
# 
# plot(refbias$math, main = "Outlier plot overall (2-block average)")
# abline(h=math.mean + 3*math.sd)
# 
# par(old.par)
# dev.off()
