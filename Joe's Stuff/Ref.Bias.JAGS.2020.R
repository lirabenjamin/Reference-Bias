library(rjags)
library(runjags)
require(dplyr)
require(parallel)

# Loaded Refbias cleanup file
load("./Study1/Data/R.Datafiles/2020-Graduation.Analysis.Data.4-6yr.rda")

refbias.char

# 1166 if we drop grit
refbias.charter %>% 
  group_by(is.na(grit.c.Z), is.na(GRAD_Any_in_4YRS), is.na(GRAD_Any_in_6YRS)) %>% 
  summarize(n())

jags.master <- 
  refbias.charter %>% 
  select(schoolID, math = Tmath.c.Z, grit = grit.c.Z, 
         grad4 = GRAD_Any_in_4YRS, grad6 = GRAD_Any_in_6YRS,
         ravens = ravens.strict.c.Z_MR, ravens.MD = ravens.strict.c.Z_MD) %>% 
  filter(!is.na(grit), !is.na(grad4))

jags.grit <- jags.master %>% 
  filter(!is.na(grit))
jags.math <- jags.master %>% 
  filter(!is.na(math))


nrow(refbias.charter) # 1166
nrow(jags.grit) # 1166 (none missing under current clean)
nrow(jags.math) # 802 (364 w/o ADT)



# Functions to run JAGS models ----------


# Lists parameters to follow
parameters <- c("beta.0", # Intercept
                "beta.w", # Within-school coefficient of grit/ADT
                "sigmaSqrd.w", # Within-school predictor variance
                
                "beta.b", # Between-school coefficient of grit/ADT
                "tauSqrd.b", # Between-school predictor variance 
                
                "beta.3", # Ravens 
                
                "delta", # School-level residual (intercept)
                "tauSqrd.p" # School-level residual (intercept) variance 
) %>% setNames(
  c("Intercept",
    "b.pred.within",
    "var.pred.within",
    "b.pred.between",
    "var.pred.between",
    "b.ravens",
    "schoolresid.intercept",
    "schoolresid.intercept.var"
  ))

# Function to quickly create multiple threads from different initialization points, to help check convergence:

init.list <- function(){
  list(
    list(beta.0 = runif(1), beta.w = runif(1),
         beta.b = runif(1)),
    list(beta.0 = -runif(1), beta.w = -runif(1),
         beta.b = -runif(1)),
    list(beta.0 = rnorm(1), beta.w = rnorm(1),
         beta.b = rnorm(1))
  )}


jags.run <- function(jags.model, jags.data){
  # Calls run.jags(), with consistent settings for all models.
  run.jags(
    model= jags.model,
    data = jags.data,
    monitor = parameters,
    n.chains = 3, 
    n.sims = 3,
    method = "rjags", 
    inits = init.list(),
    burnin = 30000, 
    sample = 30000, 
    plots = TRUE
  )
}


save(jags.grit, jags.math, init.list, jags.run, parameters, file = "./Study1/Data/R.Datafiles/2020-JAGS.Data.4-6yr.rda")


cl <- makeCluster(7)

# 4 year ----------------------------------------------

# NOTE: jags.run is just a wrapper around run.jags() with consistent parameters of
# 30k burn-in (high due to autocorrelation in the between-schools estimates
# and 30k samples, with plotting output. See above, and in 2019-JAGS.prep.R

jags.grit.4year.mod <- 
  jags.run(
    jags.model = "./Study1/R scripts/2020/2020-jags.grit.4year.jag",
    jags.data = jags.grit %>% select(-grad6)
  )


jags.math.4year.mod <- 
  jags.run(
    jags.model = "./Study1/R scripts/2020/2020-jags.math.4year.jag",
    jags.data = jags.math %>% select(-grad6)
  )

# 6 year ------------------------------------------------------

jags.grit.6year.mod <- 
  jags.run(
    jags.model = "./Study1/R scripts/2020/2020-jags.grit.6year.jag",
    jags.data = jags.grit %>% select(-grad4)
  )


jags.math.6year.mod <- 
  jags.run(
    jags.model = "./Study1/R scripts/2020/2020-jags.math.6year.jag",
    jags.data = jags.math %>% select(-grad4)
  )



# Save output to file -----------------------------------------------------

save(
  jags.grit.4year.mod, jags.grit.6year.mod, 
  jags.math.4year.mod, jags.math.6year.mod, 
  file = "./Study1/R scripts/2020/2020.jags.4-6year.rda"
)


# Review output -----------------------------------------------------------


plotvars <-
  c("beta.0", # Intercept
    "beta.w", # Within-school coefficient of grit/ADT
    "beta.b", # Between-school coefficient of grit/ADT
    "beta.3", # Ravens 
    "tauSqrd.p" # School-level residual (intercept) variance 
  )

vars.report <-
  c("beta.0", 
    "beta.w", 
    "beta.b", 
    "beta.3",
    "tauSqrd.p"
  )



summary(jags.grit.4year.mod, vars = vars.report) 
summary(jags.math.4year.mod, vars = vars.report)
summary(jags.grit.6year.mod, vars = vars.report) 
summary(jags.math.6year.mod, vars = vars.report) 

plot(jags.grit.4year.mod, vars = vars.report) 
plot(jags.math.4year.mod, vars = vars.report) 
plot(jags.grit.6year.mod, vars = vars.report) 
plot(jags.math.6year.mod, vars = vars.report) 
