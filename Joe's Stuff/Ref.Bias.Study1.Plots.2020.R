

# Library calls -----------------------------------------------------------

require(tidyverse)
require(DescTools)
require(ADRGtools)
require(gridExtra)


# Function definitions ----------------------------------------------------

#Formatting to drop leading zeros and round:

dropZero <- function(c){Format(x = c, leading = "drop", digits = 3)}


# Pulls out legend (from https://www.rdocumentation.org/packages/lemon/versions/0.4.3/topics/g_legend)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}




# Data cleaning/prep ------------------------------------------------------


refbias.graph <- refbias.charter


### Expand predictors back out to range of original measures (range 1 to 5), for plotting ###
# Grit:
refbias.graph$grit.remap <- 
  4 * range0to1(refbias.graph$grit.c.Z) + 1

# Self-control:
refbias.graph$sc.remap <-
  4 * range0to1(refbias.graph$sc.f.c.Z) + 1

# Self-regulation composite:
refbias.graph$selfreg.remap <-
  4 * range0to1(refbias.graph$selfreg.f.c.Z) + 1


### Same for the L2 variables so we can use them in the plot:

refbias.graph$L2.grit.remap <- 
  4 * range0to1(refbias.graph$grit.c.Z, newdata = refbias.graph$L2.eb.grit.c.Z) + 1

refbias.graph$L2.sc.remap <- 
  4 * range0to1(refbias.graph$sc.f.c.Z, newdata = refbias.graph$L2.eb.sc.f.c.Z) + 1

refbias.graph$L2.selfreg.remap <- 
  4 * range0to1(refbias.graph$selfreg.f.c.Z, newdata = refbias.graph$L2.eb.selfreg.f.c.Z) + 1

refbias.graph$L2.math.remap <- 
  172.5 * range0to1(refbias.graph$math.c.Z, newdata = refbias.graph$L2.eb.math.c.Z)

refbias.combined <- refbias.graph
refbias.combined$schoolID <- 16
refbias.graph.combined <- rbind(refbias.graph, refbias.combined)

# Prediction of values for plots ------------------------------------------

### Collecting mean values for predict functions:

# mean value for the persistence probability variable, by school:
refbias.graph.means <- 
  refbias.graph %>% 
  group_by(schoolID) %>% 
  summarise(GRAD_Any_in_4YRS = mean(GRAD_Any_in_4YRS, na.rm=T),
            GRAD_Any_in_6YRS = mean(GRAD_Any_in_6YRS, na.rm=T),
            math.remap = unique(L2.math.remap),
            grit.remap = unique(L2.grit.remap),
            sc.remap = unique(L2.sc.remap),
            selfreg.remap = unique(L2.selfreg.remap)
  )


### make model predictions based on the values, for points on plots:

for(i in unique(refbias.graph$schoolID)){
  
  # Predictions for 1 YEAR PERSISTENCE:
  
  # for grit:
  refbias.graph.means$grit.pred.4yr[i] <-  
    predict(
      glm(GRAD_Any_in_4YRS ~ grit.remap, 
          refbias.graph[refbias.graph$schoolID==i,], 
          family="binomial"
      ), 
      newdata=data.frame(grit.remap = refbias.graph.means$grit.remap[i]),
      type="response") 

  # for sc:
  refbias.graph.means$sc.pred.4yr[i] <-  
    predict(
      glm(GRAD_Any_in_4YRS ~ sc.remap, 
          refbias.graph[refbias.graph$schoolID==i,], 
          family="binomial"
      ), 
      newdata=data.frame(sc.remap = refbias.graph.means$sc.remap[i]),
      type="response") 
  
  # for selfreg:
  refbias.graph.means$selfreg.pred.4yr[i] <-  
    predict(
      glm(GRAD_Any_in_4YRS ~ selfreg.remap, 
          refbias.graph[refbias.graph$schoolID==i,], 
          family="binomial"
      ), 
      newdata=data.frame(selfreg.remap = refbias.graph.means$selfreg.remap[i]),
      type="response") 
  
  # for ADT:
  refbias.graph.means$math.pred.4yr[i] <-  
    predict(
      glm(GRAD_Any_in_4YRS ~ math, 
          refbias.graph[refbias.graph$schoolID==i,], 
          family="binomial"
      ), 
      newdata=data.frame(math = refbias.graph.means$math.remap[i]),
      type="response")
  
  # Predictions for 5-YEAR GRADUATION:
  
  # Grit
  refbias.graph.means$grit.pred.6yr[i] <-  
    predict(
      glm(GRAD_Any_in_6YRS ~ grit.remap, 
          refbias.graph[refbias.graph$schoolID==i,], 
          family="binomial"
      ), 
      newdata=data.frame(grit.remap = refbias.graph.means$grit.remap[i]),
      type="response") 
  
  # Self-Control
  refbias.graph.means$sc.pred.6yr[i] <-  
    predict(
      glm(GRAD_Any_in_6YRS ~ sc.remap, 
          refbias.graph[refbias.graph$schoolID==i,], 
          family="binomial"
      ), 
      newdata=data.frame(sc.remap = refbias.graph.means$sc.remap[i]),
      type="response") 
  
  # Self-Regulation
  refbias.graph.means$selfreg.pred.6yr[i] <-  
    predict(
      glm(GRAD_Any_in_6YRS ~ selfreg.remap, 
          refbias.graph[refbias.graph$schoolID==i,], 
          family="binomial"
      ), 
      newdata=data.frame(selfreg.remap = refbias.graph.means$selfreg.remap[i]),
      type="response") 
  
  # for ADT:
  refbias.graph.means$math.pred.6yr[i] <-  
    predict(
      glm(GRAD_Any_in_6YRS ~ math, 
          refbias.graph[refbias.graph$schoolID==i,], 
          family="binomial"
      ), 
      newdata=data.frame(math = refbias.graph.means$math.remap[i]),
      type="response")
  
}


# Graphing preconfig----------------------------------------------------------------

# Sets up legend names:
schoolIDs.name <- c(as.character(1:15), "Overall")

# Line types for each school + overall:
linetype.values <- c(rep(1,15),2)

# Colors for lines:
set.seed(007)
line.colors <- c(sample(rainbow(15, start=0)), "#000000", "#000000")



# Getting the L2 linear models for the plots:
grit.means.4yr.lm <- lm(grit.pred.4yr ~ grit.remap, refbias.graph.means)
grit.means.6yr.lm <- lm(grit.pred.6yr ~ grit.remap, refbias.graph.means)
math.means.4yr.lm <- lm(math.pred.4yr ~ math.remap, refbias.graph.means)
math.means.6yr.lm <- lm(math.pred.6yr ~ math.remap, refbias.graph.means)



# Annotation Label construction -------------------------------------------

cor.test(~ grit.pred.4yr + grit.remap, data =refbias.graph.means)

makeAnnotation <- function(outcome, predictor){
  form <- eval(substitute(formula(~ outcome + predictor)))
  cor.test(form, data = refbias.graph.means)[c("estimate", "p.value")] %>% 
    as_tibble %>% 
    mutate_all(funs(round(., 3) %>% dropZero)) %>% 
    pmap_chr(
      function(estimate, p.value){
        paste0("Between-school simple correlation\n(r = ", estimate, ", p = ", p.value, ")")
      })
}


grit.4yr.plot.text <- makeAnnotation(grit.pred.4yr, grit.remap) 
sc.4yr.plot.text <- makeAnnotation(sc.pred.4yr, sc.remap)
selfreg.4yr.plot.text <- makeAnnotation(selfreg.pred.4yr, selfreg.remap) 
math.4yr.plot.text <- makeAnnotation(math.pred.4yr, math.remap)
grit.6yr.plot.text <- makeAnnotation(grit.pred.6yr, grit.remap) 
sc.6yr.plot.text <- makeAnnotation(sc.pred.6yr, sc.remap)
selfreg.6yr.plot.text <- makeAnnotation(selfreg.pred.6yr, selfreg.remap) 
math.6yr.plot.text <- makeAnnotation(math.pred.6yr, math.remap)
    


# Plotting.Functions ----------------------------------------------

# Plot theme setup (allows changing all at once:)
plottheme <- function(newtheme = theme_minimal, family = "Helvetica", size = 20){
  annotation.size <<- size
  new.theme <- match.fun(newtheme)
  new.theme() +
    theme(
      legend.text = element_text(family = family, size=size*.8),
      legend.title = element_text(family = family, size=size*.9),
      axis.text = element_text(family = family, size=size),
      axis.title = element_text(family = family, size=size),
      plot.title = element_text(family = family, size=size * 25/20, hjust = 0.5)
    )
}


grit.4yr.cleanup.final <- function(plot){
  set.seed(007)
  plot +
    #VISUALS:
    #legend:
    scale_colour_manual(name="Individual School\nBest Fit Lines", breaks=1:16,
                        values=line.colors, labels=schoolIDs.name) +
    scale_fill_identity(name = 'Between-Schools\nBest Fit Line',
                        guide = "legend",labels = c('')) +
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 2)) + 
    
    #BW theme (less background, adds border)
    plottheme() + 
    #Axes:
    geom_vline(xintercept=1, alpha=.8, size=.8) +
    geom_hline(yintercept=0, alpha=.8, size=.8) +
    ylim(c(0,1)) + 
    #Axis labels:
    ylab("Probability of On-Time Degree Attainment (4 Years)") +
    xlab("Self-Reported Grit")
}

grit.6yr.cleanup.final <- function(plot){
  set.seed(007)
  plot +
    #VISUALS:
    #legend:
    scale_colour_manual(name="Individual School\nBest Fit Lines", breaks=1:16,
                        values=line.colors, labels=schoolIDs.name) +
    scale_fill_identity(name = 'Between-Schools\nBest Fit Line',
                        guide = "legend",labels = c('')) +
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 2)) +
    
    #BW theme (less background, adds border)
    plottheme() + 
    #axes:
    geom_vline(xintercept=1, alpha=.8, size=.8) +
    geom_hline(yintercept=0, alpha=.8, size=.8) +
    ylim(c(0,1)) + 
    #Axis labels:
    ylab("Probability of Degree Attainment Within 6 Years") +
    xlab("Self-Reported Grit")
}

sc.4yr.cleanup.final <- function(plot){
  set.seed(007)
  plot +
    #VISUALS:
    #legend:
    scale_colour_manual(name="Individual School\nBest Fit Lines", breaks=1:16,
                        values=line.colors, labels=schoolIDs.name) +
    scale_fill_identity(name = 'Between-Schools\nBest Fit Line',
                        guide = "legend",labels = c('')) +
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 2)) +
    
    #BW theme (less background, adds border)
    plottheme() + 
    #Axes:
    geom_vline(xintercept=1, alpha=.8, size=.8) +
    geom_hline(yintercept=0, alpha=.8, size=.8) +
    ylim(c(0,1)) + 
    #Axis labels:
    ylab("Probability of On-Time Degree Attainment (4 Years)") +
    xlab("Self-Reported Self-Control")
}

sc.6yr.cleanup.final <- function(plot){
  set.seed(007)
  plot +
    #VISUALS:
    #legend:
    scale_colour_manual(name="Individual School\nBest Fit Lines", breaks=1:16,
                        values=line.colors, labels=schoolIDs.name) +
    scale_fill_identity(name = 'Between-Schools\nBest Fit Line',
                        guide = "legend",labels = c('')) +
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 2)) +
    
    #BW theme (less background, adds border)
    plottheme() + 
    #axes:
    geom_vline(xintercept=1, alpha=.8, size=.8) +
    geom_hline(yintercept=0, alpha=.8, size=.8) +
    ylim(c(0,1)) + 
    #Axis labels:
    ylab("Probability of Degree Attainment Within 6 Years") +
    xlab("Self-Reported Self-Control")
}

selfreg.4yr.cleanup.final <- function(plot){
  set.seed(007)
  plot +
    #VISUALS:
    #legend:
    scale_colour_manual(name="Individual School\nBest Fit Lines", breaks=1:16,
                        values=line.colors, labels=schoolIDs.name) +
    scale_fill_identity(name = 'Between-Schools\nBest Fit Line',
                        guide = "legend",labels = c('')) +
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 2)) +
    
    #BW theme (less background, adds border)
    plottheme() + 
    #Axes:
    geom_vline(xintercept=1, alpha=.8, size=.8) +
    geom_hline(yintercept=0, alpha=.8, size=.8) +
    ylim(c(0,1)) + 
    #Axis labels:
    ylab("Probability of On-Time Degree Attainment (4 Years)") +
    xlab("Composite of Self-Reported Regulation")
}

selfreg.6yr.cleanup.final <- function(plot){
  set.seed(007)
  plot +
    #VISUALS:
    #legend:
    scale_colour_manual(name="Individual School\nBest Fit Lines", breaks=1:16,
                        values=line.colors, labels=schoolIDs.name) +
    scale_fill_identity(name = 'Between-Schools\nBest Fit Line',
                        guide = "legend",labels = c('')) +
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 2)) +
    
    #BW theme (less background, adds border)
    plottheme() + 
    #axes:
    geom_vline(xintercept=1, alpha=.8, size=.8) +
    geom_hline(yintercept=0, alpha=.8, size=.8) +
    ylim(c(0,1)) + 
    #Axis labels:
    ylab("Probability of Degree Attainment Within 6 Years") +
    xlab("Composite of Self-Reported Regulation")
  }

math.4yr.cleanup.final <- function(plot){
  set.seed(007)
  plot +
    #VISUALS:
    #legend:
    scale_colour_manual(name="Individual School\nBest Fit Lines", breaks=1:16,
                        values=line.colors, labels=schoolIDs.name) +
    scale_fill_identity(name = 'Between-Schools\nBest Fit Line',
                        guide = "legend",labels = c('')) +
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 2)) +
    
    #BW theme (less background, adds border)
    plottheme() + 
    #axes:
    geom_vline(xintercept=0, alpha=.8, size=.8) +
    geom_hline(yintercept=0, alpha=.8, size=.8) +
    ylim(c(0,1)) + 
    #Axis labels:
    ylab("Probability of On-Time Degree Attainment (4 Years)") + 
    xlab("Academic Diligence Task Score (# Math Problems Correct)")
}

math.6yr.cleanup.final <- function(plot){
  set.seed(007)
  plot +
    #VISUALS:
    #legend:
    scale_colour_manual(name="Individual School\nBest Fit Lines", breaks=1:16,
                        values=line.colors, labels=schoolIDs.name) +
    scale_fill_identity(name = 'Between-Schools\nBest Fit Line',
                        guide = "legend",labels = c('')) +
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 2)) +
    
    #BW theme (less background, adds border)
    plottheme() + 
    #axes:
    geom_vline(xintercept=0, alpha=.8, size=.8) +
    geom_hline(yintercept=0, alpha=.8, size=.8) + 
    ylim(c(0,1)) + 
    #Axis labels:
    ylab("Probability of Degree Attainment Within 6 Years") +
    xlab("Academic Diligence Task Score (# Math Problems Correct)")
}


# 4 YEAR ON-TIME GRADUATION PLOTS ------------------------------------------------


# Grit --------------------------------------------------------------------

grit.4yr.plot.noline <- 
  ggplot(
    #DATA  
    #IV/DV/Groupings
    refbias.graph.combined, 
    aes(x=grit.remap, y=GRAD_Any_in_4YRS, colour=as.factor(schoolID))) + 
  #logistic curves fror groups:
  stat_smooth(method="glm", method.args = list(family = "binomial"), 
              se=FALSE, fullrange=F, size=.6) + theme_bw() + 
  stat_smooth(method="glm", method.args = list(family = "binomial"), 
              se=FALSE, fullrange=T, linetype = "dotted", size=.6) + theme_bw()

grit.4yr.plot.points <- 
  grit.4yr.plot.noline +
  # school-level points (different data):
  geom_point(data=refbias.graph.means, 
             aes(x=grit.remap, 
                 y=grit.pred.4yr, 
                 colour=as.factor(schoolID))) 

grit.4yr.plot.between <- 
  grit.4yr.plot.points +
  #school level line (different data):
  geom_smooth(aes(x=grit.remap,
                  y=grit.pred.4yr,
                  fill="black"), 
              data=refbias.graph.means, 
              method="lm", color="black", 
              size=2, se=FALSE, fullrange=F) 



grit.4yr.plot.noline.final <- grit.4yr.cleanup.final(grit.4yr.plot.noline)
grit.4yr.plot.pointsonly.final <- grit.4yr.cleanup.final(grit.4yr.plot.points)
grit.4yr.plot.final <- grit.4yr.cleanup.final(grit.4yr.plot.between)

# Self-control --------------------------------------------------------------------

sc.4yr.plot.noline <- 
  ggplot(
    #DATA  
    #IV/DV/Groupings
    refbias.graph.combined, 
    aes(x=sc.remap, y=GRAD_Any_in_4YRS, colour=as.factor(schoolID))) + 
  #logistic curves fror groups:
  stat_smooth(method="glm", method.args = list(family = "binomial"), 
              se=FALSE, fullrange=F, size=.6) + theme_bw() + 
  stat_smooth(method="glm", method.args = list(family = "binomial"), 
              se=FALSE, fullrange=T, linetype = "dotted", size=.6) + theme_bw()

sc.4yr.plot.points <- 
  sc.4yr.plot.noline +
  # school-level points (different data):
  geom_point(data=refbias.graph.means, 
             aes(x=sc.remap, 
                 y=sc.pred.4yr, 
                 colour=as.factor(schoolID))) 

sc.4yr.plot.between <- 
  sc.4yr.plot.points +
  #school level line (different data):
  geom_smooth(aes(x=sc.remap,
                  y=sc.pred.4yr,
                  fill="black"), 
              data=refbias.graph.means, 
              method="lm", color="black", 
              size=2, se=FALSE, fullrange=F) 


sc.4yr.plot.noline.final <- sc.4yr.cleanup.final(sc.4yr.plot.noline)
sc.4yr.plot.pointsonly.final <- sc.4yr.cleanup.final(sc.4yr.plot.points)
sc.4yr.plot.final <- sc.4yr.cleanup.final(sc.4yr.plot.between)

# Self-Reg --------------------------------------------------------------------

selfreg.4yr.plot.noline <- 
  ggplot(
    #DATA  
    #IV/DV/Groupings
    refbias.graph.combined, 
    aes(x=selfreg.remap, y=GRAD_Any_in_4YRS, colour=as.factor(schoolID))) + 
  #logistic curves fror groups:
  stat_smooth(method="glm", method.args = list(family = "binomial"), 
              se=FALSE, fullrange=F, size=.6) + theme_bw() + 
  stat_smooth(method="glm", method.args = list(family = "binomial"), 
              se=FALSE, fullrange=T, linetype = "dotted", size=.6) + theme_bw()

selfreg.4yr.plot.points <- 
  selfreg.4yr.plot.noline +
  # school-level points (different data):
  geom_point(data=refbias.graph.means, 
             aes(x=selfreg.remap, 
                 y=selfreg.pred.4yr, 
                 colour=as.factor(schoolID))) 

selfreg.4yr.plot.between <- 
  selfreg.4yr.plot.points +
  #school level line (different data):
  geom_smooth(aes(x=selfreg.remap,
                  y=selfreg.pred.4yr,
                  fill="black"), 
              data=refbias.graph.means, 
              method="lm", color="black", 
              size=2, se=FALSE, fullrange=F) 



selfreg.4yr.plot.noline.final <- selfreg.4yr.cleanup.final(selfreg.4yr.plot.noline)
selfreg.4yr.plot.pointsonly.final <- selfreg.4yr.cleanup.final(selfreg.4yr.plot.points)
selfreg.4yr.plot.final <- selfreg.4yr.cleanup.final(selfreg.4yr.plot.between)


# ADT ---------------------------


math.4yr.plot.noline <- ggplot(
  #DATA  
  #IV/DV/Groupings
  refbias.graph.combined, aes(x=math, y=GRAD_Any_in_4YRS, colour=as.factor(schoolID))) + 
  #logistic curves fror groups:
  stat_smooth(method="glm", method.args = list(family="binomial"), se=FALSE, fullrange=F, size = .6) + 
  stat_smooth(method="glm", method.args = list(family="binomial"), se=FALSE, fullrange=T, linetype = "dotted", size = .6) +
  theme_bw()

math.4yr.plot.points <- math.4yr.plot.noline +
  geom_point(data=refbias.graph.means, aes(x=math.remap, y=math.pred.4yr, colour=as.factor(schoolID)))

math.4yr.plot.between <- math.4yr.plot.points +
  geom_smooth(
    aes(x=math.remap, y=math.pred.4yr, fill="black"), 
    data=refbias.graph.means, method="lm", se=FALSE, size=2, 
    color="black", fullrange=F) 


math.4yr.plot.noline.final <- math.4yr.cleanup.final(math.4yr.plot.noline)
math.4yr.plot.pointsonly.final <- math.4yr.cleanup.final(math.4yr.plot.points)
math.4yr.plot.final <- math.4yr.cleanup.final(math.4yr.plot.between) 


# 6-YEAR GRADUATION PLOTS -------------------------------------------------



# Grit --------------------------------------------------------------------

grit.6yr.plot.noline <- 
  ggplot(
    #DATA  
    #IV/DV/Groupings
    refbias.graph.combined, 
    aes(x=grit.remap, y=GRAD_Any_in_6YRS, colour=as.factor(schoolID))) + 
  #logistic curves fror groups:
  stat_smooth(method="glm", method.args = list(family = "binomial"), 
              se=FALSE, fullrange=F, size=.6) + theme_bw() + 
  stat_smooth(method="glm", method.args = list(family = "binomial"), 
              se=FALSE, fullrange=T, linetype = "dotted", size=.6) + theme_bw()

grit.6yr.plot.points <- 
  grit.6yr.plot.noline +
  # school-level points (different data):
  geom_point(data=refbias.graph.means, 
             aes(x=grit.remap, 
                 y=grit.pred.6yr, 
                 colour=as.factor(schoolID))) 

grit.6yr.plot.between <- 
  grit.6yr.plot.points +
  #school level line (different data):
  geom_smooth(aes(x=grit.remap,
                  y=grit.pred.6yr,
                  fill="black"), 
              data=refbias.graph.means, 
              method="lm", color="black", 
              size=2, se=FALSE, fullrange=F) 


grit.6yr.plot.noline.final <- grit.6yr.cleanup.final(grit.6yr.plot.noline)
grit.6yr.plot.pointsonly.final <- grit.6yr.cleanup.final(grit.6yr.plot.points)
grit.6yr.plot.final <- grit.6yr.cleanup.final(grit.6yr.plot.between) 


# Self-Control --------------------------------------------------------------------

sc.6yr.plot.noline <- 
  ggplot(
    #DATA  
    #IV/DV/Groupings
    refbias.graph.combined, 
    aes(x=sc.remap, y=GRAD_Any_in_6YRS, colour=as.factor(schoolID))) + 
  #logistic curves fror groups:
  stat_smooth(method="glm", method.args = list(family = "binomial"), 
              se=FALSE, fullrange=F, size=.6) + theme_bw() + 
  stat_smooth(method="glm", method.args = list(family = "binomial"), 
              se=FALSE, fullrange=T, linetype = "dotted", size=.6) + theme_bw()

sc.6yr.plot.points <- 
  sc.6yr.plot.noline +
  # school-level points (different data):
  geom_point(data=refbias.graph.means, 
             aes(x=sc.remap, 
                 y=sc.pred.6yr, 
                 colour=as.factor(schoolID))) 

sc.6yr.plot.between <- 
  sc.6yr.plot.points +
  #school level line (different data):
  geom_smooth(aes(x=sc.remap,
                  y=sc.pred.6yr,
                  fill="black"), 
              data=refbias.graph.means, 
              method="lm", color="black", 
              size=2, se=FALSE, fullrange=F) 


sc.6yr.plot.noline.final <- sc.6yr.cleanup.final(sc.6yr.plot.noline)
sc.6yr.plot.pointsonly.final <- sc.6yr.cleanup.final(sc.6yr.plot.points)
sc.6yr.plot.final <- sc.6yr.cleanup.final(sc.6yr.plot.between) 


# Self-reg --------------------------------------------------------------------

selfreg.6yr.plot.noline <- 
  ggplot(
    #DATA  
    #IV/DV/Groupings
    refbias.graph.combined, 
    aes(x=selfreg.remap, y=GRAD_Any_in_6YRS, colour=as.factor(schoolID))) + 
  #logistic curves fror groups:
  stat_smooth(method="glm", method.args = list(family = "binomial"), 
              se=FALSE, fullrange=F, size=.6) + theme_bw() + 
  stat_smooth(method="glm", method.args = list(family = "binomial"), 
              se=FALSE, fullrange=T, linetype = "dotted", size=.6) + theme_bw()

selfreg.6yr.plot.points <- 
  selfreg.6yr.plot.noline +
  # school-level points (different data):
  geom_point(data=refbias.graph.means, 
             aes(x=selfreg.remap, 
                 y=selfreg.pred.6yr, 
                 colour=as.factor(schoolID))) 

selfreg.6yr.plot.between <- 
  selfreg.6yr.plot.points +
  #school level line (different data):
  geom_smooth(aes(x=selfreg.remap,
                  y=selfreg.pred.6yr,
                  fill="black"), 
              data=refbias.graph.means, 
              method="lm", color="black", 
              size=2, se=FALSE, fullrange=F) 


selfreg.6yr.plot.noline.final <- selfreg.6yr.cleanup.final(selfreg.6yr.plot.noline)
selfreg.6yr.plot.pointsonly.final <- selfreg.6yr.cleanup.final(selfreg.6yr.plot.points)
selfreg.6yr.plot.final <- selfreg.6yr.cleanup.final(selfreg.6yr.plot.between) 




# ADT ---------------------------------------------------------------------

math.6yr.plot.noline <- ggplot(
  #DATA  
  #IV/DV/Groupings
  refbias.graph.combined, aes(x=math, y=GRAD_Any_in_6YRS, colour=as.factor(schoolID))) + 
  #logistic curves fror groups:
  stat_smooth(method="glm", method.args = list(family="binomial"), se=FALSE, fullrange=F, size = .6) + 
  stat_smooth(method="glm", method.args = list(family="binomial"), se=FALSE, fullrange=T, linetype = "dotted", size = .6) +
  theme_bw()

math.6yr.plot.points <- math.6yr.plot.noline +
  geom_point(data=refbias.graph.means, aes(x=math.remap, y=math.pred.6yr, colour=as.factor(schoolID)))

math.6yr.plot.between <- math.6yr.plot.points +
  geom_smooth(
    aes(x=math.remap, y=math.pred.6yr, fill="black"), 
    data=refbias.graph.means, method="lm", se=FALSE, size = 2.5, 
    color="black", fullrange=F) 


math.6yr.plot.noline.final <- math.6yr.cleanup.final(math.6yr.plot.noline)
math.6yr.plot.pointsonly.final <- math.6yr.cleanup.final(math.6yr.plot.points)
math.6yr.plot.final <- math.6yr.cleanup.final(math.6yr.plot.between) 

# Final tweaks ------------------------------------------------------------

# noncog.plot.final.notitle <- noncog.plot.final + ggtitle(NULL)
# grit.plot.final.notitle <- grit.plot.final + ggtitle(NULL) 
# math.plot.final.notitle <- math.plot.final + ggtitle(NULL)

plots <- list(
  grit.4yr.plot.final,
  sc.4yr.plot.final,
  selfreg.4yr.plot.final,
  math.4yr.plot.final,
  grit.6yr.plot.final,
  sc.6yr.plot.final,
  selfreg.6yr.plot.final,
  math.6yr.plot.final
)



annotations <- 
list(
  function(){annotate("text", x=3.5, y=0.50, parse=FALSE, size= annotation.size/2.5, family = "Helvetica", label=grit.4yr.plot.text)},
  function(){annotate("text", x=3.5, y=0.50, parse=FALSE, size= annotation.size/2.5, family = "Helvetica", label=sc.4yr.plot.text)},
  function(){annotate("text", x=3.5, y=0.50, parse=FALSE, size= annotation.size/2.5, family = "Helvetica", label=selfreg.4yr.plot.text)},
  function(){annotate("text", x= 50, y=0.50, parse=F, size= annotation.size/2.5, family = "Helvetica", label=math.4yr.plot.text)},
  
  function(){annotate("text", x=3.5, y=0.60, parse=FALSE, size = annotation.size/2.5, family = "Helvetica", label=grit.6yr.plot.text)},
  function(){annotate("text", x=3.25, y=0.60, parse=FALSE, size = annotation.size/2.5, family = "Helvetica", label=sc.6yr.plot.text)},
  function(){annotate("text", x=3.25, y=0.60, parse=FALSE, size = annotation.size/2.5, family = "Helvetica", label=selfreg.6yr.plot.text)},
  function(){annotate("text", x=55, y=.75, parse=F, size= annotation.size/2.5, family = "Helvetica", label=math.6yr.plot.text)}
)



plots2 <- 
  plots %>% 
  map2(.y = annotations, ~.x + plottheme(size = 10) + .y())

mylegend<-g_legend(plots2[[1]])

# plots3 <- 
#   plots2 %>% 
#    map(~ .x+ theme(legend.position="none"))
# 
# plot.output <-
#   arrangeGrob(
#     arrangeGrob(
#       grobs = plots3,
#       ncol = 2
#     )
#     , mylegend
#     , ncol = 2
#     , widths = c(10, 1)
#     
#   )
# 
# plot(plot.output)
# 
# ggsave(filename = paste0("./Study1/Plots/2019 plots/",Sys.Date(),".quad.plot.png")
#        , width = 15
#        , height = 15
#        , units = "in"
#        , dpi = "screen"
#        , plot.output)





# Saving ------------------------------------------------------------------

# Function to save plots with single-plot size settings:
plotsave <- function(.plot, .filename){
  ggsave(filename = 
           paste0("./Study1/Plots/2020 plots/",
                           Sys.Date(), ".", .filename)
         , plot = .plot
         , width = 8
         , height = 6.5
         , units = "in"
         , dpi = "retina")
  }

# Names of plot saves:
plotnames <- 
  c(
    "grit.4yr.plot.png",
    "selfcontrol.4yr.plot.png",
    "selfreg.4yr.plot.png",
    "math.4yr.plot.png",
    "grit.6yr.plot.png", 
    "selfcontrol.6yr.plot.png", 
    "selfreg.6yr.plot.png", 
    "math.6yr.plot.png"
    )

# Save all of them, using the plot2 list since it has reduced font size and annotations:
map2(plots2, plotnames, plotsave)


# Plots of distributions by school ----------------------------------------

ordered.density.data <- 
  refbias.graph %>% 
  mutate(rankgrit = dense_rank(-L2.grit.remap))

rankgrit.labels <-
  ordered.density.data %>% 
  group_by(rankgrit) %>%
  summarize(rankgrit2 = unique(L2.grit.remap)) %>% 
  mutate(rankgrit.labels = paste0("M = ", Format(rankgrit2, digits = 2))) %>% 
  pull %>% 
  set_names(1:15)

grit.ordered.density.plot <-
  ordered.density.data %>% 
  ggplot(
    aes(x = grit.remap)
  ) +
  geom_density() +
  # geom_point(
  #   data = 
  #     refbias.charter %>% 
  #     group_by(rankgrit) %>% 
  #     summarize(grit.c = mean(grit.c, na.rm=T),
  #               grad = mean(GRAD_Any_in_6YRS, na.rm=T)
  #     ),
  #   aes(y = grad)
  # ) + 
  ylim(c(0,1)) +
  facet_wrap(facets = "rankgrit", nrow = 3, 
             labeller = labeller(rankgrit = rankgrit.labels)) + 
  scale_color_discrete(guide = F) + 
  plottheme(size = 10) + 
  xlab("Self-Evaluated Grit") 


ggsave(plot = grit.ordered.density.plot
       , filename = "./Study1/Plots/2020 plots/ordered.distribution.png"
       , width = 8
       , height = 6.5
       , units = "in"
       , dpi = "retina")

testdat <- 
ordered.density.data %>% 
  group_by(rankgrit) %>% 
  summarize(
    Skew = Skew(grit.remap, na.rm=T),
    Kurt = Kurt(grit.remap, na.rm=T),
    L2 = unique(L2.grit.remap)
    )


testdat %>% 
  select(Skew, Kurt, L2) %>% 
  cor(use="pairwise.complete")
lm(Kurt ~ L2, testdat) %>% summary

Skew(ordered.density.data$grit.remap, na.rm=T)
Kur
# #### Final noncog Plots -------------------------------------
# # Older plot code that isn't used anymore under the new system.  
#
# 
# set.seed(007)
# noncog.plot.final.1 <- noncog.plot.1 +
#   #VISUALS:
#   #legend:
#   scale_colour_manual(name="Individual School\nBest Fit Lines", breaks=1:16,
#                       values=line.colors, labels=schoolIDs.name) +
#   scale_fill_identity(name = 'Between-Schools\nBest Fit Line',
#                       guide = "legend",labels = c('')) +
#   guides(color = guide_legend(order = 1),
#          fill = guide_legend(order = 2)) +
# 
#   #BW theme (less background, adds border)
#   theme_bw() +
#   theme(legend.text = element_text(family = "Helvetica", size=20),
#         legend.title = element_text(family = "Helvetica", size=20),
#         axis.text = element_text(family = "Helvetica", size=20),
#         axis.title = element_text(family = "Helvetica", size=20),
#       plot.title = element_text(family = "Helvetica", size=23, hjust = 0.5)) + 
#   #axes:
#   geom_vline(xintercept=1, alpha=.8, size=.8) +
#   geom_hline(yintercept=0, alpha=.8, size=.8) +
#   #Axis labels:
#   ylab("Probability of On-Time Degree Attainment (4 Years)") +
#   xlab("Self-Reported Academic Diligence Score (Grit / Self-Control Survey)") +
#   ggtitle("Relation of Self-Reported Diligence\nto 1-Year College Persistence")
#   
# 
# set.seed(007)
# noncog.plot.final.points <- noncog.plot.points +
#   #VISUALS:
#   #legend:
#   scale_colour_manual(name="Individual School\nBest Fit Lines", breaks=1:16,
#                       values=line.colors, labels=schoolIDs.name) +
#   scale_fill_identity(name = 'Between-Schools\nBest Fit Line',
#                       guide = "legend",labels = c('')) +
#   guides(color = guide_legend(order = 1),
#          fill = guide_legend(order = 2)) +
# 
#   #BW theme (less background, adds border)
#   theme_bw() +
#   theme(legend.text = element_text(family = "Helvetica", size=20),
#         legend.title = element_text(family = "Helvetica", size=20),
#         axis.text = element_text(family = "Helvetica", size=20),
#         axis.title = element_text(family = "Helvetica", size=20),
#         plot.title = element_text(family = "Helvetica", size=23, hjust = 0.5)) + 
#   #axes:
#   geom_vline(xintercept=1, alpha=.8, size=.8) +
#   geom_hline(yintercept=0, alpha=.8, size=.8) +
#   #Axis labels:
#   ylab("Probability of On-Time Degree Attainment (4 Years)") +
#   xlab("Self-Reported Academic Diligence Score (Grit / Self-Control Survey)") +
#   ggtitle("Relation of Self-Reported Diligence\nto 1-Year College Persistence")
#   
# set.seed(007)
# noncog.plot.final <- noncog.plot.between +
#   #VISUALS:
#   #legend:
#   scale_colour_manual(name="Individual School\nBest Fit Lines", breaks=1:16,
#                       values=line.colors, labels=schoolIDs.name) +
#   scale_fill_identity(name = 'Between-Schools\nBest Fit Line',
#                       guide = "legend",labels = c('')) +
#   guides(color = guide_legend(order = 1),
#          fill = guide_legend(order = 2)) +
# 
#   #BW theme (less background, adds border)
#   theme_bw() +
#   theme(legend.text = element_text(family = "Helvetica", size=20),
#         legend.title = element_text(family = "Helvetica", size=20),
#         axis.text = element_text(family = "Helvetica", size=20),
#         axis.title = element_text(family = "Helvetica", size=20),
#         plot.title = element_text(family = "Helvetica", size=23, hjust = 0.5)) + 
#   #axes:
#   geom_vline(xintercept=1, alpha=.8, size=.8) +
#   geom_hline(yintercept=0, alpha=.8, size=.8) +
#   #Axis labels:
#   ylab("Probability of On-Time Degree Attainment (4 Years)") +
#   xlab("Self-Reported Academic Diligence Score (Grit / Self-Control Survey)") +
#   ggtitle("Relation of Self-Reported Diligence\nto 1-Year College Persistence") +
#   annotate("text", x=2.5, y=0.95, parse=FALSE, size=7, family = "Helvetica", label=noncog.plot.text)
# #
# # set.seed(007)
# 
# #### Final grit Plots -------------------------------------
# 
# 
# grit.4yr.plot.noline <- grit.final.4yr.cleanup(grit.4yr.plot.1)
# 
# 
#   
# 
# set.seed(007)
# grit.plot.final.points <- grit.plot.points +
#   #VISUALS:
#   #legend:
#   scale_colour_manual(name="Individual School\nBest Fit Lines", breaks=1:16,
#                       values=line.colors, labels=schoolIDs.name) +
#   scale_fill_identity(name = 'Between-Schools\nBest Fit Line',
#                       guide = "legend",labels = c('')) +
#   guides(color = guide_legend(order = 1),
#          fill = guide_legend(order = 2)) +
# 
#   #BW theme (less background, adds border)
#   theme_bw() +
#   theme(legend.text = element_text(family = "Helvetica", size=20),
#         legend.title = element_text(family = "Helvetica", size=20),
#         axis.text = element_text(family = "Helvetica", size=20),
#         axis.title = element_text(family = "Helvetica", size=20),
#         plot.title = element_text(family = "Helvetica", size=23, hjust = 0.5)) + 
#   #axes:
#   geom_vline(xintercept=1, alpha=.8, size=.8) +
#   geom_hline(yintercept=0, alpha=.8, size=.8) +
#   #Axis labels:
#   ylab("Probability of On-Time Degree Attainment (4 Years)") +
#   xlab("Self-Reported Grit") +
#   ggtitle("Relation of Self-Reported Grit\nto 1-Year College Persistence")
#   
# set.seed(007)
# grit.plot.final <- grit.plot.between +
#   #VISUALS:
#   #legend:
#   scale_colour_manual(name="Individual School\nBest Fit Lines", breaks=1:16,
#                       values=line.colors, labels=schoolIDs.name) +
#   scale_fill_identity(name = 'Between-Schools\nBest Fit Line',
#                       guide = "legend",labels = c('')) +
#   guides(color = guide_legend(order = 1),
#          fill = guide_legend(order = 2)) +
# 
#   #BW theme (less background, adds border)
#   theme_bw() +
#   theme(legend.text = element_text(family = "Helvetica", size=20),
#         legend.title = element_text(family = "Helvetica", size=20),
#         axis.text = element_text(family = "Helvetica", size=20),
#         axis.title = element_text(family = "Helvetica", size=20),
#         plot.title = element_text(family = "Helvetica", size=23, hjust = 0.5)) + 
#   #axes:
#   geom_vline(xintercept=1, alpha=.8, size=.8) +
#   geom_hline(yintercept=0, alpha=.8, size=.8) +
#   #Axis labels:
#   ylab("Probability of On-Time Degree Attainment (4 Years)") +
#   xlab("Self-Reported Grit") +
#   ggtitle("Relation of Self-Reported Grit\nto 1-Year College Persistence") +
#   annotate("text", x=3, y=0.95, parse=FALSE, size=7, family = "Helvetica", label=grit.plot.text)
# #
# # set.seed(007)
# 
# 
# # Final math plots --------------------------------------------------------
# 
# 
# math.plot.final.1 <- math.plot.1 +
#   #VISUALS:
#   #legend:
#   scale_fill_identity(name = 'Between-Schools\nBest Fit Line', guide = 'legend',labels = c('')) +
#   scale_colour_manual(name= "Individual School\nBest Fit Lines", breaks=1:16, 
#                       values= line.colors, labels=schoolIDs.name) +
#   guides(color = guide_legend(order = 1),
#          fill = guide_legend(order = 2)) +
# 
#   #BW theme (less background, adds border)
#   theme_bw() +
#   theme(legend.text = element_text(family = "Helvetica", size=20),
#         legend.title = element_text(family = "Helvetica", size=20),
#         axis.text = element_text(family = "Helvetica", size=20),
#         axis.title = element_text(family = "Helvetica", size=20),
#         plot.title = element_text(family = "Helvetica", size=23, hjust = 0.5)) + 
#   #axes:
#   geom_vline(xintercept=0, alpha=.8, size=.8) +
#   geom_hline(yintercept=0, alpha=.8, size=.8) + 
#   #Axis labels:
#   ylab("Probability of On-Time Degree Attainment (4 Years)") + 
#   xlab("Academic Diligence Task Score (# Math Problems Correct)") + 
#   ggtitle("Relation of Behaviorally Assessed Diligence\nto 1-Year College Persistence")
#   
# math.plot.final.points <- math.plot.points +
#   #VISUALS:
#   #legend:
#   scale_fill_identity(name = 'Between-Schools\nBest Fit Line', guide = 'legend',labels = c('')) +
#   scale_colour_manual(name= "Individual School\nBest Fit Lines", breaks=1:16, 
#                       values= line.colors, labels=schoolIDs.name) +
#   guides(color = guide_legend(order = 1),
#          fill = guide_legend(order = 2)) +
# 
#   #BW theme (less background, adds border)
#   theme_bw() +
#   theme(legend.text = element_text(family = "Helvetica", size=20),
#         legend.title = element_text(family = "Helvetica", size=20),
#         axis.text = element_text(family = "Helvetica", size=20),
#         axis.title = element_text(family = "Helvetica", size=20),
#         plot.title = element_text(family = "Helvetica", size=23, hjust = 0.5)) + 
#   #axes:
#   geom_vline(xintercept=0, alpha=.8, size=.8) +
#   geom_hline(yintercept=0, alpha=.8, size=.8) + 
#   #Axis labels:
#   ylab("Probability of On-Time Degree Attainment (4 Years)") + 
#   xlab("Academic Diligence Task Score (# Math Problems Correct)") + 
#   ggtitle("Relation of Behaviorally Assessed Diligence\nto 1-Year College Persistence")
#   
# math.plot.final <- math.plot.between +
#   #VISUALS:
#   #legend:
#   scale_fill_identity(name = 'Between-Schools\nBest Fit Line', guide = 'legend',labels = c('')) +
#   scale_colour_manual(name= "Individual School\nBest Fit Lines", breaks=1:16, 
#                       values= line.colors, labels=schoolIDs.name) +
#   guides(color = guide_legend(order = 1),
#          fill = guide_legend(order = 2)) +
# 
#   #BW theme (less background, adds border)
#   theme_bw() +
#   theme(legend.text = element_text(family = "Helvetica", size=20),
#         legend.title = element_text(family = "Helvetica", size=20),
#         axis.text = element_text(family = "Helvetica", size=20),
#         axis.title = element_text(family = "Helvetica", size=20),
#         plot.title = element_text(family = "Helvetica", size=23, hjust = 0.5)) + 
#   #axes:
#   geom_vline(xintercept=0, alpha=.8, size=.8) +
#   geom_hline(yintercept=0, alpha=.8, size=.8) + 
#   #Axis labels:
#   ylab("Probability of On-Time Degree Attainment (4 Years)") + 
#   xlab("Academic Diligence Task Score (# Math Problems Correct)") + 
#   ggtitle("Relation of Behaviorally Assessed Diligence\nto 1-Year College Persistence") +
#   annotate("text", x=50, y=.97, parse=F, size=7, family = "Helvetica", label=math.plot.text)
# 
# 
# 
# 
# 
# 
# 
# # noncog.plot.final.1
# # noncog.plot.final.points
# # noncog.plot.final
# # 
# # grit.plot.final.1
# # grit.plot.final.points
# # grit.plot.final
# # 
# # math.plot.final.1
# # math.plot.final.points
# # math.plot.final
# 
# 
# 

# # PRINTING PLOTS TO FILE --------------------------------------------------
# 
# ###PNG PLOTS: ----
# 
# png(paste0("./Plots/",Sys.Date(),".MATH.PLOT.png"), width=10, height=6.5, units="in", res=300)
# math.plot
# dev.off()
# 
# png(paste0("./Plots/",Sys.Date(),".MATH.POINTSONLY.png"), width=10, height=6.5, units="in", res=300)
# math.plot.pointsonly
# dev.off()
# 
# png(paste0("./Plots/",Sys.Date(),".MATH.NOLINE.png"), width=10, height=6.5, units="in", res=300)
# math.plot.noline
# dev.off()
# 
# png(paste0("./Plots/",Sys.Date(),".SELFREPORT.PLOT.png"), width=10, height=6.5, units="in", res=300)
# noncog.plot
# dev.off()
# 
# png(paste0("./Plots/",Sys.Date(),".SELFREPORT.POINTSONLY.png"), width=10, height=6.5, units="in", res=300)
# noncog.plot.pointsonly
# dev.off()
# 
# png(paste0("./Plots/",Sys.Date(),".SELFREPORT.NOLINE.png"), width=10, height=6.5, units="in", res=300)
# noncog.plot.noline
# dev.off()
# 
# png(paste0("./Plots/",Sys.Date(),".SELFREPORT.NOLABELS.png"), width=10, height=6.5, units="in", res=600)
# noncog.plot.nolab
# dev.off()
# 
# png(paste0("./Plots/",Sys.Date(),".MATH.NOLABELS.png"), width=8, height=6.5, units="in", res=600)
# math.plot.nolab
# dev.off()
# 
# ###PDF PLOTS ----
# 
# pdf(paste0("./Plots/",Sys.Date(),".MATH.PLOT.pdf"), width=10, height=6.5)
# math.plot
# dev.off()
# 
# pdf(paste0("./Plots/",Sys.Date(),".MATH.POINTSONLY.pdf"), width=10, height=6.5)
# math.plot.pointsonly
# dev.off()
# 
# pdf(paste0("./Plots/",Sys.Date(),".MATH.NOLINE.pdf"), width=10, height=6.5)
# math.plot.noline
# dev.off()
# 
# pdf(paste0("./Plots/",Sys.Date(),".SELFREPORT.PLOT.pdf"), width=10, height=6.5)
# noncog.plot
# dev.off()
# 
# pdf(paste0("./Plots/",Sys.Date(),".SELFREPORT.POINTSONLY.pdf"), width=10, height=6.5)
# noncog.plot.pointsonly
# dev.off()
# 
# pdf(paste0("./Plots/",Sys.Date(),".SELFREPORT.NOLINE.pdf"), width=10, height=6.5)
# noncog.plot.noline
# dev.off()
# 
# pdf(paste0("./Plots/",Sys.Date(),".DUAL.PLOTS.pdf"), width=10, height=6.5)
# math.plot
# noncog.plot
# dev.off()
# 
# pdf(paste0("./Plots/",Sys.Date(),".MULTIPLOT.pdf"), width=10, height=10)
# multiplot(noncog.plot, math.plot)
# dev.off()
# 
# ###PNG PLOTS, CHARTERS: ----
# 
# png(paste0("./Plots/",Sys.Date(),".CHARTER.MATH.PLOT.png"), width=10, height=6.5, units="in", res=300)
# C.math.plot
# dev.off()
# 
# png(paste0("./Plots/",Sys.Date(),".CHARTER.MATH.POINTSONLY.png"), width=10, height=6.5, units="in", res=300)
# C.math.plot.pointsonly
# dev.off()
# 
# png(paste0("./Plots/",Sys.Date(),".CHARTER.MATH.NOLINE.png"), width=10, height=6.5, units="in", res=300)
# C.math.plot.noline
# dev.off()
# 
# png(paste0("./Plots/",Sys.Date(),".CHARTER.SELFREPORT.PLOT.png"), width=10, height=6.5, units="in", res=300)
# C.noncog.plot
# dev.off()
# 
# png(paste0("./Plots/",Sys.Date(),".CHARTER.SELFREPORT.POINTSONLY.png"), width=10, height=6.5, units="in", res=300)
# C.noncog.plot.pointsonly
# dev.off()
# 
# png(paste0("./Plots/",Sys.Date(),".CHARTER.SELFREPORT.NOLINE.png"), width=10, height=6.5, units="in", res=300)
# C.noncog.plot.noline
# dev.off()
# 
# ###PDF PLOTS, CHARTERS: ----
# 
# pdf(paste0("./Plots/",Sys.Date(),".CHARTER.MATH.PLOT.pdf"), width=10, height=6.5)
# C.math.plot
# dev.off()
# 
# pdf(paste0("./Plots/",Sys.Date(),".CHARTER.MATH.POINTSONLY.pdf"), width=10, height=6.5)
# C.math.plot.pointsonly
# dev.off()
# 
# pdf(paste0("./Plots/",Sys.Date(),".CHARTER.MATH.NOLINE.pdf"), width=10, height=6.5)
# C.math.plot.noline
# dev.off()
# 
# pdf(paste0("./Plots/",Sys.Date(),".CHARTER.SELFREPORT.PLOT.pdf"), width=10, height=6.5)
# C.noncog.plot
# dev.off()
# 
# pdf(paste0("./Plots/",Sys.Date(),".CHARTER.SELFREPORT.POINTSONLY.pdf"), width=10, height=6.5)
# C.noncog.plot.pointsonly
# dev.off()
# 
# pdf(paste0("./Plots/",Sys.Date(),".CHARTER.SELFREPORT.NOLINE.pdf"), width=10, height=6.5)
# C.noncog.plot.noline
# dev.off()
# 
# pdf(paste0("./Plots/",Sys.Date(),".CHARTER.DUAL.PLOTS.pdf"), width=10, height=6.5)
# C.math.plot
# C.noncog.plot
# dev.off()
# 
# pdf(paste0("./Plots/",Sys.Date(),".CHARTER.MULTIPLOT.pdf"), width=10, height=10)
# multiplot(C.noncog.plot, C.math.plot)
# dev.off()
# 
# 
# 
# 
# 
# # Make graphs that plot the school level math means as boxplots, ordered by mean.
# # Make graphs that plot the school level grit means as boxplots, ordered by MATH mean.
# # Overlay them.
# # 
# 
