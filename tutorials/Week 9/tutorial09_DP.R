#######################
# Tutorial 9: Poisson #
#######################

#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data: Research Productivity of Doctoral Students in Bio-chemistry (Long 1990) 
# Productivity of doctoral students in biochemistry during the last three yearsof their PhD programmes. 
# The response variables the number of articles published during this period (art)
# Explanatory variables include:
# - gender of the student (fem=1 for women, 0=men)
# - student’s marital status (mar= 1 if married, 0 otherwise)
# - student’s number of children five years old or younger (kid5); 
# - a rating of the prestige of the student’sPhD department (phd); 
# - number of articles published by the student’s mentor during the three-yearperiod (ment)

# Make sure your data are in the correct format.

# (a) Examine the distribution of the response variable. 
# Does least-squares linear regression appear a promising strategy for these data?
# Do we meet the OLS assumptions?

# (b) Perform a Poisson regression of number of articles published on the explanatory variables. 
# Do we meet assumptions for Poisson?
# What conclusions would you draw from this analysis (i.e. interpret your estimated coefficients)?

# What is the predicted number of articles for a married male PhD researcher with 1 child at 2-rated institute whose PhD supervisor published 5 articles?
# Plot predictions vs count.
# Calculate pseudo R squared.
# Calculate RMSE.
# Should we add an interaction for gender with our covariates?

# (c) Consider the possibility of over-dispersion, either by fitting an over-dispersed Poisson model. 
# Is there evidence for over-dispersion? How, if at all, do the results change when over-dispersion is taken into account?

long_data <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Long.txt", header=T)
str(long_data)
table(long_data$art)
hist(long_data$art)
mean(long_data$art)
var(long_data$art)

mod <- lm(art ~ ., data = long_data)
summary(mod)

pois_mod <- glm(art ~ ., data = long_data, family = poisson)
summary(pois_mod)

pseudo_rsquared <- 1 - pois_mod$deviance/ pois_mod$null.deviance
pseudo_rsquared

rmse <- sqrt(mean(long_data$art - pred)^2)
rmse

int_mod <- glm(art ~ fem * (kid5 + mar+ phd + ment), data = long_data, family = poisson)
summary(int_mod)

anova(pois_mod, int_mod, test = "LRT")

install.packages("AER")
library("AER")
# dispersion over 1 -> data is overdispersed
dispersiontest(pois_mod)

install.packages("pscl")
library("pscl")

mod.zip <- zeroinfl(art ~ ., data = long_data, dist = "poisson")
summary(mod.zip)

library(MASS)
nb_mod <- glm.nb(art ~ ., data = long_data)
summary(nb_mod)
