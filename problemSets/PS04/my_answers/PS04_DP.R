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

lapply(c("nnet", "MASS", "eha", "survival", "sampleSelection", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data on child mortality by mother's background and child gender
data(child)
head(child)

# fit Cox Proportional Hazard model
add_surv <- coxph(Surv(enter,exit, event) ~ sex + m.age,
            data = child)
summary(add_surv)
stargazer(add_surv)

# get hazard ratios
stargazer(exp(coef(add_surv)))

# plot it 
plot_coxph <- coxreg(Surv(enter,exit, event) ~ sex + m.age,
                    data = child)

pdf("coxph_plot.pdf")
plot(plot_coxph)
dev.off()

#####################
# Problem 2
#####################

# load data
disaster_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/refs/heads/main/datasets/disaster_response.csv")
head(disaster_data)

# fit Heckman selection model
heck <- heckit(binContribution ~ occurrences + deathsEM + normalizedDamageEMLogged,
               originalContributionMillionUSDLogged ~ occurrences + deathsEM + normalizedDamageEMLogged,
               data = disaster_data)
summary(heck)
stargazer(heck)
