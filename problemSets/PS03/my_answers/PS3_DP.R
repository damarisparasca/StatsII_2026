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

lapply(c("nnet", "MASS", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)

gdp_data$GDPWdiff <- ifelse(gdp_data$GDPWdiff < 0, "negative",
                                ifelse(gdp_data$GDPWdiff == 0, "no_change", "positive"))

gdp_data$GDPWdiff <- factor(gdp_data$GDPWdiff,
                                levels = c("negative", "no_change", "positive"))
gdp_data$GDPWdiff <- relevel(gdp_data$GDPWdiff, ref = "no_change")

# Part 1 - unordered multinomial logit
unord_log <- multinom(GDPWdiff ~ REG + OIL , data = gdp_data)
summary(unord_log)
stargazer(unord_log)

exps_unord <- exp(coef(unord_log))
stargazer(exps_unord)

# Part 2 - ordered multinomial logit 
gdp_data$GDPWdiff <- factor(gdp_data$GDPWdiff,
                                levels = c("negative","no_change","positive"),
                                ordered = TRUE)
ord_log <- polr(GDPWdiff ~ REG + OIL , data = gdp_data, Hess = TRUE)
summary(ord_log)
stargazer(ord_log)

ci <- confint(ord_log)
exps_ord <- exp(cbind(OR = coef(ord_log), ci))
stargazer(exps_ord)

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")
