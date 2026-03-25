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
lapply(c("nnet", "MASS", "stargazer", "AER", "pscl"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)

# thresholds for data categories 
gdp_data$GDPWdiff <- ifelse(gdp_data$GDPWdiff < 0, "negative",
                                ifelse(gdp_data$GDPWdiff == 0, "no_change", "positive"))
gdp_data$GDPWdiff <- factor(gdp_data$GDPWdiff,
                                levels = c("negative", "no_change", "positive"))

# set no_change as the reference category
gdp_data$GDPWdiff <- relevel(gdp_data$GDPWdiff, ref = "no_change")

# Part 1 - unordered multinomial logit
# fit model 
unord_log <- multinom(GDPWdiff ~ REG + OIL , data = gdp_data)
summary(unord_log)
stargazer(unord_log)

# get odd ratios for interpretation
exps_unord <- exp(coef(unord_log))
stargazer(exps_unord)

# Part 2 - ordered multinomial logit 
# turn into ordered data 
gdp_data$GDPWdiff <- factor(gdp_data$GDPWdiff,
                                levels = c("negative","no_change","positive"),
                                ordered = TRUE)

# fit model
ord_log <- polr(GDPWdiff ~ REG + OIL , data = gdp_data, Hess = TRUE)
summary(ord_log)
stargazer(ord_log)

# get odd ratios for interpretation
exps_ord <- exp(coef(ord_log))
stargazer(exps_ord)

# get the cutoff points
cut_points <- ord_log$zeta
stargazer(cut_points)
#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")
head(mexico_elections)

# Task A 
# fit poisson model
pois_mod <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, 
                data = mexico_elections, family = poisson)
summary(pois_mod)
stargazer(pois_mod)

# save coefficients 
coefs <- summary(pois_mod)$coefficients

# provide test statistic and p-value for district
z_district <- coefs["competitive.district", "z value"]
p_district <- coefs["competitive.district", "Pr(>|z|)"]
cat("test statistic:", z_district,"| p-value:", p_district, "\n")

# Task B
# exponentiate for interpretation
exp_pois <- exp(coef(pois_mod))
stargazer(exp_pois)

# Task C
# make a dataframe with the desired values
data_pred <- data.frame(
                competitive.district = 1,  
                marginality.06 = 0, 
                PAN.governor.06 = 1)

# predict the estimated count
lambda_pred <- predict(pois_mod,
                newdata = data_pred,
                type = "response")
lambda_pred
