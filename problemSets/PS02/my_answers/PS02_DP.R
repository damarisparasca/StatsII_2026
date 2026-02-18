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

lapply(c("stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##########################
# Problem 1, Question 1 
##########################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))
data <- climateSupport
str(data)

# unorder data to obtain more readable levels 
data$countries <- factor(data$countries, ordered = FALSE)
data$sanctions <- factor(data$sanctions, ordered = FALSE)

# fit additive model 
add_model <- glm(
			choice ~ countries + sanctions ,
			data = data, family = binomial(link = "logit"))
summary(add_model)
stargazer(add_model)

# fit null model
null_model <- glm(
			choice ~ 1 , data = data, 
			family = binomial(link = "logit"))

# perform global null hypothesis test
anova(null_model, add_model, test = "LRT")

##########################
# Problem 1, Question 2 
##########################

# relevel model for interpretability 
data$sanctions <- relevel(data$sanctions, ref = "5%")

# refit the additive model with the new reference category
add_model5 <- glm(
			choice ~ countries + sanctions ,
			data = data, family = binomial(link = "logit"))
summary(add_model5)
stargazer(add_model5)

exp(coef(add_model5)["sanctions15%"])

# estimated probability - none, 80/192 
est_prob <- predict(add_model,
                    newdata = data.frame(sanctions = "None",
                                         countries = "80 of 192"),
                    type = "response")
est_prob

##########################
# Problem 1, Question 3 
##########################

# model with interaction term 
mult_model <- glm(
			choice ~ countries * sanctions ,
			data = data, family = binomial(link = "logit"))
			
summary(mult_model)
stargazer(mult_model)

# likelihood ratio test additive vs. multiplicative 
anova(add_model, mult_model, test = "LRT")
			
