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

#####################
# Problem 1
#####################

set.seed(123)
# generate data 
data <- (rcauchy(1000, location = 0, scale = 1))
# create empirical distribution of observed data
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data)))
D
# generate p-value 
# tol threshold is the default from the ks function in R
# stop_k is an arbitrary chosen safe value 
ks_pvalue <- function(d, tol = 1e-8, stop_k = 1000) {
	k <- 1
	result <- Inf
	s <- 0 

	while (result > tol && k < stop_k) {
		result <- exp(-((2*k-1)^2 * pi^2) / (8* d^2))
		s <- s + result
		k <- k + 1
	}
	
	p_value <- sqrt(2*pi)/d * s
	p_value 
}

ks_pval <- function(data, d) {
	s <- 0
	for (k in 1:length(data)) {
		result <- exp(-((2*k-1)^2 * pi^2) / (8* d^2))
		s <- s + result
	}
	
	p_value <- sqrt(2*pi)/d * s
	p_value 
}



p <- ks_pval(data, D)
p

install.packages("dgof")
library("dgof")

# Perform the K-S test to check if the sample follows a normal distribution
ks_test <- ks.test(data, "pnorm")
ks_test
ks_test$p.value
#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)
plot(data$x, data$y, ylab = "Y", xlab = "X")
lm_model <- lm(y ~ x, data = data)
summary(lm_model)

linear.lik <- function(theta, y, X) {
	n <- nrow(X)
	k <- ncol(X)
	beta <- theta[1:k]
	sigma2 <- theta[k+1]^2
	e <- y - X%*%beta
	logl <- -0.5*n*log(2*pi) -0.5*n*log(sigma2) - ((t(e) %*% e) / (2*sigma2))
	return (-logl)
}


linear.MLE <- optim(fn = linear.lik, par = c(1,1,1), hessian = TRUE, 
y = data$y, X = cbind(1, data$x), method = "BFGS")


linear.MLE$par
