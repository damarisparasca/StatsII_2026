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

# max_k is an arbitrary value 
# as k increases the term gets closer to 0
ks_pvalue <- function(d, max_k = 1000) {
  k <- 1:max_k
  # save individual terms in a vector
  terms <- exp(-((2*k - 1)^2 * pi^2) / (8*d^2))
  # sum the elements of the vector
  s <- sum(terms)
  # implement final p-value formula
  p_value <- sqrt(2*pi)/d * s
  return(p_value)
}

p <- ks_pvalue(D)
p

#####################
# Problem 2
#####################

set.seed (123) 
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# taking a look at the data 
pdf(file = "scatter_xy.pdf")
plot(data$x, data$y, ylab = "Y", xlab = "X")
dev.off()

# fitting the usual lm model
lm_model <- lm(y ~ x, data = data)
summary(lm_model)
 
# implement the log of the likelihood function 
linear.lik <- function(theta, y, X) {
	n <- nrow(X)  # no. observations 
	k <- ncol(X)  # no. independent variables (+ intercept)
	beta <- theta[1:k]  # coefficients 
	sigma2 <- theta[k+1]^2
	e <- y - X%*%beta   # residuals 
	logl <- -.5*n*log(2*pi) -.5*n*log(sigma2) - ((t(e) %*% e) / (2*sigma2))
	return (-logl) # negative because optim finds the minimum 
}

# maximising the likelihood function calculated above 
linear.MLE <- optim(fn = linear.lik, par = c(0,1,1), hessian = TRUE, 
y = data$y, X = cbind(1, data$x), method = "BFGS")

# look at the parameters obtained
cat("Intercept:", linear.MLE$par[1], "\n",
    "Slope:", linear.MLE$par[2], "\n")