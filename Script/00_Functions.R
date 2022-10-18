
# Step 00 Key auxiliary -------------------------------------------------------------------------------------------
# Critical Pearson's R value from N, critical alpha, and null correlation
# value (typically zero, but it doesn't have to be).
# Two-tailed, significant difference from any specified correlation value
# using the basic z test formula typically used with Fisher's r to z.
critical.r.function <- function(
    alpha, n.pairs, r.null) {
  # Start out with some functions for calculations later
  r.to.z <- function(r){
    result <- .5*log( (1+r) / (1-r) )
    return(result)
  }
  z.to.r <- function(z){
    result <- (exp(2*z)-1) / (exp(2*z)+1)
    return(result)
  }
  # Calculate some values you will need
  r.to.z.null <- r.to.z(r.null)
  critical.z <- qnorm(alpha/2, mean = 0, sd = 1, lower.tail = F)
  se <- sqrt(1/(n.pairs-3))
  # Find the confidence interval simits
  ci.high.z <- se*critical.z+r.to.z.null
  ci.low.z <- se*(-critical.z)+r.to.z.null
  # Change the limits from the z to the r metric and output them.
  result <- c( z.to.r(ci.high.z), z.to.r(ci.low.z) )
  names(result) <- c("High Critical Pearson's R", "Low Critical Pearson's R")
  return(result)
} # End function


# Step 01 Detrending crop yield over years ------------------------------------------------------------------------

# Helper function: Fill in the missing values of the time series with a linear interpolation
seriesFill = function(y) {
  # Generate a generic x vector and a new y vector
  x = c(1:length(y))
  ynew = y
  # Find indices of missing values
  ii = which(is.na(y))
  # Approximate the missing values with a linear fit
  if (length(ii) > 0) ynew[ii] = approx(x, y, xout = x[ii], rule = 2)$y
  return(ynew)
} 


## Helper function: Compute a non-linear trend from a given time series (input) using SSA
## 	input:  1-dimensional vector
##	output: ssa-trend as a 1-dimensional vector

ssatrend = function(y, L = (length(y) - 1) %/% 2, fill = TRUE) {
  library(Rssa)
  # Compute the trend from SSA decomposition
  # If all (or only one) values in input are NA, then return NA
  if (sum(!is.na(y)) <= 1 ) {
    trend = rep(NA, length(y))
    return(trend)
  }
  
  # If fill requested, make sure that NAs are filled in
  if (fill) y = seriesFill(y)
  
  # Get a new ssa object and
  # Reconstruct the first element
  ssa   = ssa(y, L = L)
  trend = reconstruct(ssa, groups = list(1))$F1
  
  # That's it! Return the trend...
  return(trend)
}



# SSA detrending - returning absolute residuals
detrending.ssa = function(x){
  x = as.ts(x)
  x.new = x - ssatrend(x, L = 5)   
  return(as.numeric(x.new))
}


# SSA detrending - returning relative residuals
detrending.ssa.pct = function(x){    
  x = as.ts(x)
  model = ssatrend(x, L = 5)
  x.new = (x - model) / model
  return(as.numeric(x.new))
}


# linear detrending - returning absolute residuals
detrending.linear = function(x) {
  t = 1:length(x)
  lm = lm(x ~ t, na.action = na.exclude) #fit a linear model to the time series
  return(resid(lm)) #return a time series of residuals
}

# linear detrending - returning relative residuals
detrending.linear.pct = function(x) {
  t = 1:length(x)
  lm = lm(x ~ t, na.action = na.exclude) #fit a linear model to the time series
  return(resid(lm)/predict(lm)*100) #return a time series of residuals relative to predicted value (in percent)
}


# quadratic detrending - returning absolute residuals
detrending.quadratic = function(x){
  t = 1:length(x)
  lm = lm(x ~ poly(t, 2), na.action = na.exclude) #fit a linear model to the time series
  return(resid(lm)) #return a time series of residuals
}


# quadratic detrending - returning relative residuals
detrending.quadratic.pct = function(x){
  t = 1:length(x)
  lm = lm(x ~ poly(t, 2), na.action = na.exclude) #fit a linear model to the time series
  return(resid(lm)/predict(lm)*100) #return a time series of residuals relative to predicted value (in percent)
}


# best fit detrending - returning absolute residuals
# best fit from 4 models: 1) de-meaning, 2) linear, 3) quadratic, 4) cubic
detrending.best.fit = function(x){
  t = 1:length(x)
  
  # fit four different models
  lm_fit = list()
  lm_fit[[1]] = lm(x ~ 1, na.action = na.exclude)
  lm_fit[[2]] = lm(x ~ t, na.action = na.exclude)
  lm_fit[[3]] = lm(x ~ poly(t, 2), na.action = na.exclude)
  lm_fit[[4]] = lm(x ~ poly(t, 3), na.action = na.exclude)
  
  # calculate AIC for each model
  aic = sapply(X = lm_fit, FUN = AIC)
  
  # select best model
  pos = which(aic == min(aic))
  if (length(pos) > 1) { # no model fits best
    return(rep(NA, length(x)))
  } else {
    lm_fit = lm_fit[[pos]]
    return(resid(lm_fit)) #return a time series of residuals
  }
}


# best fit detrending - returning relative residuals
# best fit from 4 models: 1) de-meaning, 2) linear, 3) quadratic, 4) cubic
detrending.best.fit.pct = function(x){
  t = 1:length(x)
  
  # fit four different models
  lm_fit = list()
  lm_fit[[1]] = lm(x ~ 1, na.action = na.exclude)
  lm_fit[[2]] = lm(x ~ t, na.action = na.exclude)
  lm_fit[[3]] = lm(x ~ poly(t, 2), na.action = na.exclude)
  lm_fit[[4]] = lm(x ~ poly(t, 3), na.action = na.exclude)
  
  # calculate AIC for each model
  aic = sapply(X = lm_fit, FUN = AIC)
  
  # select best model
  pos = which(aic == min(aic))
  if (length(pos) > 1) { # no model fits best
    return(rep(NA, length(x)))
  } else {
    lm_fit = lm_fit[[pos]]
    return(resid(lm_fit)/predict(lm_fit)*100) #return a time series of residuals relative to predicted value (in percent)
  }
}


# annual increments - returning absolute residuals
detrending.incr = function(x){
  y = NULL
  y[1] = NA
  y[2:length(x)] = x[2:length(x)] - x[1:(length(x) - 1)]
  return(y)
}


# annual increments - returning relative residuals
detrending.incr.pct = function(x){
  y = NULL
  y[1] = NA
  y[2:length(x)] = (x[2:length(x)] - x[1:(length(x) - 1)]) / x[1:(length(x) - 1)] * 100
  return(y)
}


# wrapper function for all the different detrending types mentioned above
# options for detrending_type: lin, lin_pct, quad, quad_pct, best_fit, best_fit_pct, ssa, ssa_pct, incr, incr_pct

detrending = function(x, detrending_type, required_timesteps = 10, verbose = FALSE){
  
  if (verbose) print(sprintf("function: detrend"))
  
  # if time series has less than 'required_timesteps' values, return a vector of NAs
  if (sum(!is.na(x)) < required_timesteps) return(rep(NA, length(x)))
  
  if (detrending_type == "lin") {
    if (verbose) print(sprintf("linear detrending"))
    return(detrending.linear(x))
  } else if (detrending_type == "lin_pct") {
    if (verbose) print(sprintf("linear detrending, percentages"))
    return(detrending.linear.pct(x))
  } else if (detrending_type == "quad") {
    if (verbose) print(sprintf("quadratic detrending"))
    return(detrending.quadratic(x))
  } else if (detrending_type == "quad_pct") {
    if (verbose) print(sprintf("quadratic detrending, percentages"))
    return(detrending.quadratic.pct(x))
  } else if (detrending_type == "best_fit") {
    if (verbose) print(sprintf("best fit detrending"))
    return(detrending.best.fit(x))
  } else if (detrending_type == "best_fit_pct") {
    if (verbose) print(sprintf("best fit detrending, percentages"))
    return(detrending.best.fit.pct(x))
  } else if (detrending_type == "ssa") {
    if (verbose) print(sprintf("ssa detrending"))
    return(detrending.ssa(x))
  } else if (detrending_type == "ssa_pct") {
    if (verbose) print(sprintf("ssa detrending, percentages"))
    return(detrending.ssa.pct(x))
  } else if (detrending_type == "incr") {
    if (verbose) print(sprintf("annual increments"))
    return(detrending.incr(x))
  } else if (detrending_type == "incr_pct") {
    if (verbose) print(sprintf("annual increments, percentages"))
    return(detrending.incr.pct(x))
  }
}


# Step 02 Heat stress functions and trending analysis -------------------------------------------------------------

tbase <- 33

# total heat degree-days beyond a threshold
hdd_severity <- function(x, base = tbase) {
  int <- x >= base
  hdd <- sum(x[int] - base)
  return(hdd)
}

# count days beyond a threshold
hdd_days <- function(x, base = tbase) {
  days <- sum(x >= base)
  return(days)
}

# average temperature for days beyond a threshold
hdd_intensity <- function(x, base = tbase) {
  int <- x >= tbase
  if (any(int)) {
    hdd <- mean(x[int])
  } else {
    hdd <- 0
  }
  return(hdd)
}


# the maximum continuous days exposure to heat
hdd_max <- function(x, base = tbase) {
  s <- rle(x > base)
  if (any(s$values)) {
    rs <- max(s$lengths[s$values])
  } else {
    rs <- integer(1)
  }
  return(rs)
}


