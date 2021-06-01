ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))

library(dplyr)
library(lfe)
library(ggplot2)
library(dotwhisker)
library(tidyverse)
library(lubridate)
library(reshape2)

####Functions####
# Dta is the data
# Data must have the following columns: deaths, xvar (temp), fips (county id), monthyear (pasted together)
fe_model <- function(dta, level, interact=F) {
  
  dta <- na.omit(dta, cols=c("xvar", "yvar"))
  # if(interact) {
  #   if (level == 1) {
  #     mod <- felm(deaths ~ xvar + xvar:income_group | fips + monthyear, data=dta)
  #   } else if (level == "log") {
  #     mod <- felm(deaths ~ log(xvar) + log(xvar):income_group | fips + monthyear, data=dta )
  #   } else if (level > 1) {
  #     mod <- felm(deaths ~ poly(xvar,level,raw=T) + xvar:income_group | fips + monthyear, data=dta)
  #   }
  # } else {
  if (level == 1) {
    mod <- felm(yvar ~ xvar  + 
                  as.factor(county)*year | census_block_group + monthweek | 0 | census_block_group + countyyear, data=dta)
  } else if (level == "log") {
    mod <- felm(yvar ~ log(xvar)  + 
                  as.factor(county)*year | census_block_group + monthweek | 0 | census_block_group + countyyear, data=dta)
    
  } else if(level == "bin") {
      dta <- na.omit(dta, cols=c("xvar_bin", "yvar"))
      mod <- felm(yvar ~ xvar_bin + 
                    as.factor(county)*year | census_block_group + monthweek | 0 | census_block_group + countyyear, data=dta)
    
  } else if (level > 1) {
    mod <- felm(yvar ~ poly(xvar,level,raw=T) + 
                  as.factor(county)*year | census_block_group + monthweek | 0 | census_block_group + countyyear, data=dta)
  } 
  
  return(mod)
}

#Function to use in order to get a 95% confidence interval for our regression
#Bootstraps the data and essentially resamples and runs the same regression on subset of the data a bunch of times
#Level refers to the type of regression we are running. 1 = linear 2 = quadratic "log" = log of course
#returns a list of coefficients that can be used for plotting
bootstrap_data <- function(data, short=T, level, interact=F, name = "") {
  
  num <- ifelse(short, 100, 1000)
  ll = dim(data)[1]
  coefs <- NA 

  i <- 1
  while (i <= num)  {
    #sample the original data and pull subset of rows
    samp <- sample(1:ll,size=ll,replace=T)  
    newdata = data[samp,]
    #estimate our regression y = b1*T + err
    model <- fe_model(newdata, level, interact)
    #extract the coefficient estimates of b1 and b2 and store them in the matrix we made above
    model_coef <- coef(model)
    coefs <- rbind(coefs, model_coef[grepl('xvar', names(model_coef))])
  }
  #bootstrapped
  print("bootstrapped")
  #save it out for the next run if name was provided
  return(coefs)
  
}

#Function to output a plot with the regression and 95% confidence interval
build_plot_dataset <- function(data, coefs, title, level, xlab, ylab, model) {
  
  max_val <- max(data$xvar, na.rm = T)
  min_val <- min(data$xvar, na.rm = T)
  avg_val <- mean(data$xvar, na.rm = T)
  x = 0:40 ###this should be the max temp we see 
  xlen <- length(x)
  bts <- matrix(nrow=100,ncol=length(x))
  
  num_coefs <- ncol(coef)
  
  #get all my y values
  if (num_coefs == 1) {
    for (j in 1:100) {
      yy <- x*coefs[j]
      yy <- yy - yy[x=20] ## this x value should be the average temp. Otherwise we can just set it to the first yy value
      bts[j,] <- yy
    }  
  } else if (num_coefs == 2) {
    for (j in 1:100) {
      yy <- x*coefs[j,1] + x^2*coefs[j,2]  
      yy <- yy - yy[x=20]
      bts[j,] <- yy 
    }
  } else if (num_coefs == 3) {
    for (j in 1:100) {
      yy <- x*coefs[j,1] + x^2*coefs[j,2] + x^3*coefs[j,3] 
      yy <- yy - yy[x=20]
      bts[j,] <- yy 
    }
  }
  
  #figure out the 95 and 5 percentiles of the bootstraps
  conf <- apply(bts,1,function(x) quantile(x,probs=c(0.05,0.5,0.95), na.rm = T)) 
  conf <- as.data.frame(cbind(x, conf[1,], conf[2,], conf[3,]))
  colnames(conf) <- c("x", "low", "mid", "upper")   
  return(conf)
  
}

#Function to output plot with binned regressions 
build_bin_plot_dataset <- function(data, coefs, title, level, xlab, ylab,  model, dataset=NA, bins) {
  
  
  
  for(b in bins) {
    
    dta <- data %>% filter(xvar_bin == b)
    max_val <- max(dta$xvar, na.rm = T)
    min_val <- min(dta$xvar, na.rm = T)
    avg_val <- mean(dta$xvar, na.rm = T)
    x = min_val:max_val ###this should be the max temp we see 
    bts <- matrix(nrow=length(x),ncol=100)
    
    for (j in 1:100) {
      yy <- x*coefs[j]
      #yy <- yy - yy[x=1] ## this x value should be the average temp. Otherwise we can just set it to the first yy value
      bts[,j] <- yy
    }  
    
    #figure out the 95 and 5 percentiles of the bootstraps
    conf <- apply(bts,1,function(x) quantile(x,probs=c(0.05,0.5,0.95), na.rm = T)) 
    conf <- as.data.frame(cbind(x, conf[1,], conf[2,], conf[3,]))
    colnames(conf) <- c("x", "low", "mid", "upper") 
    dataset <- rbind(dataset, conf)
  }
 
  return(dataset)
  
}

