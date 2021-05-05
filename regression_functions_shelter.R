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
# Data must have the following columns: deaths, measure (temp), fips (county id), monthyear (pasted together)
fe_model <- function(dta, level, interact=F) {
  
  dta <- na.omit(dta, cols=c("measure", "yvar"))
  # if(interact) {
  #   if (level == 1) {
  #     mod <- felm(deaths ~ measure + measure:income_group | fips + monthyear, data=dta)
  #   } else if (level == "log") {
  #     mod <- felm(deaths ~ log(measure) + log(measure):income_group | fips + monthyear, data=dta )
  #   } else if (level > 1) {
  #     mod <- felm(deaths ~ poly(measure,level,raw=T) + measure:income_group | fips + monthyear, data=dta)
  #   }
  # } else {
  if (level == 1) {
    mod <- felm(yvar ~ measure  + 
                  as.factor(county)*year | census_block_group + monthweek | 0 | census_block_group + countyyear, data=dta)
  } else if (level == "log") {
    mod <- felm(yvar ~ log(measure)  + 
                  as.factor(county)*year | census_block_group + monthweek | 0 | census_block_group + countyyear, data=dta)
  } else if (level > 1) {
    mod <- felm(yvar ~ poly(measure,level,raw=T) + 
                  as.factor(county)*year | census_block_group + monthweek | 0 | census_block_group + countyyear, data=dta)
  }
  #}
  
  return(mod)
}

#Function to use in order to get a 95% confidence interval for our regression
#Bootstraps the data and essentially resamples and runs the same regression on subset of the data a bunch of times
#Level refers to the type of regression we are running. 1 = linear 2 = quadratic "log" = log of course
#returns a list of coefficients that can be used for plotting
bootstrap_data <- function(data, short=T, level, interact=F, name = "") {
  
  num <- ifelse(short, 100, 1000)
  ll = dim(data)[1]
  # if(interact) {
  #   coef <- matrix(nrow=num, ncol=(ifelse(level=="log", 1, level) + 1))
  # } else {
  #   
  # }
  
  coef <- matrix(nrow=num, ncol=ifelse(level=="log", 1, level))
  
  i <- 1
  while (i <= num)  {
    #sample the original data and pull subset of rows
    samp <- sample(1:ll,size=ll,replace=T)  
    newdata = data[samp,]
    #estimate our regression y = b1*T + err
    model <- fe_model(newdata, level, interact)
    #extract the coefficient estimates of b1 and b2 and store them in the matrix we made above
    if(level == "log") {coef[i,] <- coef(model)[1]} else {coef[i,] <- coef(model)[1:level]}
    #print(i)  #print this out so you can watch progress 
    i <- i+1
  }
  #bootstrapped
  print("bootstrapped")
  #save it out for the next run if name was provided
  returnlist <- list(coef)
  return(returnlist)
  
}


#Function to output a plot with the regression and 95% confidence interval
plot_regs <- function(data, coefs, title, level, xlab, ylab, model) {
  
  coefs <- coefs[[1]]
  max_val <- max(data$measure, na.rm = T)
  min_val <- min(data$measure, na.rm = T)
  avg_val <- mean(data$measure, na.rm = T)
  x = min_val:max_val ###this should be the max temp we see 
  bts <- matrix(nrow=100,ncol=length(x))
  
  #get all my y values
  if (level == 1) {
    for (j in 1:100) {
      yy <- x*coefs[j]
      yy <- yy - avg_val ## this x value should be the average temp. Otherwise we can just set it to the first yy value
      bts[j,] <- yy
    }  
  } else if (level == 2) {
    for (j in 1:100) {
      yy <- x*coefs[j,1] + x^2*coefs[j,2]  
      yy <- yy - avg_val
      bts[j,] <- yy 
    }
  } else if (level == 3) {
    for (j in 1:100) {
      yy <- x*coefs[j,1] + x^2*coefs[j,2] + x^3*coefs[j,3] 
      yy <- yy - avg_val
      bts[j,] <- yy 
    }
  }
  
  #figure out the 95 and 5 percentiles of the bootstraps
  confint <- apply(bts,2,function(x) quantile(x,probs=c(0.05,0.5,0.95), na.rm = T)) 
  
  
  #plot median estimate among the bootstraps
  plot(x, confint[2,], type = "l", las=1,xlab=xlab,ylab=ylab,
       ylim = c(min(confint[1,]), max(confint[3,])), col="navy", main=title)   
  #plot confidence intervals
  polygon(c(x,rev(x)),c(confint[1,],rev(confint[3,])),col=adjustcolor("navy", alpha=.3),border = NA)
  rug(data$measure, side = 1, col=adjustcolor("black", alpha = 0.05))
  
  #Get the R^2, p-val, and AIC value
  r <- summary(model)$r.squared
  pval <- summary(model)$coefficients[,4]
  aic <- AIC(model)
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n',
       main = title)
  text(x = 0.5, y = 0.5, paste(title, 
                               "\n R^2 =", round(r, 3), 
                               "\n pvals =", round(pval[1],3), 
                               ",", round(pval[2],3),
                               "\n AIC =", round(aic,3)), 
       cex = .75, col = "black")
  
}

#Function to output plot with binned regressions 
plot_regs_binned <- function(data, coefs, title, level, xlab, ylab,  model, plt=NA) {
  
  coefs <- coefs[[1]]
  max_val <- max(data$measure, na.rm = T)
  min_val <- min(data$measure, na.rm = T)
  avg_val <- mean(data$measure, na.rm = T)
  x = min_val:max_val ###this should be the max temp we see 
  bts <- matrix(nrow=100,ncol=length(x))
  
  #get all my y values
  if (level == 1) {
    for (j in 1:100) {
      yy <- x*coefs[j]
      yy <- yy - avg_val ## this x value should be the average temp. Otherwise we can just set it to the first yy value
      bts[j,] <- yy
    }  
  } else if (level == 2) {
    for (j in 1:100) {
      yy <- x*coefs[j,1] + x^2*coefs[j,2]  
      yy <- yy - avg_val
      bts[j,] <- yy 
    }
  } else if (level == 3) {
    for (j in 1:100) {
      yy <- x*coefs[j,1] + x^2*coefs[j,2] + x^3*coefs[j,3] 
      yy <- yy - avg_val
      bts[j,] <- yy 
    }
  }
  
  #figure out the 95 and 5 percentiles of the bootstraps
  confint <- apply(bts,2,function(x) quantile(x,probs=c(0.05,0.5,0.95), na.rm = T)) 
  
  #if this is the first plot, start ggplot, if not, just add to plt
  if(is.na(plt)) {
  
    plt <- ggplot(data=data, aes(x=x, y=confint[2,])) +
      geom_point() +
      geom_ribbon(aes(ymin = confint[1,], ymax = confint[3,]), alpha = 0.1) +
      ggtitle(title) + ylab(ylab) + xlab(xlab) +
      scale_x_continuous() +
      theme(text = element_text(size = 15)) + 
      theme_bw()
    
  } else {
    
    plt <- plt + geom_line(aes(x, confint[2,]))
    plt <- plt + geom_ribbon(aes(ymin = confint[1,], ymax = confint[3,]), alpha = 0.1) +      
      scale_x_continuous()

  }
  
  return(plt)

  
  
}
