ifelse(dir.exists("~/Box Sync/heatwave_covid"),
       setwd("~/Box Sync/heatwave_covid"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/heatwave_covid"))


# UPDATE Make sure you install these libraries install.packages("library name")
library(dplyr)
library(lfe)
library(ggplot2)
library(dotwhisker)
library(tidyverse)
library(lubridate)
library(reshape2)

##Read in datasets
temps <- read_rds("./calculated/temperature_data.rds")
mortality <- read_rds("./calculated/all_mortality.rds")
income <- read.csv("./us census/income_county.csv", stringsAsFactors = F, header = T)

##Consistent Datatypes

#Set up temps for analysis
tm <- temps %>% dplyr::filter(measure == "tmmn")
tx <- temps %>% dplyr::filter(measure == "tmmx")

t <- left_join(tm, tx, by = c("date", "fips"))
t <- t %>% select(date, county = county.x, fips, mean_min = mean_measure.x, 
                  max_min = max_measure.x, mean_max = mean_measure.y, 
                  max_max = max_measure.y) %>%
  mutate(fips = as.character(fips), month = month(date), year = year(date))
t$monthyear <- paste0(t$month, t$year)

#set up mortality & add in median income
m <- mortality %>% mutate(month = as.numeric(month), year = as.numeric(year))
income$fips <- as.character(income$fips)
income$group <- 
  i <- income %>% mutate("fips" = ifelse(nchar(fips) == 4, paste0("0", fips), fips)) %>% 
  select(fips, median_income, poverty_percent)
m <- left_join(m, i, by = "fips")

####Functions####
# Dta is the data
# Data must have the following columns: deaths, measure (temp), fips (county id), monthyear (pasted together)
fe_model <- function(dta, level, interact=F) {
  
  dta <- na.omit(dta, cols=c("measure", "deaths"))
  
  if(interact) {
    if (level == 1) {
      mod <- felm(deaths ~ measure + measure:median_income | fips + monthyear, data=dta)
    } else if (level == "log") {
      mod <- felm(deaths ~ log(measure) + log(measure):median_income | fips + monthyear, data=dta )
    } else if (level > 1) {
      mod <- felm(deaths ~ poly(measure,level,raw=T) + measure:median_income | fips + monthyear, data=dta)
    }
  } else {
    if (level == 1) {
      mod <- felm(deaths ~ measure | fips + monthyear, data=dta)
    } else if (level == "log") {
      mod <- felm(deaths ~ log(measure) | fips + monthyear, data=dta )
    } else if (level > 1) {
      mod <- felm(deaths ~ poly(measure,level,raw=T) | fips + monthyear, data=dta)
    }
  }
  
  return(mod)
}

#Function to use in order to get a 95% confidence interval for our regression
#Bootstraps the data and essentially resamples and runs the same regression on subset of the data a bunch of times
#Level refers to the type of regression we are running. 1 = linear 2 = quadratic "log" = log of course
#returns a list of coefficients that can be used for plotting
bootstrap_data <- function(data, short=T, level, interact=F) {
  
  num <- ifelse(short, 100, 1000)
  ll = dim(data)[1]
  coef <- matrix(nrow=num, ncol=ifelse(level=="log", 1, level))
  
  i <- 1
  while (i <= num)  {
    #sample the original data and pull subset of rows
    samp <- sample(1:ll,size=ll,replace=T)  
    newdata = data[samp,]
    #estimate our regression y = b1*T + err
    model <- fe_model(newdata, level, interact)
    #extract the coefficient estimates of b1 and b2 and store them in the matrix we made above
    coef[i,] <- coef(model) 
    print(i)  #print this out so you can watch progress 
    i <- i+1
  }
  
  #save it out for the next run if name was provided
  returnlist <- list(coef)
  return(returnlist)
  
}


#Function to output a plot with the regression and 95% confidence interval
plot_regs <- function(data, coefs, title, level) {
  
  coefs <- coefs[[1]]
  max_val <- max(data$measure, na.rm = T)
  min_val <- min(data$measure, na.rm = T)
  x = min_val:max_val ###this should be the max SWE we see 
  bts <- matrix(nrow=100,ncol=length(x))
  
  #get all my y values
  if (level == 1) {
    for (j in 1:100) {
      yy <- x*coefs[j]
      yy <- yy - yy[x=1] ## this x value should be the average swe. Otherwise we can just set it to the first yy value
      bts[j,] <- yy
    }  
  } else if (level == 2) {
    for (j in 1:100) {
      yy <- x*coefs[j,1] + x^2*coefs[j,2]  
      yy <- yy - yy[x=1]
      bts[j,] <- yy 
    }
  } else if (level == 3) {
    for (j in 1:100) {
      yy <- x*coefs[j,1] + x^2*coefs[j,2] + x^3*coefs[j,3] 
      yy <- yy - yy[x=1]
      bts[j,] <- yy 
    }
  }
  
  #figure out the 95 and 5 percentiles of the bootstraps
  confint <- apply(bts,2,function(x) quantile(x,probs=c(0.05,0.5,0.95), na.rm = T)) 
  
  #plot median estimate among the bootstraps
  plot(x, confint[2,], type = "l", las=1,xlab="Temperature (Celsius)",ylab="Mortality", col="navy", main=paste(title))   
  #plot confidence intervals
  polygon(c(x,rev(x)),c(confint[1,],rev(confint[3,])),col=adjustcolor("navy", alpha=.3),border = NA)
  rug(data$measure, side = 1)
  
}


####Regressions####

#### W/O interactions ####

##All deaths + Avg Max Temp in County
t_max <- t %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(maximum_temp_in_county = max(max_max, na.rm = T),
            avg_max_temp_in_county = mean(max_max, na.rm = T))

data <- left_join(m, t_max, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = avg_max_temp_in_county - 273.15, monthyear, county = county.x) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure))

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1)
plot_regs(data, boots, "All Deaths + Avg Max Temp in County", level = 1)
boots <- bootstrap_data(data, short=T, level=2)
plot_regs(data, boots, "All Deaths + Avg Max Temp in County", level = 2)
boots <- bootstrap_data(data, short=T, level=3)
plot_regs(data, boots, "All Deaths + Avg Max Temp in County", level = 3)

all_high_lin <- fe_model(data, level = 1)
all_high_quad <- fe_model(data, level = 2)
all_high_cub <- fe_model(data, level = 3)

##All deaths + Night Temp in County
t_min <- t %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(max_min_temp_in_county = max(max_min, na.rm = T),
            avg_max_min_temp_in_county = mean(max_min, na.rm = T))

data <- left_join(m, t_min, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = avg_max_min_temp_in_county - 273.15, monthyear, county = county.x) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure))

boots <- bootstrap_data(data, short=T, level=1)
plot_regs(data, boots, "All Deaths + Avg Max Low Temp in County", level = 1)
boots <- bootstrap_data(data, short=T, level=2)
plot_regs(data, boots, "All Deaths + Avg Max Low Temp in County", level = 2)
boots <- bootstrap_data(data, short=T, level=3)
plot_regs(data, boots, "All Deaths + Avg Max Low Temp in County", level = 3)

all_low_lin <- fe_model(data, level = 1)
all_low_quad <- fe_model(data, level = 2)
all_low_cub <- fe_model(data, level = 3)

##Plot betas for significance 
betas <- dwplot(list(all_high_lin, all_high_quad, all_high_cub, all_low_lin, all_low_quad, all_low_cub)) +
  ggtitle("Estimate of Mortality in Response to Temperature") +
  theme_linedraw() +
  theme(plot.title = element_text(face="bold"),
        legend.title = element_blank()) +
  #legend.text = element_text(c("Max Temp, Linear", 
  #               "Max Temp, Quadratic",
  #              "Max Low Temp, Linear",
  #             "Max Low Temp, Quadratic"))) +
  coord_flip() +
  xlim(-.5, 2) 
betas + geom_hline(yintercept = 0, colour = "black", linetype = 2) 


#### W interactions ####
t_max <- t %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(maximum_temp_in_county = max(max_max, na.rm = T),
            avg_max_temp_in_county = mean(max_max, na.rm = T))

data <- left_join(m, t_max, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = avg_max_temp_in_county - 273.15, monthyear, county = county.x, median_income) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure))

par(mfcol = c(3,2))
boots <- bootstrap_data(data, short=T, level=1, interact=T)
plot_regs(data, boots, "All Deaths + Avg Max Temp in County w/Interaction", level = 1)
boots <- bootstrap_data(data, short=T, level=2, interact=T)
plot_regs(data, boots, "All Deaths + Avg Max Temp in County w/Interaction", level = 2)
boots <- bootstrap_data(data, short=T, level=3, interact=T)
plot_regs(data, boots, "All Deaths + Avg Max Temp in County w/Interaction", level = 3)

all_high_int_lin <- fe_model(data, level = 1)
all_high_int_quad <- fe_model(data, level = 2)
all_high_int_cub <- fe_model(data, level = 3)

##All deaths + Night Temp in County
t_min <- t %>% group_by(county, fips, month, year, monthyear) %>%
  summarize(max_min_temp_in_county = max(max_min, na.rm = T),
            avg_max_min_temp_in_county = mean(max_min, na.rm = T))

data <- left_join(m, t_min, by = c("fips", "month", "year"))
data <- data %>% 
  group_by(fips, measure = avg_max_min_temp_in_county - 273.15, monthyear, county = county.x) %>% 
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  dplyr::filter(is.finite(measure))

boots <- bootstrap_data(data, short=T, level=1, interact = T)
plot_regs(data, boots, "All Deaths + Avg Max Low Temp in County w/Interaction", level = 1)
boots <- bootstrap_data(data, short=T, level=2, interact = T)
plot_regs(data, boots, "All Deaths + Avg Max Low Temp in County w/Interaction", level = 2)
boots <- bootstrap_data(data, short=T, level=3, interact = T)
plot_regs(data, boots, "All Deaths + Avg Max Low Temp in County w/Interaction", level = 3)

all_low_int_lin <- fe_model(data, level = 1)
all_low_int_quad <- fe_model(data, level = 2)
all_low_int_cub <- fe_model(data, level = 3)

##Plot betas for significance 
betas <- dwplot(list(mod1, mod2, mod3, mod4)) +
  ggtitle("Estimate of Mortality in Response to Temperature w/Interaction") +
  theme_linedraw() +
  theme(plot.title = element_text(face="bold"),
        legend.title = element_blank()) +
  #legend.text = element_text(c("Max Temp, Linear", 
  #               "Max Temp, Quadratic",
  #              "Max Low Temp, Linear",
  #             "Max Low Temp, Quadratic"))) +
  coord_flip() +
  xlim(-.5, 2) 
betas + geom_hline(yintercept = 0, colour = "black", linetype = 2) 


