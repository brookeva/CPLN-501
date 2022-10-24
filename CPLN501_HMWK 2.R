#CPLN501, FALL 2022#
#REGRESSION IN POPULATION TREND#
#TIPS FOR ASSIGNMENT 2#
rm(list=ls())

#CREATING DATA####
#make up some data
year <- c(1950, 1960, 1970, 1980, 1990, 2000, 2010)
population <- c(15, 29, 77, 234, 366, 496, 631)  
dat <- cbind.data.frame(year, population)
#for your assignment, you need to collect data for Philly's population between 1950 and 2010
#and plug them into the commands above

#the assignment asks you to estimate three regression models
#1. linear
#2. non-linear
#3. lag
#and use each model to project population

#LINEAR TREND####
#TASK: first, let's estimate a linear trend####
mod.1 <- lm( population ~ year, data = dat) 
summary(mod.1)

#to project population in 2020 and 2030, we can feed a new data set into the predict function
#TASK: creating new data frame futurepop with one variable, two observations (2020 and 2030)
futurepop <- data.frame(c(2020,2030))
#TASK: make sure the variable in the new dataset has same name as indep. var. in model
colnames(futurepop) <-"year" 
#TASK: create a new variable called pred_linear in futurepop, then store the predicted population in the variable
futurepop$pred_linear<- predict(mod.1, newdata = futurepop)

#TASK: visualize the regression line
plot(dat$year, dat$population,
     main = "Linear model", #give plot a title
     xlab = "year", ylab = "population", #add labels for x and y axes
     col = "red", pch = 6)
abline(mod.1)
#is this a good prediction for 2020? why or why not?

#NON-LINEAR TREND####
#TASK: next, let's try a model with a quadratic term
mod.2 <- lm(population ~  year + I(year^2), data = dat)
summary(mod.2)

#will this model likely do a good job at predicting 2020 population? 
#TASK: plot the base plot
plot(dat$year, dat$population,
     main = "quadratic model",
     xlab="Year",ylab = "Population",
     col="blue", pch="$")

#TASK: plot the predicted population
points(dat$year, predict(mod.2, newdata = dat), col = "red", pch = 16)
#TASK: plot smooth line through points
lines(dat$year, predict(mod.2), col = "red") 

#TASK: add another column in futurepop and store predicted population in the column
futurepop$pred_quadratic <-predict(mod.2, newdata = futurepop)

#LAG MODEL####
#TASK: finally, let's try a model with lag
library(tidyverse)
#first, let's create a population with 10 year lag called pop_lag (i.e., shifting one row down)
dat <- dat %>% mutate(pop_lag = lag(population,1))

#TASK: build the model with year and lag
mod.3 <- lm(population ~  year + pop_lag, data = dat)
summary(mod.3)

#TASK: plot the points on base plot
plot(dat$year, dat$population) #base plot
points(dat$year[dat$year>1950],predict(mod.3),
       col="red", pch="@") #make sure x an dy axes have the same length

#TASK: plot the line that connects the predicted values
lines(dat$year[dat$year>1950], predict(mod.3)) #make sure x an dy axes have the same length 

#TASK: predict population of 2020 and 2030
#hint: mod.3$coefficients PICK UP HERE
pop2020 <- 
pop2030 <- 

#TASK: store predicted population in a new column in futurepop
pred_lag <- c(pop2020, pop2030)
futurepop <- cbind.data.frame(futurepop, pred_lag)

#based on your judgment, which model will likely predict the best?
#TASK (OPTIONAL): plot all three trends on same plot
plot(dat$, dat$,
     main = "", 
     xlab = "", ylab = "", 
     col = "", pch = )
abline()
lines()
lines()
legend(1950, 600, 
       legend=c(),
       col=c(), lty = 1)
