library(tidyverse)
library(dplyr)
library(modelr)
options(na.action = na.warn)

ggplot(sim1,aes(x,y)) + geom_point()

models <- tibble(a1 = runif(250,-20,40), a2 = runif(250, -5,5))

#geom_abline takes the slope and intercept as parameters to produce possible trend lines for the data
ggplot(sim1,aes(x,y)) + geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 0.25) +geom_point()

#Need a good way to quantify the fit of the model, using the distance between the observed value and the predictted value
#To compute this distance we first turn our modle into a R function, this take the model paraeters and the data as inputs
# and gives back the values predicted as output

# a is a vector with the intercept and slope, data is the dataset from which to predict the output
model1 <- function(a,data) {
  a[1] + data$x * a[2]
}

model1(c(7,1.5), sim1)

#next we compute the sum of the distances for all the points in the data set, against the predicted model
#Root-mean-squared-deviation - Compute the distance between predicted and actual for all points, square them,
# average them and square root the result

measure_distance <- function(mod,data) {
  diff <- data$y - model1(mod,data)
  sqrt(mean(diff^2))
}

measure_distance(c(7,1.5),sim1)

#Function to measure the distance for multiple models to determine the best model available
sim1_dist <- function(a1,a2) {
  measure_distance(c(a1,a2),sim1)
}

#Repeat the process for multiple inputs and adding a column of distance to each model
models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1,a2,sim1_dist))

models

#filtering the top 10 results of the distance calculation, and plotting them on the data scatter plot
ggplot(sim1,aes(x,y)) + geom_point(size=2, color = "grey30") + geom_abline( 
  aes(intercept = a1, slope = a2, color = dist), data = filter(models,rank(dist)<=10))

ggplot(models,aes(a1,a2)) + 
  geom_point(data = filter(models, rank(dist)<=10),size = 4, color = "red")+ 
  geom_point(aes(color = -dist))

# Generate an even spread of points and the top 10 models are highlighted
grid <- expand.grid(
  a1 = seq(-5,20, length = 25),
  a2 = seq(1,3,length = 25)) %>%
    mutate(dist = purrr::map2_dbl(a1,a2,sim1_dist))

grid %>% ggplot(aes(a1,a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, color = "red") +
  geom_point(aes(colour = -dist))

#Overlay the top 10 models back on the original data
ggplot(sim1,aes(x,y)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(aes(intercept = a1, slope = a2, colour = -dist), data = filter(grid, rank(dist)<=10))

#Optim function to use a newton-raphson method to find the best possible model
best <- optim(c(0,0),measure_distance, data = sim1)
best$par

ggplot(sim1,aes(x,y)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(aes(intercept = best$par[1], slope = best$par[2]))

#call the linear model function to find the best linear model
sim1_mod <- lm(y~x, data = sim1)

# Visualising models
#To visualise the predictions from a model, we start by generating an evenly spaced grid of values that covers the
# region where our data lives. The easiest way to do that is to use modelr::datagrid() 

grid <- sim1 %>%
  data_grid(x)

#add predictions from our model for a series of x values
grid <- grid %>% 
  add_predictions(sim1_mod)

ggplot(sim1,aes(x)) + geom_point(aes(y=y))+ geom_line(aes(y=pred),data = grid, colour = "red", size = 1)

# Add residual column to the table
sim1 <- sim1 %>% 
  add_residuals(sim1_mod)

ggplot(sim1,aes(resid)) + geom_freqpoly(binwidth = 0.5)
ggplot(sim1,aes(x,resid)) + geom_point() + geom_ref_line(h=0)


#Model matrix, takes a dataframe and a model and returns a tibble that defines the model equation

df <- tribble(
  ~y, ~x1, ~x2,
  4,2,5,
  5,1,6
)

model_matrix(df,y~x1)
model_matrix(df,y~x1 + x2)

#Categorical Variables
df <- tribble (
  ~sex, ~response,
  "male",1,
  "female",2,
  "male", 1
)

model_matrix (df, response ~ sex )

ggplot(sim2) + geom_point(aes(x,y))

mod2 <- lm(y~x, data = sim2)

grid <- sim2 %>% data_grid(x) %>%
  add_predictions(mod2)

grid

ggplot(sim2,aes(x)) + geom_point(aes(y=y)) + geom_point(data = grid, aes(y=pred), colour = "red",size = 4)

#Continuous and Categorical variables
ggplot(sim3,aes(x1,y)) + geom_point(aes(color = x2))

#When you add variables with the  +, the model will estimate each effect independently of all the others
#When you use the * , both the interaction and the individual components are included in the model
mod1<- lm(y~x1 + x2, data=sim3)
mod2<- lm(y~x1 * x2, data= sim3)

#To visualise the models, we have two functions
#data_grid() find all the unique values of x1 and x2 and then generates all the combinations
#gather_predictions() adds each prediction as a row

grid <- sim3 %>%
  data_grid(x1,x2) %>% 
  gather_predictions(mod1,mod2)

grid

ggplot(sim3, aes(x1,y,color = x2)) + geom_point()+
  geom_line(data = grid, aes(y=pred))+
  facet_wrap(~model)

mod1<- lm(y~x1 + x2, data = sim4)
mod2<- lm(y~x1 * x2, data = sim4)

grid <- sim4 %>%
  data_grid(
    x1 = seq_range(x1,5),
    x2 = seq_range(x2,5)
  ) %>%
  gather_predictions(mod1,mod2)

