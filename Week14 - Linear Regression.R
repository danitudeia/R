# **********INTRO LINEAR REGRESSION******
library(tidyverse)

download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
load("mlb11.RData")


mlb11 %>% 
  ggplot(aes(x=at_bats,y=runs))+
  geom_point()+
  geom_smooth(method = 'lm')

cor(mlb11$runs, mlb11$at_bats)

# This function takes in a value for the slope and intercept of a line and plots
# the original data points plus the specified line. It also shows the residuals 
# and calculates the sum of the squares of the residuals and prints that in the title.

# We can think of any line as a model for the relationship between runs and at_bats,
# although some of these models (lines) will be much worse representations of our 
# data than others. This function allows you to explore finding the line that 
# minimizes the sum of the squared residuals throught trial and error. 

best_line <- function(slope, intercept, residuals = TRUE) {
  mlb <- mlb11 %>% mutate(y_predicted = intercept + slope * at_bats,  
                          square_residuals = (runs - y_predicted)^2)
  
  ssr <- mlb %>% summarize(ssr = round(sum(square_residuals), 0))
  
  if (residuals) {
    p <- ggplot(mlb, aes(x = at_bats, y = runs)) +
      geom_point(shape = 21, fill = 'skyblue', size = 2) + 
      geom_line(aes(x = at_bats, y = y_predicted), colour = 'blue') + 
      geom_segment(aes(x = at_bats, y = runs, 
                       xend = at_bats, yend = y_predicted), linetype = 2) +
      ggtitle(paste0("Sum of Squared Residuals: ", ssr[[1]]))
  } else {    
    p <- ggplot(mlb, aes(x = at_bats, y = runs)) +
      geom_point(shape = 21, fill = 'skyblue', size = 2) + 
      geom_line(aes(x = at_bats, y = y_predicted), colour = 'blue')
  }
  p
}

best_line(.55, -2000)
best_line(.55, -2300)
best_line(.55, -2350) # best slope and intercept - sum of squared residuals = 125888


m1 <- lm(runs ~ at_bats, data = mlb11)
summary(m1)

best_line(.6305, -2789.2429) # best slope and intercept - using the lm function




mlb11 %>% 
  ggplot(aes(x=homeruns,y=runs))+
  geom_point()+
  geom_smooth(method = 'lm')

cor(mlb11$runs, mlb11$homeruns)


best_line <- function(slope, intercept, residuals = TRUE) {
  mlb <- mlb11 %>% mutate(y_predicted = intercept + slope * homeruns,  
                          square_residuals = (runs - y_predicted)^2)
  
  ssr <- mlb %>% summarize(ssr = round(sum(square_residuals), 0))
  
  if (residuals) {
    p <- ggplot(mlb, aes(x = homeruns, y = runs)) +
      geom_point(shape = 21, fill = 'skyblue', size = 2) + 
      geom_line(aes(x = homeruns, y = y_predicted), colour = 'blue') + 
      geom_segment(aes(x = homeruns, y = runs, 
                       xend = homeruns, yend = y_predicted), linetype = 2) +
      ggtitle(paste0("Sum of Squared Residuals: ", ssr[[1]]))
  } else {    
    p <- ggplot(mlb, aes(x = homeruns, y = runs)) +
      geom_point(shape = 21, fill = 'skyblue', size = 2) + 
      geom_line(aes(x = homeruns, y = y_predicted), colour = 'blue')
  }
  p
}

best_line(.57, 600)

m1 <- lm(runs ~ homeruns, data = mlb11)
summary(m1)


best_line(1.8345, 415.2389) # ****scope is like a rate - each homerun means 1.8 runs


best_line <- function(x, slope, intercept, residuals = TRUE) {
  mlb <- mlb11 %>% mutate(y_predicted = intercept + slope * x,  
                          square_residuals = (runs - y_predicted)^2)
  
  ssr <- mlb %>% summarize(ssr = round(sum(square_residuals), 0))
  
  if (residuals) {
    p <- ggplot(mlb, aes(x = x, y = runs)) +
      geom_point(shape = 21, fill = 'skyblue', size = 2) + 
      geom_line(aes(x = x, y = y_predicted), colour = 'blue') + 
      geom_segment(aes(x = x, y = runs, 
                       xend = x, yend = y_predicted), linetype = 2) +
      ggtitle(paste0("Sum of Squared Residuals: ", ssr[[1]]))
  } else {    
    p <- ggplot(mlb, aes(x = x, y = runs)) +
      geom_point(shape = 21, fill = 'skyblue', size = 2) + 
      geom_line(aes(x = x, y = y_predicted), colour = 'blue')
  }
  p
}


best_line(mlb11$homeruns, 1.8345, 415.2389) # ****scope is like a rate - each homerun means 1.8 runs

# ********EXERCISE 6

best_line(mlb11$at_bats, .6305, -2789.2429, residuals = FALSE)


mlb11 %>% 
  ggplot(aes(x=at_bats, y=runs))+
  geom_point()+
  geom_abline(slope =  .6305 , intercept=-2789.2429)+
  geom_smooth(color='skyblue', method='lm', se=FALSE) # *** "se" means the shadow




slope <- .6305
intercept <- -2789.2429

y_hat <-intercept + slope *5578
y_hat

filter_mlb <-mlb11 %>% 
  filter(between(at_bats, 5578, 5580))


error <- filter_mlb$runs - y_hat         
error

# --------------------
m1 <- lm(runs ~ at_bats, data = mlb11)
summary(m1)

mean_x <- mean(mlb11$at_bats)
mean_y <- mean(mlb11$runs)

sd_x <- sd(mlb11$at_bats)
sd_y <- sd(mlb11$runs)

cor_xy <- cor(mlb11$runs,mlb11$at_bats)

slope <- sd_y/sd_x *cor_xy

intercept <- mean_y - slope * mean_x

mlb11_mod <- mlb11 %>% 
  mutate(pred_runs = intercept + slope * at_bats,
         residuals = runs - pred_runs) %>% 
  select(team, runs, at_bats, pred_runs, residuals)

mlb11_mod %>% 
  ggplot(aes(x = at_bats, y=residuals))+
  geom_point() # **** good model because there are no relation between the error and the x value

mlb11_mod %>% 
  ggplot(aes(x = residuals))+
  geom_histogram(color = 'black',
                 fill='skyblue',
                 binwidth = 55) # **** good model because there concentration is on zero

mlb11_mod %>% 
  ggplot(aes(x = at_bats, y=residuals))+
  geom_point()+ # **** good model because there are no pattern on residuals
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 100, color='blue')+
  geom_hline(yintercept = -100, color='blue')
  
library(broom)
m1_aug <- augment(m1) %>% 
  select(runs, at_bats, predicted = .fitted, residuals = .resid)
m1_aug # **WE CAN NOT USE THIS ON OUR PROJECT


# *****DA PRA CRIAR UMA FUNCAO COM AS VARIAVEIS E CALCULAR O SLOPE E INTERCEPTION

slo_inte <- function(x,y){
mean_x <- mean(x)
mean_y <- mean(y)

sd_x <- sd(x)
sd_y <- sd(y)

cor_xy <- cor(y,x)

slope <- sd_y/sd_x *cor_xy

intercept <- mean_y - slope * mean_x

return (c(slope, intercept))
}

slo_inte(mlb11$at_bats, mlb11$runs)

# EXERCISE 

mlb11 %>% 
  ggplot(aes(x=hits,y=runs))+
  geom_point()+
  geom_smooth(method = 'lm')

cor(mlb11$runs, mlb11$hits)

slo_inte(mlb11$hits, mlb11$runs)

m1 <- lm(runs ~ hits, data = mlb11)
summary(m1)


