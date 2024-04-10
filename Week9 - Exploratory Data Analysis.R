library (tidyverse)

wages <- read_csv("http://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Bwages.csv")

glimpse(wages)

length(is.na(wages$sex)) / length(wages$sex) *100

summary(wages)

length(unique(wages$...1))

wages_clean <- wages %>% 
  select(-sex,-...1) %>% 
  mutate(educ = factor(educ))

wages_clean %>% 
  ggplot(aes(x=wage))+
  geom_histogram(fill = 'skyblue',
                 colour = 'black')

range_wage <- max(wages_clean$wage) - min(wages_clean$wage)
default_bin <- range_wage / 30

wages_clean %>% 
  ggplot(aes(x=wage))+
  geom_histogram(fill = 'skyblue',
                 colour = 'black',
                 binwidth = default_bin)


range_wage <- max(wages_clean$wage) - min(wages_clean$wage)
default_bin <- range_wage / 15

wages_clean %>% 
  ggplot(aes(x=wage))+
  geom_histogram(fill = 'skyblue',
                 colour = 'black',
                 binwidth = default_bin)


wages_clean %>% 
  ggplot(aes(x=wage))+
  geom_histogram(fill = 'skyblue',
                 colour = 'black',
                 binwidth = 2)

wages_clean %>% 
  ggplot(aes(x=wage))+
  geom_histogram(fill = 'skyblue',
                 colour = 'black',
                 binwidth = 2)+
  geom_vline(xintercept= mean(wages_clean$wage),
             color = 'red')+
  geom_vline(xintercept= median(wages_clean$wage),
             color = 'blue')
  


wages_clean %>% 
  filter (wage > 35)


wages_clean %>% 
  group_by(educ) %>% 
  mutate(rank = rank(-wage)) %>% 
  filter(rank <= 2) %>% 
  arrange(rank)

wages_clean %>% 
  group_by(educ) %>% 
  mutate(rank = rank(-wage)) %>% 
  filter(rank <= 2) %>% 
  arrange(rank)


sd(wages_clean$wage)
IQR(wages_clean$wage)
# ---------------------------------------------

wages_clean <- read_csv("http://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Bwages.csv") %>% 
  select(-sex,-...1) %>% 
  mutate(educ = factor(educ)) %>% 
  rename(education = educ) # ****alterar nome da coluna

wages_clean %>% 
  ggplot(aes(education))+
  geom_bar(fill='skyblue',
           color='black')


wages_clean %>% 
  group_by(education) %>% 
  summarise(n=n())


wages_clean %>% 
  ggplot(aes(education, y=..prop.., group=1))+
  geom_bar(fill='skyblue',
           color='black')


wages_clean %>% 
  group_by(education) %>% 
  summarise(n=n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  mutate(prop_tot = sum(prop)) 


wages_clean %>% 
  group_by(education) %>% 
  summarise(n=n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  summarize(prop_tot = sum(prop)) 


wages_clean %>% 
  group_by(education) %>% 
  summarise(n=n()) %>% 
  arrange(n)


# *****Exploratory Data Analysis - Part II

library(tidyverse)
library(here)
library(gridExtra) # ***for plots side by side

norms <- tibble(x_wider = rnorm(1000, mean = 0, sd = 10), 
                x_narrower = rnorm(1000, mean = 0, sd = 1))


p1 <- ggplot(norms, aes(x = x_wider)) +
  geom_histogram(fill = 'skyblue2', colour = 'black', binwidth = 5) +
  coord_cartesian(xlim = c(-30, 30)) +
  ggtitle("Mean = 0 and Standard deviation = 10")


p2 <- ggplot(norms, aes(x = x_narrower)) +
  geom_histogram(fill = 'skyblue2', colour = 'black', binwidth = 0.5) +
  coord_cartesian(xlim = c(-30, 30)) +
  ggtitle("Mean = 0 and Standard deviation = 1")


grid.arrange(p1, p2)

------------
  norms_2 <- tibble(x_wider = rnorm(1000, mean = 0, sd = 10), 
                    x_wider_2 = rnorm(1000, mean = 15, sd = 10))


p3 <- ggplot(norms_2, aes(x = x_wider)) +
  geom_histogram(fill = 'skyblue2', colour = 'black', binwidth = 5) +
  coord_cartesian(xlim = c(-30, 45)) +
  ggtitle("Mean = 0 and Standard deviation = 10")


p4 <- ggplot(norms_2, aes(x = x_wider_2)) +
  geom_histogram(fill = 'skyblue2', colour = 'black', binwidth = 5) +
  coord_cartesian(xlim = c(-30, 45)) +
  ggtitle("Mean = 15 and Standard deviation = 10")


grid.arrange(p3, p4)

---------------------------------
  
  norms_3 <- tibble(x_wider = rnorm(1000, mean = 15, sd = 10), 
                    x_narrower = rnorm(1000, mean = 28, sd = 1))


p5 <- ggplot(norms_3, aes(x = x_wider)) +
  geom_histogram(fill = 'skyblue2', colour = 'black', binwidth = 5) +
  coord_cartesian(xlim = c(-20, 45)) +
  ggtitle("Mean = 15 and Standard deviation = 10")


p6 <- ggplot(norms_3, aes(x = x_narrower)) +
  geom_histogram(fill = 'skyblue2', colour = 'black', binwidth = 0.5) +
  coord_cartesian(xlim = c(-20, 45)) +
  ggtitle("Mean = 28 and Standard deviation = 1")


grid.arrange(p5, p6)

-----------------
  
  norms_4 <- tibble(x_wider = rnorm(1000, mean = 15, sd = 10), 
                    x_narrower = rnorm(1000, mean = 28, sd = 1))


p7 <- ggplot(norms_4, aes(x = x_wider)) +
  geom_histogram(fill = 'skyblue2', colour = 'black', binwidth = 5) +
  ggtitle("Mean = 15 and Standard deviation = 10")


p8 <- ggplot(norms_4, aes(x = x_narrower)) +
  geom_histogram(fill = 'skyblue2', colour = 'black', binwidth = 0.5) +
  ggtitle("Mean = 28 and Standard deviation = 1")


grid.arrange(p7, p8)


-------------
  
wages <- read_csv("http://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Bwages.csv") %>% 
  select(-...1, -sex) %>% 
  mutate(educ = factor(educ)) 

wages %>% 
  summarise(wages_var = sum((wage - mean(wage))**2) / (n()-1))

var(wages$wage)

---------------
wages %>% 
  summarise(mean_wages = mean(wage),
            sd_wage = sd(wage),
            median_wage = median(wage),
            iqr_eage = IQR(wage))

---------------
anscombe %>% 
  select(x1:x4) %>% 
  summarise_all(funs(mean, sd))
  

anscombe %>% 
  select(contains('x')) %>% 
  summarise_all(funs(mean, sd))

anscombe %>% 
  select(y1:y4) %>% 
  summarise_all(funs(mean, sd))


anscombe %>% 
  select(contains('y')) %>% 
  summarise_all(funs(mean, sd))
  
----------------------------------
  
anscombe_x <- anscombe %>% select(x1, x2, x3, x4) %>% 
  gather(set_x, x, x1:x4) %>% 
  mutate(set_x = str_replace(set_x, 'x', ''))

anscombe_y <- anscombe %>% select(y1, y2, y3, y4) %>% 
  gather(set_y, y, y1:y4) %>% 
  mutate(set_y = str_replace(set_y, 'y', ''))

anscombe_tidy <- cbind(anscombe_x, anscombe_y) %>%
  select(-set_x) %>% 
  rename(set = set_y) 

------------------

anscombe_tidy %>% 
  group_by(set) %>% 
  summarize(mean_x = mean(x),
            sd_x = sd(x),
            mean_y = mean(y),
            sd_y = sd(y))


anscombe_tidy %>% 
  group_by(set) %>% 
  select(x,y) %>% 
  summarize_all(funs(mean, sd))

------------------
anscombe_tidy %>% 
  ggplot(aes(x=x , y=y, color=set))+
  geom_point()+
  facet_wrap(~set)
  
