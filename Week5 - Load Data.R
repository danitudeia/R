# *********Data Visualization: Practice******

install.packages('gridExtra')
install.packages("gapminder")

library(tidyverse)
library(gapminder)

# ******Exercises 1 to 3*****
my_data <- gapminder
?gapminder

# ***** Exercise 4*******
summary(my_data)

# *****Exercise 5**********

sort(unique(gapminder$country))

# *****Exercise 6*******

country <- gapminder %>% filter(country == "Ireland")

# **** Exercise 7 *****
country

# **** Exercise 9 *****

ggplot(data = country) + 
  geom_point(mapping = aes(x = year, 
                           y = lifeExp))

# **** Exercise 10 *****
ggplot(data = country) + 
  geom_point(mapping = aes(x = year, 
                           y = lifeExp,
                           size = gdpPercap))

# **** Exercise 11 *****
ggplot(data = country) + 
  geom_point(mapping = aes(x = year, 
                           y = lifeExp,
                           size = gdpPercap)) +
  geom_line(mapping = aes(x = year, 
                          y = lifeExp))
  
# **** Exercise 12 *****
ggplot(data = country) + 
  geom_point(mapping = aes(x = year, 
                           y = lifeExp,
                           size = gdpPercap)) +
  geom_line(mapping = aes(x = year, 
                          y = lifeExp),
            colour = 'blue',
            size = 0.8)

# **** Exercise 13 *****
ggplot(data = country) + 
  geom_point(mapping = aes(x = year, 
                           y = lifeExp,
                           size = gdpPercap),
             shape = 21,
             colour = 'blue',
               fill = 'red') +
geom_line(mapping = aes(x = year, 
                        y = lifeExp),
          colour = 'blue',
          size = 0.8)

# **** Exercise 14 *****
ggplot(data = country) + 
  geom_line(mapping = aes(x = year, 
                          y = lifeExp),
            colour = 'blue',
            size = 0.8) +
  geom_point(mapping = aes(x = year, 
                           y = lifeExp,
                           size = gdpPercap),
             shape = 21,
             colour = 'blue',
             fill = 'red')

# **** Exercise 15 and 16 *****
ggplot(data = country) + 
  geom_line(mapping = aes(x = year, 
                          y = lifeExp),
            colour = 'blue',
            size = 0.8) +
  geom_point(mapping = aes(x = year, 
                           y = lifeExp,
                           size = gdpPercap),
             shape = 21,
             colour = 'blue',
             fill = 'red')+
  labs(x ='Year')+
  labs(y = 'Life Expectancy (Years)')
  


# **** Exercise 17 and 18 *****
ggplot(data = country) + 
  geom_line(mapping = aes(x = year, 
                          y = lifeExp),
            colour = 'blue',
            size = 0.8) +
  geom_point(mapping = aes(x = year, 
                           y = lifeExp,
                           size = gdpPercap),
             shape = 21,
             colour = 'blue',
             fill = 'red')+
  labs(x ='Year')+
  labs(y = 'Life Expectancy at Birth (Years)')+
  ggtitle('Evolution of Life Expectancy and GDP over the Years')+
  guides(size = guide_legend(title = "GDP per capita (US$, inflation-adjusted)"))

# **** Exercise 19 *****
ggplot(data = country) + 
  geom_line(mapping = aes(x = year, 
                          y = lifeExp)) +
  geom_point(mapping = aes(x = year, 
                           y = lifeExp,
                           size = gdpPercap))+
  labs(x ='Year')+
  labs(y = 'Life Expectancy at Birth (Years)')+
  ggtitle('Evolution of Life Expectancy and GDP over the Years')+
  guides(size = guide_legend(title = "GDP per capita (US$, inflation-adjusted)"))+
 theme_test()

