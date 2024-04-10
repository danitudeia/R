# *********DATA VISUALIZATION PART II - R4DS_Chapter_3b*****

library(tidyverse)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, 
                           y = hwy,
                           colour = cty))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, 
                           y = hwy,
                           size = cty))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, 
                           y = hwy,
                           shape = cty)) # R so tem 5 ou 6 diferentes tipos de formas disponiveis, por isso da erro, city tem muito mais dados que 5 ou 6


# CATEGORICAL DATA

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, 
                           y = hwy,
                           colour = drv))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, 
                           y = hwy,
                           size = drv))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, 
                           y = hwy,
                           shape = drv,
                           color = drv))

ggplot(data = mpg) +
  geom_point(mapping = aes()) 


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cty, 
                           y = hwy,
                           color = displ < 5))


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cty, 
                           y = hwy,
                           color = displ < mean(displ)))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cty, 
                           y = hwy,
                           color = class))

# *********DATA VISUALIZATION PART III - R4DS_Chapter_3c*****

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

#GLOBAL MAPPING

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() +
  geom_smooth() 

# GLOBAL + LOCAL MAPPING

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(shape = fl)) +
  geom_smooth(mapping = aes(colour = drv))

ggplot(mapping = aes(x = displ, y = hwy), data = mpg) + 
  geom_point()


ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = drv))  + 
  geom_point()

ggplot(mpg)  + 
  geom_point(aes(x = displ, y = hwy, colour = drv))


ggplot(mpg)  + 
  geom_point(aes(displ, hwy, colour = drv))


# *********Aesthetics vs Attributes**********

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = cty))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, 
                           y = hwy), 
             size = 5)


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, 
                           y = hwy, 
                           size = cty, 
                           colour = cty))


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, 
                           y = hwy), 
             size = 3, 
             colour = 'pink')



ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(shape = 24,
             colour = 'blue', 
             fill = 'red') +
  geom_smooth()


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color ="blue"))

# **************DATA VISUALIZATION IV **************

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow =2) # NUMERO DE LINHAS EM QUE SERAO APRESENTADAS AS FACETAS

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~ class, ncol = 3) # NUMERO DE COLUNAS EM QUE SERAO APRESENTADAS AS FACETAS

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ class)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ class)

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) + 
  facet_wrap(~ cty)
