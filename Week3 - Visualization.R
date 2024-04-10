
# ************DATA VISUALIZATION PART 1************

install.packages("tidyverse")
library(tidyverse)

mpg

my_date <- mpg

dim (mpg)

head(mpg, n=2) # Traz as top 1 linhas
tail(mpg) # Traz as ultimas 6 linhas

?mpg

unique(mpg$fl)
unique(mpg$class)
unique(mpg$cty)
glimpse(mpg) # traz os tipos das colunas (features)

# *********CREATING PLOTS

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = cty))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = hwy, y = cty))


ggplot(data = mpg) +
  geom_point(mapping = aes(x = class, y = drv))


ggplot(data = mpg) + 
  geom_histogram(mapping = aes(x = hwy), 
                 fill = 'pink', 
                 colour = 'red', 
                 binwidth = 2)

ggplot(data = mpg) + 
  geom_bar(mapping = aes(x = class), # NAO FAZ HISTOGRAMA COM DADOS CATEGORICOS
                 fill = 'pink', 
                 colour = 'red')

ggplot(data = mpg) + 
  geom_bar(mapping = aes(y = manufacturer), # NAO FAZ HISTOGRAMA COM DADOS CATEGORICOS
           fill = 'pink', 
           colour = 'red')
  