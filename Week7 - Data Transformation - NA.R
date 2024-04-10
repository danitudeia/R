

library(tidyverse)
library(nycflights13)

?flights

unique(flights$carrier)

length(unique(flights$carrier))

sort(unique(flights$carrier))

filter(flights, dep_time %in% c(517, 533))

sum(flights$dep_delay) # ****has NA values so the statics formulas don't work
max(flights$dep_delay) # ****has NA values so the statics formulas don't work

sum(is.na(flights$dep_delay)) # *****counting the number of NAs

sum(is.na(flights$dep_delay))/length(flights$dep_delay)*100 # ****share of NA values 

sum(flights$dep_delay, na.rm = TRUE) # ***** ignore the NAs and sum
max(flights$dep_delay, na.rm = TRUE) # ***** ignore the NAs and max
min(flights$dep_delay, na.rm = TRUE) # ***** ignore the NAs and min


# ****************************

flights %>% 
  filter(arr_delay >= 120)


flights %>% 
  filter(dest =='IAH' | dest == 'HOU')


flights %>% 
  filter(dest %in% c('IAH', 'HOU'))


airlines

flights %>% 
  filter(carrier %in% c('UA', 'AA', 'DL'))

flights %>% 
  filter(month %in% c('7', '8', '9'))

high_season <- flights %>% 
  filter(month %in% c(7, 8, 9))

unique(high_season$month)


flights %>% 
  filter(arr_delay > 120 & dep_delay <=0)


flights %>% 
  filter(dep_delay >=60 & (dep_delay - arr_delay) > 30)


# ******************************
min(flights$dep_time, na.rm = TRUE)
max(flights$dep_time, na.rm = TRUE)

midnight_to_6 <- flights %>% 
  filter(dep_time <=600 | dep_time ==2400)

sort(unique(midnight_to_6$dep_time))


missing_dep_time <- flights %>% 
  filter(is.na(dep_time))

clean_dep_time <- flights %>% 
  filter(!is.na(dep_time))


# *********DATA TRANSFORMATION III******

library(gapminder)

?gapminder

gdp <- gapminder %>% 
  mutate(GDP = pop * gdpPercap)

view(gdp)

gdp_norm <- gdp %>% 
  mutate(gdp_norm = GDP / 1000000000 )

view(gdp_norm)

pop_norm <- gdp %>% 
  mutate(pop = pop / 1000000) # ******modifica o valor das infos das colunas

gdp %>% 
  transmute (population = pop) # ********filtra a coluna

gdp %>% 
  rename (population = pop) # *****renomeia a coluna

gdp %>% 
  arrange (lifeExp) # ***ordena as linhas pela feature selecionada

gdp %>% 
  arrange (desc(gdpPercap)) # ***ordena descendente as linhas pela feature selecionada

gdp %>% 
  arrange(year, pop) # ***ordena mais de uma feature


gdp %>% 
  arrange(pop, year) # ***ordena mais de uma feature


flights %>% 
  arrange (desc(dep_delay)) %>% 
  select (dep_delay, arr_delay) %>% 
  mutate(dep_delay = dep_delay / 60) 


flights %>% 
  arrange (dep_delay)

flights %>% 
  arrange(air_time)


# ******* Section 5.5.2 Exercises

flights %>% 
  arrange(air_time)

((flights$dep_time %/% 100)*60 + flights$dep_time %% 100) %% 1440 #***precisa incluir o 1440 pois 24h00 eh igual a 0 minutos

flights %>% 
  mutate(dep_in_min = ((flights$dep_time %/% 100)*60 + flights$dep_time %% 100) %% 1440)  %>% 
  select (dep_time, dep_in_min)
 

time_in_min <- function(x) {
  ((x %/% 100)*60 + x %% 100) %% 1440
}

flights %>% 
  mutate (dep_in_min = time_in_min(dep_time) ) %>% 
  select (dep_time, dep_in_min)

