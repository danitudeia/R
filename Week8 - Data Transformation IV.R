library(tidyverse)
library(gapminder)
library(nycflights13)

gapminder %>% 
  summarize(avg_life = mean(lifeExp)) # ****create a dataset with the information

mean(gapminder$lifeExp) # *****show the mean as value

gapminder %>% 
  summarize(avg_gdp = mean(gdpPercap)) # *****media

gapminder %>% 
  summarize(sd_pop = sd(pop)) # ****desvio padrao

gapminder %>% 
  summarize(n_distinct(continent)) # *****distintos valores

gapminder %>% 
  summarize(last(country)) # ******ultimo item

gapminder %>% 
  select ( lifeExp, pop, gdpPercap) %>% 
  summarize_all(funs(mean, median)) # ****aplica o summerize a todos os campos selecionados no SELECT


gapminder %>% 
  group_by(country) %>% 
  summarize(number_samples = n()) # agrupa e conta os registros


gapminder %>% 
  group_by(continent) %>% 
  summarize(number_samples = n_distinct(country)) # agrupa por continente e conta os distintos paises


gapminder %>% 
  group_by(continent,year) %>% 
  summarize(avg_life = mean(lifeExp)) %>% # agrupa por continente e ano e traz a media da expectativa de vida
  ggplot(aes(x = year, y=avg_life, colour = continent))+
  geom_line()

gapminder %>% 
  group_by(continent,year) %>% 
  summarize(avg_life = mean(lifeExp)) %>% # agrupa por continente e ano e traz a media da expectativa de vida
  ggplot(aes(x = year, y=avg_life))+
  geom_line()+
  facet_grid(vars(continent))


gapminder %>% 
  group_by(year) %>% 
  summarize(max_gdp = max(gdpPercap)) %>% # agrupa por ano e traz a maior gdp
  arrange(-max_gdp)

gapminder %>% 
  group_by(year) %>% 
  summarize(min_gdp = min(gdpPercap)) %>% # agrupa por ano e traz a menor gdp
  arrange(-min_gdp)

head(flights)

flights %>% 
  group_by(carrier) %>% 
  summarize(avg_delay = mean(dep_delay)) %>% # agrupa por airline e traz o maior atraso
  arrange(desc(avg_delay))

flights %>% 
  group_by(carrier) %>% 
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) %>% # agrupa por airline e traz o maior atraso retirando NA
  arrange(desc(avg_delay))


flights %>% 
  group_by(month) %>% 
  summarize(n_flights = n()) %>% # agrupa por month e conta
  arrange(desc(n_flights))

flights %>% 
  filter(dep_delay <= 0) %>% 
  group_by(month) %>% 
  summarize(n_flights = n()) %>%  # agrupa por month e conta os que nao atrasaram
  arrange(desc(n_flights))


flights %>% 
  filter(!is.na(dep_delay)) %>% 
  group_by(origin) %>% 
  summarize(delay = mean(dep_delay)) %>%  # agrupa por aeroporto e faz a media do atraso
  arrange(desc(delay))


flights %>% 
  group_by(origin) %>% 
  summarize(delay = mean(dep_delay, na.rm = TRUE)) %>%  # agrupa por aeroporto e faz a media do atraso
  arrange(desc(delay))


flights %>% 
  filter ( origin == 'EWR' & !is.na(dep_delay)) %>% 
  group_by(month) %>% 
  summarize(delay = mean(dep_delay)) %>%  # agrupa por aeroporto e faz a media do atraso
  arrange(desc(delay))


flights %>% 
  group_by(dest) %>% 
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>%  # agrupa por destino e faz a media do atraso
  arrange(desc(delay))


flights %>% 
  group_by(dest, month) %>% 
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>%  # agrupa por destino e faz a media do atraso
  arrange(desc(delay))

flights %>% 
  group_by(dest, month) %>% 
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>%  # agrupa por destino e faz a media do atraso
  arrange(dest,desc(delay))

flights %>% 
  filter(!is.na(arr_delay) & dest == 'CAE') %>% 
  group_by(dest, month) %>% 
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>%  # agrupa por destino e faz a media do atraso
  arrange(desc(dest),desc(delay))

flights %>% 
  filter(!is.na(arr_delay) & dest == 'CAE') %>% 
  group_by(carrier) %>% 
  summarize(delay = mean(arr_delay))


# *******************************Exercise 15

time_in_min <- function (x) {
  ((x %/% 100)*60 + x %% 100) %% 1440
}

time_in_hr <- function(x) {
  as.integer(paste(as.character(x%/%60), as.character(x%%60), sep=""))
}


flights %>% 
  group_by(day, month, year) %>% 
  mutate(dep_time = time_in_min(dep_time)) %>% 
  summarize ( first = min(dep_time, na.rm = TRUE),
              last = max(dep_time, na.rm = TRUE)) %>% 
  mutate (first = time_in_hr(first),
          last = time_in_hr(last))



flights %>% 
  group_by(month) %>% 
  summarize(n = n()) %>% 
  ungroup()  


flights %>% 
  group_by(month) %>% 
  summarize(n = n())
  

flights %>% group_by(origin, carrier) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  mutate(rank = rank(desc(avg_dep_delay))) %>%  # *********** cria um ranking dos dados e traz a posicao 1 e 2
  filter(rank < 3) %>% 
  arrange(origin, rank)



flights %>% 
  group_by(origin, carrier) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  mutate(rank = rank(desc(avg_dep_delay))) %>%  # *********** cria um ranking dos dados filtrando o maior e o menor lugar no ranking
  filter(rank == 1 | rank == max(rank)) %>% 
  arrange(origin, rank)


flights %>% 
  group_by(origin, carrier) %>% 
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  mutate(rank = rank(desc(avg_dep_delay))) %>%  # *********** cria um ranking dos dados filtrando o maior e o menor lugar no ranking
  filter(rank %in% c(1,2,max(rank))) %>% 
  arrange(origin, rank)


flights %>% 
  group_by(dest) %>% 
  mutate(sum_delay = sum(arr_delay, na.rm = TRUE),
         sum_delay_prop = arr_delay/sum_delay ) %>% 
  select (dest, arr_delay, sum_delay, sum_delay_prop) %>% 
  arrange(dest)
  

flights %>% 
  group_by(dest) %>% 
  summarize(sum_delay = sum(arr_delay, na.rm = TRUE),
         sum_delay_prop = arr_delay/sum_delay ) %>% 
 # select (dest, arr_delay, sum_delay, sum_delay_prop) %>% 
  arrange(dest)


flights %>% 
  group_by(month) %>% 
  summarize(count = n())


flights %>% 
  group_by(origin) %>% 
  summarize(count = n())            

flights %>% 
  group_by(carrier, dest, month) %>% 
  summarize(count = n())

flights %>% 
  group_by(carrier, dest, month) %>% 
  summarize(n = n()) %>% 
  summarize(n = n()) %>%    # ********cada summarize remove um subgroup do group by
  summarize(n = n())


flights %>% 
  filter(!is.na(dep_delay) & !is.na(arr_delay)) %>% 
  count(tailnum) # para contar nao precisa do group by, a funcao count ja agrupa

no_cancel <- flights %>% 
  filter(!is.na(dep_delay) & !is.na(arr_delay))

delays <- no_cancel %>% 
  group_by(tailnum) %>% 
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE), count_num = n())

delays %>% 
  ggplot(aes(x = count_num, y=avg_arr_delay))+
  geom_point(shape=21, alpha = 0.25)

delays %>% 
  filter(count_num >=25) %>% 
  ggplot(aes(x = count_num, y=avg_arr_delay))+
  geom_point(shape=21, alpha = 0.25)

