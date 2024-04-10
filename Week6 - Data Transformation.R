# ***DATA TRANSFORMATION****

library(tidyverse)
library(gapminder)

pop <- gapminder$pop  # cria um vetor com os valores de pop
pop_dataframe <- gapminder['pop'] # cria um dataset com a coluna pop (tabela)

class(pop)
class(pop_dataframe)

subset_pop <- gapminder[100,'pop'] # seleciona um registro
subset_pop <- gapminder[1:10,'pop'] # seleciona o intervalo
subset_pop <- gapminder[c(1, 10, 15, 200, 300), 'pop'] # seleciona apenas as linhas descritas

subset_country <- gapminder[10000, 'country']
subset_country

class(subset_pop)

subset_life <- gapminder[172,'lifeExp'] # seleciona um registro

gapminder[1:5,c('year','pop')]

gapminder[c(1,12,38),c('country','year','lifeExp')]

# ***************

select (gapminder, pop, year)

gapminder %>% select(pop) # ctlr+shift+m

gapminder %>% select(continent, year, pop) # ctlr+shift+m

gapminder %>% select(-pop) # todas as colunas menos pop

gapminder %>% select(-(country:continent)) # todas as colunas exceto as entre country e continent


select(gapminder, gdpPercap, everything())

select(gapminder, starts_with('c'))

select(gapminder, ends_with('p'))

select(gapminder, contains('Exp'))

select(gapminder, contains('exp'))



gapminder %>% select(year, everything())

gapminder %>% select(starts_with('c'))

gapminder %>% select(ends_with('p'))

gapminder %>% select(contains('Exp'))



filter(gapminder, continent == 'Europe')

filter(gapminder, year == '1967')

filter(gapminder, lifeExp >= 80)


gapminder %>% filter(continent == 'Europe')

gapminder %>% filter(year == 1967)

gapminder %>% filter(lifeExp >= 80)

gapminder %>% filter(year > 1967 & gdpPercap <4000)

gapminder %>% filter(pop < 500000 | pop > 1000000000)

gapminder %>% filter(continent == 'Europe' & lifeExp >=78 & pop < 1000000 | (pop > 1200000000))

canada <- gapminder %>% filter(country == 'Canada')
view(canada)

write_csv(canada,"canada.csv")

canada <- read_csv("canada.csv")

country_name <- 'Canada'

gapminder %>% 
  filter(country == country_name) %>% 
  select (-continent) %>%
  ggplot(aes(x=year, y=pop))+
  geom_point(aes(size = gdppercap))+
  geom_line()+
  labs(title = country_name)
