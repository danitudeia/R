# ******Data Wrangling - Part I

library(tidyverse)
library(here)

tibble(x = 1:2, y = c("a", "b"))

tibble(x = letters)

data.frame(x = letters)

df1 <-data.frame(x = letters, stringsAsFactors=FALSE)

df1 <-data.frame(x = letters, stringsAsFactors=TRUE)

names(data.frame('crazy name' = 1))

names(tibble('crazy name' = 1))

tibble(x = 1:5, y = x ^ 2) # **series are created as integer number in R

# to create the data frame we need to create the variable x first, in tibble no
x <- 1:5
data.frame(x = 1:5, y = x ^ 2)


data.frame(x=x, y=x^2)

tibble(x = 1:6, y="a")

tibble(x = 1:6, y = c('a','b'))

tibble(x = 1:6, y = c('a','b'))

df2 <- data.frame(x = 1:9, y = c('a','b','c'))

x

df2$x

df2$y

y

tibble(x = 1:1000)

df1 <- data.frame(x = 1:3, y = 3:1)
class(df1[, 1:2])
class(df1[, 1]) # data type of subset of tibbles are always data frame but subsets of data frames are the type of feature (case you get one feature)


df2 <- tibble(x = 1:3, y = 3:1)
class(df2[, 1:2])
class(df2[, 1])


df <- data.frame(abc = 1)
df$a
df$ab
df$abc


df <- data.frame(abc = 1, a= 3:5)
df$a
df$ab
df$abc

df <- data.frame(abc = 1, abd= 3:5)
df$a   # ****pode colocar parte do nome da coluna pra exibir, exceto se nao houver outro com nome semelhante
df$ab
df$abc

df2 <- tibble(abc = 1)
df2$a # *** precisa colocar o nome inteiro da coluna pra exibir, se nao da erro

as.data.frame(as_tibble(iris))

# *****Data Import

wages <- read_csv(here('data', 'bwages.csv')) 

read_csv("The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3", skip = 2) # pula o numero de linhas descritas no "Skip" antes de importar o arquivo

read_csv("x,y,z
# A comment I want to skip
  1,2,3", comment = "#") # desconsidera as linhas com # pois estao marcadas como comentarios


read_csv("The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3", skip = 2,
         comment = "#") # pula o numero de linhas descritas no "Skip" e das com # antes de importar o arquivo


read_csv("The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3", skip = 2,
         comment = "#",
         col_names = FALSE) # pula o numero de linhas descritas no "Skip" e das com # e considera que nao ha nome nas colunas do arquivo antes de importar o arquivo

examp.df <-read_csv("The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3", skip = 2,
         comment = "#",
         col_names = c('Apple','Bell','Color')
         ) # pula o numero de linhas descritas no "Skip" e das com # e considera que nao ha nome nas colunas do arquivo mas permite nomear as colunas antes de importar o arquivo
examp.df


examp.df <- read_csv("a, b, c \n 1, 2, -", na = "-")
examp.df


challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(                 # ****mudando o tipo da coluna
    x = col_double(),
    y = col_character()
  )
)

challenge


challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(                 # ****mudando o tipo da coluna
    x = col_double(),
    .default = col_integer()  )
)

challenge

write_csv(challenge, "challenge.cvs") 
read_rds("challenge.csv") # ***using CSV we lost the data types we've defined

write_rds(challenge, "challenge.rds") # ***using RDS we save the data in R format and keeps the data types
read_rds("challenge.rds")

# ----------------Tidy Data
library(tidyverse)

table1

table2

table3

table4a

table4b

table5


#------------

table2_cases <- table2 %>% 
  filter(type=='cases') %>% 
  arrange(country,year) %>% 
  rename(count_cases = count)

table2_pop <- table2 %>% 
  filter(type=='population') %>% 
  arrange(country,year) %>% 
  rename(count_pop = count) %>% 
  select(count_pop)
  
table2_rate <- as_tibble(table2_cases$count / table2_pop$count *10000)

table2_cases %>% 
  cbind(table2_pop) %>% 
  mutate(rate = count_cases / count_pop * 10000 ) %>% 
  select(~type)

#---exercise4
table1 %>% 
  ggplot(aes(x= year, y = cases, color = country))+
  geom_line()+
  geom_point()

#-----exercise5
table2 %>% 
  filter(type=='cases') %>% 
  arrange(country,year) %>% 
  rename(count_cases = count) %>% 
  ggplot(aes(x= year, y = count_cases, color = country))+
  geom_line()+
  geom_point()


#-----exercise6
table4a %>% 
  gather(key = "year", value = "cases", c(`1999`, `2000`))

#-----exercise7
table4b %>% 
  gather(key = "year", value = "population", c(`1999`, `2000`))

table4b %>% 
  gather(key = "year", value = "population", 2:3)


#-----exercise8

table2 %>% 
  spread(key=type,value=count)


#-----exercise9
?separate

table3 %>% 
  separate(rate, c('cases','population'), sep='/') # serve para separar os dados da coluna em mais colunas a partir de um caracter 


#-----exercise10
table3_split <- table3 %>% 
  separate(year, c('century','year'), sep=2) # serve para separar os dados da coluna em mais colunas a partir de uma qtde de caracteres



#-----exercise11
table3_split %>% 
  unite(year, c('century','year'),sep='')

#-----challenge
table5 %>% 
  separate(rate, c('cases','population'), sep='/') %>% 
  unite(year, c('century','year'),sep='') %>% 
  mutate(cases = as.integer(cases),
         population = as.integer(population),
         rate = as.integer(cases) / as.integer(population) * 10000)


  
