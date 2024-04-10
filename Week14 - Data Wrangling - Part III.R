# ********DATA WRANGLING - PART III ******
library(tidyverse)

my_string <-"'This is a test'"

writeLines(my_string)

my_vec <- c("'This","is","a","test'")
my_vec[1]

my_string[1]
my_string[2]


# ****JOIN STRINGS

str_c('This',"is","a","joined", 'string', sep=' ') # ***join the strings

str_c('This',"is","a",NA,"joined", 'string', sep=' ')

# ***a melhor opcao e sempre tirar os NAs no import da base

read_csv("test.csv", na="#")

# *****outro uso do str_c

str_c('file_', 1:10, '.csv') # ****join 10 files csv


length(my_string)

str_length(my_string) # ***number of characters, including spaces

str_length(NA)

str_length(factor('abc'))

str_length(c("'This","is","a",NA))


# *** SUBSTRING
  
my_string <-"StClairCollege"

str_sub(my_string, start=1, end=7) # ***traz "StClair"

str_sub(my_string, start=-7, end=-1) # ****traz "College"

str_sub(my_string, start=1, end=-8) # ****deixa tudo menos "College"

str_sub(my_string, start=-7, end=-1) <-"" # *****substitui "College" por vazio


spaces <- c(' Left', 'Right ',' both ')
spaces

str_trim(spaces,side='left')
str_trim(spaces,side='right')
str_trim(spaces,side='both')


str_length(str_trim(spaces))
str_length(str_trim(spaces,side='both'))

# ***PARSING

str_pad('hello', length =10, side = 'left', pad='') #*** inclui espacos a esquerda ate que o campo tenha 10 caracteres
str_pad('hello', length =10, side = 'right', pad='') 
str_pad('hello', length =10, side = 'both', pad='') 

# ***Case sensitive adjust

str_to_lower('HELLO') # **** tudo minuscula
str_to_upper('hello') # **** tudo maiuscula
str_to_title('hello') # ***primeira maiuscula e demais minusculas


# ****SPLIT

my_string <- str_split('This sentence needs to be split up', pattern=' ')

# ***como identificar lista no console? - vem com essa linha [[1]]

my_string


# ***criando um vetor - usando colchetes duplos ou unlist

my_string[[1]]

unlist(my_string)

# *********PATTERN*******

str_split("This sentence needs to be split up", pattern =' ')

this_book <- 'This book os mine'

str_view_all(this_book,'i')

str_view_all(this_book,'.i') #****selects the i and the character before
str_view_all(this_book,'.i.') #****selects the i and the character before and after
str_view_all(this_book,'..i') #****selects the i and 2 characters before


this_book <- 'This book os mine. '
str_view_all(this_book,'\\.') #****selects the special character dot in this case


new_book <- "Did you buy 1 or 2 books?"

str_view_all(new_book, '\\?')#****selects the special character question mark in this case

str_view_all(new_book, '[1-2]')#****selects digits, in this case 1 and 2

str_view_all(new_book, '[0-9]')#****selects all digits

str_view_all(new_book, '\\d')#****selects all digits

str_view_all(new_book, '[a-z]')#****selects all letters (minuscula)

str_view_all(new_book, '[A-Z]')#****selects all letters (maiuscula)

str_view_all(new_book, '[A-z]')#****selects all letters (todas)

str_view_all(new_book, '[a-z][a-z]')#****selects pair of letters

str_view_all(new_book, '[a-z]{2}')#****selects the number of letters you want


his_book <-'Does that book belong to Dr.Samet?'

str_view_all(his_book, '[A-Z]')#****selects all letters (maiuscula)

str_view_all(his_book, '^[A-Z]')#****selects all letters (maiuscula)


color_book <- 'Do you use "grey" or "gray"?'

str_view_all(color_book, 'gr.y') #****selects words that has "gr" then anything, then "y"

str_view_all(color_book, 'gr(e|a)y') #****selects words that has "gr" then "e" or "a", then "y"

phone_number <- 'Is your phone number is (519) 110-2100?'

str_view_all(phone_number,'\\(\\d{3}\\) (\\d{3})\\-(\\d{4})') #****selects the phone number
str_view_all(phone_number,'\\(\\d{3}\\)\\s{1}(\\d{3})\\-(\\d{4})') #****selects the phone number - defining the number of spaces

str_replace(phone_number,pattern='\\(\\d{3}\\)\\s{1}(\\d{3})\\-(\\d{4})', '(***) ***-****') #****replace the phone number for what ever you want

# -----------------------
library(lubridate)

class(today())
now()

ymd('2023-06-30')
class(ymd('2023-06-30'))

dmy("30 June, 2023")

mdy("June 30, 2023") + weeks(10)


make_date(2023,06,30)

make_datetime(2023,06,30,12,00,50)

today()-weeks(1)

# ******PRACTICE*****

long_jump <- read_csv('C:/Users/Danielle/Downloads/long_jump(2).csv', show_col_types = FALSE)

long_jump


# ****Exercise 4 to 9
long_jump %>% 
  rowwise() %>%  # ******se nao colocar, ele repete o primeiro item da lista para todos os registros
  mutate(Distance = as.double(str_sub(Mark, start=1, end=4)),
         First_name = unlist(str_split(Athlete,'\\s{1}'))[1], 
         Last_name = unlist(str_split(Athlete,'\\s{1}'))[2],
         Country_code = str_sub(Athlete,-4,-2), 
         Country_name = str_trim(unlist(str_split(Venue,'\\,'))[2],'left'),
         City = unlist(str_split(Venue,'\\,\\s{1}'))[1], 
         Day = unlist(str_split(Date,'\\s{1}'))[1],
         Month = unlist(str_split(Date,'\\s{1}'))[2],
         Year = str_sub(Date,-7,-4)) %>% 
  select(Distance, First_name, Last_name, Country_code, City, Country_name, Day, Month, Year) 


long_jump %>% 
  rowwise() %>%  # ******se nao colocar, ele repete o primeiro item da lista para todos os registros
  mutate(Distance = as.double(str_sub(Mark, start=1, end=4)),
         First_name = unlist(str_split(Athlete,'\\s{1}'))[1], 
         Last_name = unlist(str_split(Athlete,'\\s{1}'))[2],
         Country_code = str_sub(Athlete,-4,-2), 
         Country_name = str_trim(unlist(str_split(Venue,'\\,'))[2],'left'),
         City = unlist(str_split(Venue,'\\,\\s{1}'))[1], 
         Day = unlist(str_split(Date,'\\s{1}'))[1],
         Month = unlist(str_split(Date,'\\s{1}'))[2],
         Year = str_sub(Date,-7,-4)) %>% 
  select(Distance, First_name, Last_name, Country_code, City, Country_name, Day, Month, Year) 

# ****primeiro jeito de fazer exercicio 9

long_jump %>% 
  rowwise() %>%  # ******se nao colocar, ele repete o primeiro item da lista para todos os registros
  mutate(Date = dmy(unlist(str_split(Date,'\\['))[1])) %>% 
  select(Date) 

# ****segundo jeito de fazer exercicio 9
long_jump %>% 
  rowwise() %>%  # ******se nao colocar, ele repete o primeiro item da lista para todos os registros
  mutate(Day = unlist(str_split(Date,'\\s{1}'))[1],
         Month = unlist(str_split(Date,'\\s{1}'))[2],
         Year = str_sub(Date,-7,-4),
         Date = dmy(str_c(Day, Month, Year))) %>% 
  mutate(date_new = make_date(Year, match(Month, month.name), Day)) %>% 
  select(Date, Day, Month, Year,date_new) 
  

