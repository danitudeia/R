# *******Data Wrangling - Part I - MISSING VALUE******

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks


stocks %>% 
  complete(year,qtr) # *********show the hiding missing values and fill/create a row with it

stocks %>% 
  spread(year, return)

df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
df

df %>% drop_na()

df %>% drop_na(x)

# tribble is used for row-wise tibble creation
treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)

treatment


treatment %>% 
  fill(person)

treatment %>% 
  fill(person, .direction = "up")


df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
df


df %>% replace_na(replace = list(x = 0, y = "unknown"))

df %>% mutate(x = replace_na(x, 0))

df %>% mutate(x = replace_na(x, max(x, na.rm = TRUE)))

# ***************************


titanic = read_csv('C:/Users/Danielle/Downloads/titanic(2).csv')

summary(titanic)

sum(is.na(titanic$Age))/length(titanic$Age) *100 # ***share of NAs in Age

mean(is.na(titanic$Age)) # ***another way to get the share of NAs in Age

titanic %>% 
  mutate(imputed_age = replace_na(Age, mean(Age, na.rm = TRUE))) %>% 
  select(Age, imputed_age)

titanic %>% 
  mutate(imputed_age = replace_na(Age, median(Age, na.rm = TRUE))) %>% 
  select(Age, imputed_age) %>% 
  ggplot() +
  geom_histogram(aes(x=Age),
                 color='black',
                 fill='pink',
                 binwidth = 5,
                 alpha = 0.5)+
  geom_histogram(aes(x=imputed_age),
                 color='black',
                 fill='blue',
                 binwidth = 5,
                 alpha = 0.1)

titanic %>% 
  mutate(imputed_age = replace_na(Age, mean(Age, na.rm = TRUE))) %>% 
  select(Age, imputed_age) %>% 
  ggplot() +
  geom_histogram(aes(x=Age),
                 color='black',
                 fill='pink',
                 binwidth = 5,
                 alpha = 0.5)+
  geom_histogram(aes(x=imputed_age),
                 color='black',
                 fill='blue',
                 binwidth = 5,
                 alpha = 0.1)

titanic %>% 
  group_by(Sex, Pclass) %>% 
  select(Age, Sex, Pclass) %>% 
  arrange(Sex,Pclass) %>% 
  mutate(imputed_age = replace_na(Age, median(Age, na.rm = TRUE))) %>% # ******substituindo os NAs pelas medias do grupo sex+classe da cabine
  ggplot() +
  geom_histogram(aes(x=Age),
                 color='black',
                 fill='pink',
                 binwidth = 4,
                 alpha = 0.5)+
  geom_histogram(aes(x=imputed_age),
                 color='black',
                 fill='blue',
                 binwidth = 4,
                 alpha = 0.1)



