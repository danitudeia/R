# ***********Exploratory Data Analysis - Part III******

library (tidyverse)

wages <- read_csv("http://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Bwages.csv")

wages %>%
  ggplot(aes(x=exper, y=wage))+
  geom_point(shape =21, alpha = 1/3)+
  geom_smooth(method = 'lm', se = FALSE)+
  facet_grid(~educ)

cor(wages$exper, wages$wage) # ***correlation


wages_clean %>%
  ggplot(aes(x = educ, y = wage))+
  geom_boxplot()


wages_clean %>%
  ggplot(aes(x = educ, y = exper))+
  geom_boxplot()

library(GGally)

ggpairs(wages_clean) # ***Cria varios graficos comparando dados com um so comando


wages_clean %>%
  ggplot((aes(x=exper, y=wage, color = educ)))+
  geom_point(shape =21, alpha = 1/2)+
  geom_smooth(method = 'lm', se = FALSE)


wages_clean %>%
  group_by(educ) %>% 
  summarize(corr = cor(wage, exper))

corr_wage <- wages_clean %>%
  group_by(educ) %>% 
  mutate(corr = cor(wage, exper)) 

corr_wage%>% 
  ggplot((aes(x=exper, y=wage, color = educ)))+
  geom_point(shape =21, alpha = 1/2)+
  geom_smooth(method = 'lm', se = FALSE)+
  facet_grid(~ round(corr,2))


corr_wage%>% 
  ggplot((aes(x=exper, y=wage, color = educ)))+
  geom_point(shape =21, alpha = 1/2)+
  geom_smooth()+
  facet_grid(~ round(corr,2))


wages %>%
  count(educ)

# ***********Exploratory Data Analysis - Part IV******

library(tidyverse)
library(here)

wages <- read_csv("http://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Bwages.csv")

wages <- wages %>% select(-X1, -sex) %>% 
  mutate(educ = factor(educ)) 

wages


?cut

sort(unique(cut(wages$exper, 5)))

sort(unique(cut(wages$exper, c(0,5,10,15,20,25,30,47)))) # ***O primeiro numero da faixa nao esta incluso nela

sort(unique(cut(wages$exper, seq(min(wages$exper)-1, max(wages$exper), 5)))) # ***Do primeiro (-1 pois o primeiro nao esta incluso) ao ultimo valor de exper com 5 numeros de "tamanho"****

sort(unique(cut(wages$exper, seq(min(wages$exper), 
                                 max(wages$exper),
                                 5),
                include.lowest= TRUE))) # ***Do primeiro (nao precisa do -1 pois usamos a funcao include lowest = True) ao ultimo valor de exper com 5 numeros de "tamanho"****


sort(unique(cut_width(wages$exper,10)))

table(cut(wages$exper,5)) # mostra o volume de registros por cada uma das 5 quebras de exper criadas pelo R

wages %>% 
  mutate(exper_bin = cut(wages$exper,5) ) %>% # mostra o volume de registros por cada uma das 5 quebras de exper criadas pelo R
  group_by(exper_bin) %>% 
  summarize(n=n())


table((cut_number(wages$exper,5))) # separa em 5 categorias com o mesmo numero de registros (ou o mais perto disso)


wages %>% 
  ggplot(aes(x=exper, y=wage))+
  geom_point(alpha = 1/3)

wages %>% 
  ggplot(aes(x=cut(exper,5), y=wage))+
  geom_boxplot()

  
wages %>% 
  ggplot(aes(x=cut(exper,5), y=wage))+
  geom_violin()


wages %>% 
  ggplot(aes(x=cut(exper,c(0,5,10,15,20,25,30,47), 
                   include.lowest = TRUE), y=wage))+
  geom_violin()
  
wages %>% 
  ggplot(aes(x=cut(exper,c(0,5,10,15,20,25,30,47), 
                   include.lowest = TRUE), y=wage))+
  geom_boxplot()+
  labs(x='Range years of experience')

# ****exercise 9

wages %>% 
  ggplot(aes(x=cut_width(exper,10), y=wage))+
  geom_boxplot()+
  labs(x='Range experience')

table(cut_width(wages$exper,10))

wages %>% 
  ggplot(aes(x=cut_width(exper,10)))+
  geom_bar( fill='skyblue',
            color = 'black')+
  labs(x='Range experience')

wages %>% 
  ggplot(aes(x=exper))+
  geom_histogram(binwidth = 10, 
                 fill='skyblue',
                 color = 'black')+
  labs(x='Range experience')


# ****exercise 10

wages %>% 
  ggplot(aes(x=cut_number(exper,5), y=wage))+
  geom_boxplot()+
  labs(x='Range experience')

wages %>% 
  ggplot(aes(x=cut_number(exper,5), y=wage))+
  geom_violin()+
  labs(x='Range experience')

# ****exercise 11

wages %>% 
  mutate(exper_cat = cut(exper, 3, labels = c('low', 'mid', 'high'))) %>% 
  ggplot(aes(x=exper_cat, y=wage))+
  geom_boxplot()+
  labs(x='Range experience')

wages %>% 
  mutate(exper_cat = cut(exper, 3, labels = c('low', 'mid', 'high'))) %>% 
  ggplot(aes(x=exper_cat, y=wage))+
  geom_violin()+
  labs(x='Range experience')

# ****exercise 12 and 13

wages %>% 
  mutate(exp_cat = factor(ifelse(wage<17, 'low', 'high'))) %>% 
  ggplot(aes(x=exp_cat, y=exper))+
  geom_boxplot()+
  labs(x='Range experience')

wages %>% 
  mutate(exp_cat = factor(ifelse(wage < 10, 'low', ifelse (wage < 25, 'mid', 'high')))) %>% 
  ggplot(aes(x=exp_cat, y=exper))+
  geom_boxplot()


# ****exercise 14
wages %>% 
  mutate(educ_cat = factor( case_when (
          educ == 1 ~ 'primary',
          educ == 2 ~ 'secondary', 
          educ == 3 ~ 'bachelors',
          educ == 4 ~ ' masters',
          educ == 5 ~ 'doctorate',
          TRUE ~ 'other'
      )
    )
  ) %>% 
  ggplot(aes(x=educ_cat, y=wage))+
  geom_boxplot()

