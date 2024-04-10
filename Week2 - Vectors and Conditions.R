#VECTOR - SO PERMITE UM TIPO DE VARIAVEL

vec1 <- c(100,200,300)
##vec1 <- c(vec1[1],150,vec1[2:3]) # adicionar um novo item no meio do vetor

len_vec1 <- length(vec1)

vec1 <- c(vec1[1:(len_vec1%/%2)],
          150,
          vec1[((len_vec1%/%2)+1):len_vec1]) # adicionar um novo item no meio do vetor

vec2 <- c(1,2,3,4,5)
vec2 <- vec2[-3] #remover um item do vetor
vec2 <- vec2[-c(1,2,5)] #remover varios itens - cria um vetorcom o que quer excluir e exclui do outro

#CONDITION

vec3 <- c(5,10,26,40,52,21)
condition1 <- vec3 >20
condition2 <- vec3 <40
condition3 <- vec3 %%2 == 0 #numero par
vec3[condition1 & condition2 & condition3] # buscar valores entre as duas condicoes (AND)
vec3[condition1 & condition2 | !condition3] # buscar valores entre as duas condicoes (AND + OR)

#Matrix

matrix1 <-matrix(1:9,nrow=3,ncol=3)
matrix1

matrix1[,2] # busca todos os itens da coluna 2
matrix1[2,] #busca todos os itens da linha 2
matrix1[2,3] #busca item especifico

submat <- matrix1[1:2,1:2] #busca os dois primeiros itens das colunas e das linhas
submat

matrix_rowwise <- matrix(1:9, nrow=3, ncol=3, byrow = TRUE) # ordenar os numeros pela linha

#Array

myarray <- array(1:8, dim = c(2,2,2))
myarray

myarray[1,,2]
myarray[,2,1]

#LIST - VARIAVEIS DE TIPOS DIFERENTES

my_list <- list(10.4, "Hello", 4L, TRUE)
typeof(my_list[[2]]) #para trazer infos sobre o item da lista, colocar dois colchetes


#DATA FRAME
mydata <- data.frame(name = c('Ali', 'John','Mary'),
                    GPA = c(4,3.5,3.8),
                    year = c(2020,2021,2022),
                    stringsAsFactors = FALSE)

mydata[,1] #select todas as linhas da coluna 1
mydata['name'] # select todas as linhas da coluna name
mydata$name[2] #select segunda linha na coluna Name


#FACTOR

fav_color <- c("skyblue", "black", "black")
color_fact <- factor(fav_color) # agrupa valores repetidos
color_fact

hand <- c(0,1,1)
hand_fct <- factor(hand,labels = c('righthanded','lefthanded'))
hand_fct

#ADD FEATURES TO DATA FRAME
mydata$hand <- hand_fct


# **********************************************
#BASIC EDA

source("http://www.openintro.org/stat/data/arbuthnot.R")

dim(arbuthnot)
names(arbuthnot)

arbuthnot$boys
arbuthnot['boys']

typeof(arbuthnot)

plot(x =arbuthnot$year, 
     y=arbuthnot$girls, 
     type = 'l')

plot(x =arbuthnot$year, 
     y=arbuthnot$boys, 
     type = 'l')

plot(x =arbuthnot$year, 
     y= arbuthnot$girls/(arbuthnot$boys + arbuthnot$girls),
     type = 'l')

#***********ASSIGNMENT 1***********

source("http://www.openintro.org/stat/data/present.R")


# What are the dimensions of the data frame and what are the variable or column names?

dim(present)  #DIMENSIONS = 63 OBS AND 3 FEATURES

names(present)   #FEATURES = YEAR, BOYS AND GIRLS

# What years are included in this data set? 

present_fact <- present$year
present_factor <- factor(present_fact)
present_factor  #YEARS FROM 1940 TO 2002

length(unique(present$year)) #YEARS FROM 1940 TO 2002


# How do these counts compare to Arbuthnot’s? Are they on a similar scale?
present['boys'] 
present['girls'] #These counts are higher then Arbuthnot's

# Make a plot that displays the boy-to-girl ratio for every year in the data set. 
# What do you see? Does Arbuthnot’s observation about boys being born in greater proportion than girls hold up in the U.S.? 
# Include the plot in your response.
plot(x = present$year,
     y = present$boys / (present$boys + present$girls),
     type = 'l')


plot(x = present$year,
     y = present$girls / (present$boys + present$girls),
     type = 'l')

# In what year did we see the most total number of births in the U.S.?

plot(x = present$year,
     y = present$boys + present$girls,
     type = 'l')

present$total_born <- present$boys + present$girls

max(present$total_born)

condition <- present$total_born == max(present$total_born)
condition

present$year [condition] # 1961