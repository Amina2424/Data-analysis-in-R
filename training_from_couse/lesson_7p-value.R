# считывание данных 
aa <- read.delim('Response2drug2.txt')
data1 <- xtabs(~ Mutation + Response, data = aa)

# таблица частот: 1 по строкам, 100 - расчет не долях а процентах
prob.tab <- prop.table(data1,1)*100 

# добавление маргинальных значений (на самый добавление столбца суммы в процентах)
margo <-  addmargins(prob.tab,2) # 2 потому что считали по столбцам ранее, суммирование в этой строке идет по столбцам

# учитывание пола
data <- table(aa$Mutation, aa$Response, aa$Gender) # по последнему столбцу происходит разбиение
prob.tab <- prop.table(data, c(1,2)) # указание столбцов (1,2) из выше заданного data

# тест пирсона для определения связи между переменными
chisq.test(data1, correct = FALSE, p = T, rescale.p = T)

# тест фишера
fisher.test(data1, conf.int = T, conf.level = 0.90)
 
# тест мак-немара
mcnemar.test(data1)

# тест 
mantelhaen.test(data)
mosaicplot( ~Mutation + Response + Gender, data = aa, col = c('pink', 'blue'))

# package for comparasion groups in data (any type)
install.packages("compareGroups")
library(compareGroups)
group <- factor(rep(c('c', 'p'), each = 50))
response <- factor(sample(c('plus', 'nothing', 'minus'), 100, replace = T))
age <- rnorm(100, mean = c(45,50), sd = c(10,15))
df <- data.frame(Group = group, Age = age, Response = response)
descrTable(~Response+Age+Group, data = df, 
            Q1 = 0, 
           Q3 = 1)
descrTable(Group~Response+Age, data = df, 
           Q1 = 0, 
           Q3 = 1)