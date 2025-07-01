# заполнение по строкам, параметр ncol указывать не обязательно
mm <- matrix(1:9, nrow = 3, ncol = 3, byrow = T)

# хотим вывести матрицу, выделяем ее и в консоле она отобразится

#заполнение по столбцам
mm_col <- matrix(1:9, nrow = 3, ncol = 3, byrow = F)

# с нулевой матрицей обязательно указывать количество и столбцов, и строк
mm_na <- matrix(0, nrow = 3, ncol = 3, byrow = F)
fix(mm_na)
# присвоение элементу матрицы A(i,j) значения
mm[1,3] <- -3

#дать название строкам и столбцам матрицы
rownames(mm) <- c('F1', 'F2', 'F3')
colnames(mm) <- c('B1','B2','B3')

# количество строк и столбцов
n_rows <- nrow(mm)
n_cols <- ncol(mm)

# размерность матрицы 
dim <- dim(mm)

# определитель матрицы
determinant <- det(mm)

# фрейм данных 
aa <- letters[1:3] #latin (if LATIN)
mm <- matrix(1:9, nrow = 3)
ff <-  data.frame(aa,mm)

# если открыть дата фрейм ff, увидем значок доллара (по этим столбцам буду обращаться)
x2 <- ff$X2


# фактор - обозначение категориальных переменных (что бывают порядковые и номинальные) в R, 
# функция фактор сохраняет категориальные данные в виде вектора целых чиселв в диапазоне [1,..k], k - число уникальных значений категориальной переменной
# и в виде внутреннего вектора из цепочки символов, соотвествующим этим целым числам = k1, k2 и тд

gender <- c(rep('F',3), rep('M',3))
gg <- factor(gender)

#gg[1]


# как открыть файлы: ctrl + shift + H; 
# session -> set working dir -> choose dir; 
# или найти в папке аутпута свои данные; 
# внешний способ - открыть в проводнике свою папку и скопировать адрес и прописываю setwd('c://')
# getwd()

aa <- read.delim('C:/Users/Admin/Desktop/Мат статистика/R_files/Response2Drug.txt')
aa$Gender <- factor(aa$Gender,  order = TRUE, 
                    levels = c('M', 'F'), 
                    labels = c(1, 2)) #перезагружать данные надо, он переобновляет level и labels, сами настраиваем порядок

#внутренние данные в R
#data("mtcars")

mpg_sum <- summary(mtcars$mpg)
plot(mtcars$mpg, mtcars$disp)
plot(mtcars$mpg, mtcars$wt)

# вывести все сразу 
with(mtcars, {
  stat <<- summary(mpg)
  plot(mpg, disp)
  plot(mpg, wt)
})

# список содержит гетерогенные элементы 
xx <- letters[1:5]
ff <- matrix(1:9, ncol = 3)
df <- data.frame(aa = letters[1:10], bb = 1:10, cc = seq(-9,0,by = 1))
ll <- list(letter = xx, data_frame = df, matrix_elem = ff)

# обращение к элементу списка

# ll[1] #вывело letters

# ll[[1]][1] #из letters уже вывело первую букву
# ll$matrix_elem[2,3]

# допустим есть данные в формате *.csv 
# чтобы открыть его, можно использовать read.table(' ', sep =',') или read.delim()
# tsv sep = '\t', csv sep = ',', default sep = ' ' 

# а если я захочу сохранить файл? обязательно прописать путь для сохранения

write.table(df, 'data.txt', sep = '\t')


matrica <- matrix(0, ncol = 3, nrow = 3, byrow = T)
matrica <-  fix(matrica)

f <- det(matrica)
b <- c(7,10,12)
m1 <- m2 <- m3 <- matrica
m1[,1] <- b
m2[,2] <- b
m3[,3] <- b