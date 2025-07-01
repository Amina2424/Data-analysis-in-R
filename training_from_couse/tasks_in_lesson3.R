# task 1. two vectors and matrices

v1 <- runif(25)
v2 <- runif(25)
m1 <- matrix(v1, ncol = 5)
m2 <- matrix(v2, ncol = 5)

# task 2. mean and summ of elem in cols

mean1 <- colMeans(m1)
mean2 <- colMeans(m2)

sum1 <- colSums(m1)
sum2 <- colSums(m2)

# task 3. Determination

deter1 <- det(m1)
deter2 <- det(m2)

# task 4. + - * / 

plus <- m1 + m2
minus <- m1 - m2 
delet <-  m1/m2
umn <-  m1 %*% m2

# task 5. names rows and cols
rownames(m1) <- c('F1', 'F2', 'F3', 'F4', 'F5')
colnames(m1) <- c('B1','B2','B3', 'B4', 'B5')
rownames(m2) <- letters[1:5]
colnames(m2) <- letters[6:10]

# task 6. matrix new

new_matrix <- m1[1:3,1:2]
col_mat <- new_matrix[,2]

row_mat <- m1[4,]

# task 3. 
age <- c(25,35,75)
gender <- c('F', 'M', 'F')
disease <- c(TRUE, FALSE, TRUE)
my_table <-  data.frame(age = age, gender = gender, disease = disease)
my_table$gender <- as.factor(my_table$gender)
write.table(my_table, 'dataframe.txt', sep = '\t')


# task about Kramer

matrica <- matrix(0, ncol = 3, nrow = 3, byrow = T)
fix(matrica)

b <- c(7,10,12)
f <- det(matrica)


m1 <- m2 <- m3 <- matrica
m1[,1] <- b
m2[,2] <- b
m3[,3] <- b

x1 <- det(m1)/det(matrica)
x2 <- det(m2)/det(matrica)
x3 <- det(m3)/det(matrica)

solve(matrica,b)
