#random vector with n = 20
x <-  runif(20,-5,10)

# vector and vector with numbers between (-inf,0)
y <- c(0,25,-8,-20,50,1,15,-2,-44:-41) 
y2 <- y[y<0]

# rainbow with colors, red and purple colors
name <- c('red', 'orange','yellow', 'green', 'blue', 'lightblue','purple')
col <- name[c(1,7)]

# logical vector where 3x repeat TRUE, 3x repeat FALSE then 4x repeat TRUE
new <- c(rep(TRUE, 3), rep(FALSE, 3), rep(TRUE, 4))

# 
v <- c(2,4,NA,8,1,23:25)
b <- v+3                
d <- v*3
length(v)
class(v)
sum(v, na.rm = TRUE)
mean(v, na.rm =T)

#how to counts NA 
is.na(v)

#how count not NA
!is.na(v)
