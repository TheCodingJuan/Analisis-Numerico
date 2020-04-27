library (pracma)
x <- c(1,2)
y <- c(2,6)
plot(x,y, type='p')

f <- function(x)  x^3-3*x^2+6*x-2

plot(f,xlim = c(1,2),ylim =c(0,6),type='l',add=TRUE)