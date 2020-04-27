library(pracma)
#puntos dados (0,10) (1,15) (2,5)
x <- c(0,1,2)
y <- c(10,15,5)

#Secuencia 
xi<- seq(0, 2, by=1)

#Se genera la funcion 
fun <- cubicspline(x,y,xi=NULL,endp2nd = FALSE, der = c(1,1))
ffun <- funtion(xi) ppval(fun,xi)
print(fun)

#Se grafica
plot(x,y,type = 'b')
