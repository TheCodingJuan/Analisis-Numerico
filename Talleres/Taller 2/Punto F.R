require(PolynomF)
require(pracma)


ft <- function(x)
{
  y0 = c()
  it = 1
  while (it <= length(x))
  {
    y0[it] = tan(x[it])
    #cat(x[it],"\n")
    #cat(y0[it],"\n")
    it = it + 1
  }
  return(y0)
}

x = seq(-pi/2, pi/2, length=10)
y = ft(x)

polyAjuste = poly.calc(x,y)
print(polyAjuste)

y2 = c()
it = 1

while (it <= length(x)){
  y2[it] = polyAjuste(x[it])
  #cat(y2[it],"\n")
  it = it +1
}

#print(y)
#print(y2)

x_tan = seq(-pi/2,pi/2,length=100)
y_tan = tan(x_tan)

plot(x_tan,y_tan,type="l",xlim=c(-2,2),ylim=c(-10,10))
par(new=TRUE)

errores = c()
it2 = 1
while (it2 <= length(y)){
  errores[it2] = abs(abs(y2[it2]-y[it2])/y[it2]) *100
  #cat(errores[it2],"\n")
  it2 = it2 +1
}
plot(x[2:9],y[2:9], pch = 19, cex=1.5, col= "blue",xlim=c(-2,2),ylim=c(-10,10))
par(new=TRUE)
curve(polyAjuste,add=T,lty=3, lwd=2,xlim=c(-2,2),ylim=c(-10,10))
par(new=TRUE)
plot(x[2:9],y2[2:9], pch = 19, cex=1, col= "red",xlim=c(-2,2),ylim=c(-10,10))