require(pracma)

barylag.multip <- function(x)
{
  n = length(x)
  return((n+1)^2)
}

divided.differences <- function(x, y, x0) {
  require(rSymPy)
  n <- length(x)
  q <- matrix(data = 0, n, n)
  q[,1] <- y
  f <- as.character(round(q[1,1], 5))
  fi <- ''
  it = 0
  for (i in 2:n) {
    for (j in i:n) {
      q[j,i] <- (q[j,i-1] - q[j-1,i-1]) / (x[j] - x[j-i+1])
      it = it + 1
    }
    fi <- paste(fi, '*(x - ', x[i-1], ')', sep = '', collapse = '')
    
    f <- paste(f, ' + ', round(q[i,i], 5), fi, sep = '', collapse = '')
  }
  
  cat("Polinomio Generado: ", f, "\n")
  
  x <- Var('x')
  sympy(paste('e = ', f, collapse = '', sep = ''))
  approx <- sympy(paste('e.subs(x, ', as.character(x0), ')', sep = '', collapse = ''))
  
  return(list('Approximation from Interpolation'=as.numeric(approx),'Multiplications'=it))
}

x0 = c(200,300,500)
y0 = c(-35,-4.2,16.9)
barylag (x0,y0,c(450))
barylag.multip(x0)
divided.differences(x0,y0,450)

f0 = function(x)
{
  return(-35 + 0.308*(x - 200) - 0.00068*(x - 200)*(x - 300))
}

#plot(f0, xlim = c(100,600),ylim = c(-200,50), xlab = "Temperaturas", ylab = "Segundo Coecificente virial")

x1 = c(200,300,400,500)
y1 = c(-35,-4.2,9,16.9)
barylag (x1,y1,c(450))
barylag.multip(x1)
divided.differences(x1,y1,450)

f1 = function(x)
{
  return(-35 + 0.308*(x - 200) + -0.00088*(x - 200)*(x - 300) + 0*(x - 200)*(x - 300)*(x - 400))
}

#par( new = TRUE) 
#plot(f1, xlim = c(100,600), col = 2, ylim = c(-200,50), xlab = "Temperaturas", ylab = "Segundo Coecificente virial")

x2 = c(100,200,300,500)
y2 = c(-160,-35,-4.2,6.9)
barylag (x2,y2,c(450))
barylag.multip(x2)
divided.differences(x2,y2,450)

f2 = function(x)
{
  return(-160 + 1.25*(x - 100) + -0.00471*(x - 100)*(x - 200) + 1e-05*(x - 100)*(x - 200)*(x - 300))
}

#par( new = TRUE) 
#plot(f2, xlim = c(100,600), col = 3, ylim = c(-200,50), xlab = "Temperaturas", ylab = "Segundo Coecificente virial")


x3 = c(200,300,500,600)
y3 = c(-35,-4.2,16.9,21.3)
barylag (x3,y3,c(450))
barylag.multip(x3)
divided.differences(x3,y3,450)

f3 = function(x)
{
  return( -35 + 0.308*(x - 200) + -0.00068*(x - 200)*(x - 300) + 0*(x - 200)*(x - 300)*(x - 500) )
}

#par( new = TRUE) 
#plot(f3, xlim = c(100,600), col = 4, ylim = c(-200,50), xlab = "Temperaturas", ylab = "Segundo Coecificente virial")


x4 = c(100,200,300,400,500,600)
y4 = c(-160,-35,-4.2,9,16.9,21.3)
barylag (x4,y4,c(450))
barylag.multip(x4)
divided.differences(x4,y4,450)

f4 = function(x)
{
  return(-160 + 1.25*(x - 100) + -0.00471*(x - 100)*(x - 200) + 1e-05*(x - 100)*(x - 200)*(x - 300) + 0*(x - 100)*(x - 200)*(x - 300)*(x - 400) + 0*(x - 100)*(x - 200)*(x - 300)*(x - 400)*(x - 500))
}



par( new = TRUE) 
plot(f4, xlim = c(100,600),type = 'l', col = 1, ylim = c(-200,50), xlab = "Temperaturas", ylab = "Segundo Coecificente virial")
points( c(100,200,300,400,500,600),c(-160,-35,-4.2,9,16.9,21.3))



