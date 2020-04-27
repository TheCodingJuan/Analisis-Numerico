require(pracma)
options(digits = 15)

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

  
  x <- Var('x')
  sympy(paste('e = ', f, collapse = '', sep = ''))
  approx <- sympy(paste('e.subs(x, ', as.character(x0), ')', sep = '', collapse = ''))
  
  return(list('Approximation from Interpolation'=as.numeric(approx),'Multiplications'=it))
}

ex <- function(x)
{
  y0 = 0
  it = 1
  while (it <= length(x))
  {
    y0[it] = exp(1)^x[it]
    it = it + 1
  }
  return(y0)
}

xt = c(0.000,0.125,0.250,0.375,0.625,0.750,0.875,1.000)
yt = ex(xt)
bariloco = barylag (xt,yt,c(0.5))
barylag.multip(xt)
divided.differences(xt,yt,0.5)

print(exp(0.5))

errorAbsoluto = abs((exp(0.5)-bariloco)/exp(0.5))

print(bariloco)

print(errorAbsoluto)


teoremaDeTaylor = function(x)
{
  return (exp(0) + exp(0) * x + exp(0)/2 * x^2 + exp(0)/6 * x^3)
}


cat("Este es el resultado de Taylor:", teoremaDeTaylor(0.5),
    "\n")


errorAbsoluto = abs((exp(0.5)-teoremaDeTaylor(0.5))/exp(0.5))


print(errorAbsoluto)

