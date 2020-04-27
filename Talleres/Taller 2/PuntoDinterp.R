library(pracma)
library(rSymPy)
f <- function(x){
  log(x)
}
divdiff <- function(x, y, x0) {
    n <- length(x)
  q <- matrix(data = 0, n, n)
  q[,1] <- y
  f <- as.character(round(q[1,1], 5))
  fi <- ''
  
  for (i in 2:n) {
    for (j in i:n) {
      q[j,i] <- (q[j,i-1] - q[j-1,i-1]) / (x[j] - x[j-i+1])
    }
    fi <- paste(fi, '*(x - ', x[i-1], ')', sep = '', collapse = '')
    
    f <- paste(f, ' + ', round(q[i,i], 5), fi, sep = '', collapse = '')
  }
  
  x <- Var('x')
  sympy(paste('e = ', f, collapse = '', sep = ''))
  approx <- sympy(paste('e.subs(x, ', as.character(x0), ')', sep = '', collapse = ''))
  
  return(list('Aproximacion de interpolacion'=as.numeric(approx), 
              'Funcion interpolada'=f, 
              'Resultados dd tabla'=q))
}
xs = seq(1,2)
y = f(xs)
resultados <- divdiff(xs,y,1)
resultados2 <- divdiff(xs,y,2)
cat("Ln(x) en x = 1 \n")
print(resultados$`Aproximacion de interpolacion`)
print(resultados$`Funcion interpolada`)
print(resultados$`Resultados dd tabla`)
cat("Ln(x) en x = 2 \n")
print(resultados2$`Aproximacion de interpolacion`)
print(resultados2$`Funcion interpolada`)
print(resultados2$`Resultados dd tabla`)