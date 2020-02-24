# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Juan Jose Camacho   
# * Gabriela Maria Camacho
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 2001
# -------------------------------------------------------------------------------------


#Numeros de Chebyshev
library(PolynomF)

numerosChebyShev = function(intervaloA,intervaloB,listaNumeros)
{
  numerosCalculados= c()
  
  for (i in listaNumeros) 
  {
    numerosCalculados= c(numerosCalculados, 1/2 * (intervaloA + intervaloB)
                         + 1/2 * (intervaloB-intervaloA) 
                         * cos((2*i-1) / (2 * length(listaNumeros))* pi) )
  }
  return (numerosCalculados)
}


n = 3

E = 10e-5

listaCalculada= numerosChebyShev(pi/64,-pi/64,seq(1,n+2))

fxCheby = sin(listaCalculada)
ns=1
while (ns = ns - 1) 
{
  matrizDeEcuaciones = matrix(nrow = n+2,ncol = n+2)
  
  for (i in 1:(n+2)) 
  {
    matrizDeEcuaciones[i,1] = 1
    
    for (j in 2:(n+1)) 
    {
      matrizDeEcuaciones[i,j] = listaCalculada[i]^(j-1)
    }
    if(i%%2 == 0)
    {
      matrizDeEcuaciones[i,n+2] = -E
    }
    else
    {
      matrizDeEcuaciones[i,n+2] = E
    }
  }
  
  print(matrizDeEcuaciones)
  
  minimax =  solve(matrizDeEcuaciones,fxCheby)
  
  print(minimax)
  
  
  polinomioFinal = polynom(a=minimax[1:(length(minimax)-1)])
  
  print(polinomioFinal)
  
  salto = (pi/64-(-pi/64))/10
  inicio = -pi/64
  errorRelativo = c()
  errorAbsoluto = c()
  for (i in 1:10) {
    
    errorRelativo = c(errorRelativo,abs((sin(inicio)-polinomioFinal(inicio))/sin(inicio)))
    errorAbsoluto = c(errorAbsoluto,abs(sin(inicio)-polinomioFinal(inicio)))
    
    inicio = inicio + salto
    
  }
  
  x = seq(-pi/64,pi/64,length.out = 10)
  
  
  plot(x, sin(x),type = 'b',col="blue", main = "Funcion Sin(x)" )
  plot(x, polinomioFinal(x),type = 'b',col = "red",main = "Polinomio Generado")
  
  plot(x,errorRelativo,type = 'b',main = "Error Relativo")
  plot(x,errorAbsoluto,type = 'b',main = "Error Absoluto")
  
  
  
}