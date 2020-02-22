# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Juan Jose Camacho   
# * Gabriela Maria Camacho
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 2001
# -------------------------------------------------------------------------------------


#Numeros de Chebyshev

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


n = 7

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
  
  
  
}