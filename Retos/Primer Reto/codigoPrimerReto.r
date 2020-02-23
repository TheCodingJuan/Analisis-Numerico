#Instalar Pracma
library(pracma)
#Numeros de Chebyshev
 rm(list = ls())
 
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
 
 
 
 listaCalculada= numerosChebyShev(-pi/64,pi/64,seq(1,20))
 
 print(listaCalculada)
 
 #Polinomio de Taylor en Sen(X)

 f = function(x)
 {
     return (sin(x))
 }
 
 polinomioCalculado = taylor(f,0,10)
 
 print(polinomioCalculado)
 
 evaluarEnTaylor = function(x,polinomio)
 {
     grados = length(polinomio)-1
     total = 0
     for (i in polinomio) 
     {
         total = total + i * (x^grados) 
         grados = grados - 1
     }
     
     return (total)
 }
 
 print(evaluarEnTaylor(1,polinomioCalculado))
 
 salto = (pi/64-(-pi/64))/10
 inicio = -pi/64
 
 for (i in 1:10) 
 {
     
     cat("Valor de Seno: ",sin(inicio),"\t")
     cat("Valor en Taylor: ",evaluarEnTaylor(inicio,polinomioCalculado),"\n")
     
     
     
     inicio = inicio + salto
     
 }