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
 
 
 
 listaCalculada= numerosChebyShev(-pi/64,pi/64,seq(1,20))
 
 print(listaCalculada)
