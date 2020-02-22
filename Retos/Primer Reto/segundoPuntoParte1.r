# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Juan Jose Camacho   
# * Gabriela Maria Camacho
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 2001
# -------------------------------------------------------------------------------------


#Instalar Pracma
library(pracma)
library(MASS)
# -------------------------------------------------------------------------------------
#   2.Parte 1: Evaluar en Taylor
#     Para este punto se hizo uso de la funcion Taylor en la libreria Pracma
#     para asi poder obtener el polinomio de taylor y luego esta la funcion 
#     evaluarEnTaylor() la cual evalua el polinomio en el punto dado el cual
#     es obtenido del intervalo. Se evaluo el polinomio en 10 puntos del
#     
# -------------------------------------------------------------------------------------
#Polinomio de Taylor en Sen(X)
f = function(x)
{
    return (sin(x))
}

polinomioCalculado = taylor(f,0,3)


print(as.fractions(polinomioCalculado))

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


salto = (pi/64-(-pi/64))/10
inicio = -pi/64

for (i in 1:10) {
    cat("Valor de Seno: ",sin(inicio),"\t")
    cat("Valor en Taylor: ",evaluarEnTaylor(inicio,polinomioCalculado),"\n")
    
    inicio = inicio + salto
    
}
