rm(list = ls())
options(digits = 15)
#Problema 1 : Error relativo.
x = 536.78
y = 536.7
z = abs((x - y) / x) * 100
cat("El valor Absoluto es: ", round(z, 3), "%\n")

#Problema 2 : Algoritmo Raiz Cuadrada

n = 7
E = 1e-5
X = 3
Y = (X + n / X) / 2

iCounter = 0


valorReal = sqrt(7)

while (abs(X - Y) > E)
{
  X = Y
  Y = (X + n / X) / 2
  iCounter = iCounter + 1
  cat("El valor Calculado es: ", round(Y, 5), "\n")
  cat("El error absoluto del calculo es de: ", abs(Y - valorReal), ".\n")
}

cat("El resultado con un error absoluto de: ", E, "es ", Y, ".\n")

cat("El error absoluto del calculo es de: ", abs(Y - valorReal), ".\n")


#Teorema de Taylor


teoremaDeTaylor = function(x)
{
  return (1 + x + (x ^ 2 / 2) + (x ^ 3 / 6))
}


cat("Este es el resultado de Taylor:",
    round(teoremaDeTaylor(0.5), 4),
    "\n")

#Propagacion de Error
velocidad = 4
errorVelocidad = 0.1
tiempo = 5
errorTiempo = 0.1
respuesta = velocidad*tiempo

velocidadInferior = velocidad-errorVelocidad
tiempoInferior = tiempo - errorTiempo

vectorVelocidad = 0
vectorTiempo = 0
vectorDistancia = 0
vectorERelativo = 0
vectorEAbsoluto = 0
iCounter = 1

while(velocidadInferior <= velocidad + errorVelocidad)
{
  distancia = round(velocidadInferior*tiempoInferior,2)
  errorAbsoluto = round(abs(distancia-respuesta),2)
  errorRelativo = round(errorAbsoluto/respuesta*100,2)
  
  vectorVelocidad[iCounter] = velocidadInferior
  vectorTiempo[iCounter] = tiempoInferior
  vectorDistancia[iCounter] = distancia
  vectorEAbsoluto[iCounter] = errorAbsoluto
  vectorERelativo[iCounter] = errorRelativo
  
  iCounter=iCounter+1
  
  tiempoInferior = tiempoInferior + errorTiempo
  
  if( tiempoInferior > tiempo+errorTiempo)
  {
    tiempoInferior = tiempo-errorTiempo
    velocidadInferior = velocidadInferior + errorVelocidad
  }
}

resultados = matrix(c(vectorVelocidad,vectorTiempo,vectorDistancia,vectorEAbsoluto,vectorERelativo)
                    ,ncol=5,nrow = length(vectorEAbsoluto),byrow = FALSE
                    , dimnames = list(seq(1,iCounter-1,1), c("Velocidad","Tiempo","Distancia","E.Absoluto","E.Relativo")))

print(resultados)


#Metodo Horner

polinomio = c(-4,3,-3,0,2)
x_0 = -2


metodoHorner = function(polinomio,x_0)
{

  numeroSumas = 0
  numeroMultiplicaciones = 0
  
  b_n = polinomio[length(polinomio)]
  end = length(polinomio)-1  
  for (a_i in seq(end,1,-1))
  {
    b_n = polinomio[a_i] + b_n*x_0
  
    numeroSumas = numeroSumas + 1
    numeroMultiplicaciones = numeroMultiplicaciones +1
    
  }
  print(b_n)
  cat("Numero de sumas :",numeroSumas,"\n")
  cat("Numero de Multiplicaciones :",numeroMultiplicaciones,"\n")
  
  return(b_n)
}

metodoHorner(polinomio,x_0)

#Interpolacion Perro con Splines
require(PolynomF)
require(graphics)
library(graphicsQC)
rm(list=ls())
#    |  P1   |  P2   |  P3   |  P4   |  P5   |  P6   |  P7   |  P8   |  P9   |  P10  |  P11  |  P12  |  P13  |  P14  |  P15  |  P16  |  P17  |  P18  |  P19  |  P20  |  P21  |  P22  |  P23  |  P24  |  P25  |  P26  |  P27  |  P28  |  P29  |  P30  |  P31  |  P32  |  P33  |  P34  |  P35 |
x = c( 00.50 , 01.01 , 05.85 , 07.46 , 11.28 , 15.20 , 18.46 , 21.25 , 24.15 , 25.80 , 28.00 , 30.80 , 30.81 , 29.40 , 27.40 , 26.21 , 24.97 , 20.32 , 19.54 , 18.80 , 14.04 , 12.54 , 11.68 , 09.55 , 08.30 , 09.10 , 08.85 , 07.80 , 00.50)
y = c( 02.40 , 02.95 , 03.86 , 05.41 , 07.45 , 06.30 , 04.49 , 07.15 , 07.05 , 05.80 , 05.85 , 04.50 , 02.40 , 01.20 , 00.80 , 00.44 , 00.54 , 01.01 , 00.80 , 01.08 , 00.98 , 01.08 , 01.33 , 01.00 , 01.64 , 02.65 , 02.70 , 02.24 , 02.40)

x = x*0.95 #Ajuste a la imagen

print(x)
print(y)

y1 = y[1:7]   ; x1=x[1:7]
y2 = y[7:12]  ; x2 = x[7:12]
y3 = y[12:14] ; x3 = x[12:14]
y4 = y[14:15] ; x4 = x[14:15]
y5 = y[15:18] ; x5 = x[15:18]
y7 = y[18:20] ; x7 = x[18:20]
y8 = y[20:25] ; x8 = x[20:25]
y10 = y[25:26];x10 = x[25:26]
y11 = y[26:28];x11 = x[26:28]
y12 = y[28:29];x12 = x[28:29]

n = length(x)
dog_c = 3

p = spline(y3,x3, n = 201)
i = p$x
p$x = p$y
p$y = i

plot(x, y, pch=20,main = paste("Interpolacion con", n-1, "puntos"),xlim=c(0,31),ylim=c(0,9))
lines(spline(x1, y1, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x2, y2, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x4, y4, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x5,y5, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x7, y7, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x8, y8, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x10, y10, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x11, y11, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x12, y12, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(p, col = dog_c)



#Evaluar en x=1.0001
polinomio = c()

for (i in 1:50) 
{
  polinomio = c(polinomio,1)
}

P = metodoHorner(polinomio,1.0001)

Q = ((1.0001)^51-1)/(1.0001-1)


cat("El error absoluto entre P(X) y Q(x) Es: ",abs(P-Q),"\n")
cat("El error relativo entre P(x) y Q(x) Es: ",abs(P-Q)/Q*100,"%\n")

#Calculo de Pi Binario

print("Pi en numero binario")

variablePi= pi - 3

cat("1")
cat("1")
for (i in 1:13)
{
   variablePi = variablePi*2;
   
   if(variablePi >= 1)
   {
     cat("1")
     variablePi = variablePi - 1;
   }
   else
   {
     cat("0")
   }
}

#De Binario a base 10

 calculoParteEntera = function(X)
 {
   contadorPotencias = 0
   digito = 0
   acomulado=0
   
   while(X > 0)
   {
     digito = X %% 10
     
     acomulado = acomulado + (digito*(2^contadorPotencias))
     
     contadorPotencias = contadorPotencias + 1
     
     X = X %/% 10
   }
   
   return(acomulado)
   
 }
 
 calculoParteDecimal = function(X)
 {
   copiaX = X
   contadorPotencias = 0
   digitos = 0
   digito = 0
   acomulado=0
   
   while(X > 0)
   {
     digitos = digitos+1
     
     X = X %/% 10
   }
   
   contadorPotencias = -digitos;
   
   while(copiaX > 0)
   {
     digito = copiaX %% 10
     
     acomulado = acomulado + (digito*(2^contadorPotencias))
     
     contadorPotencias = contadorPotencias + 1
     
     copiaX = copiaX %/% 10
   }
   
   return(acomulado)
   
   
 }
 
 cat("Primer Numero: ",calculoParteEntera(101010101),"\n")
 
 cat("Segundo Numero: ",calculoParteEntera(1011) + calculoParteDecimal(101),"\n")

 cat("Tercer Numero: ",calculoParteEntera(10111) + calculoParteDecimal(010101010101))
 
 cat("Cuarto Numero: ",calculoParteEntera(111) + calculoParteDecimal(11111111))
 
 
 #De Base 10 a binaria
 
 calculoParteDecimalABinaria = function(x,cantidadDeBits)
 {
   numero=""
   for (i in 1:cantidadDeBits)
   {
     x = x*2;
     
     
     if(x >= 1)
     {
       numero = paste(numero,"1",sep="")
       x = x - 1;
     }
     else
     {
       numero = paste(numero,"0",sep="")
     }
   }
   return(numero)
 }
 
 calculoParteEnteraABinaria = function(x)
 {
   acomulado=0
   nexp=1
   while(x>0)
   {
     
     digito = x%%2
     acomulado =  acomulado+nexp*digito
     nexp=nexp*10
     
     x = x%/%2
     
   }
   return (acomulado)
 }
 

 
 cat("Primer Numero a Base 10: ", calculoParteEnteraABinaria(11),".",calculoParteDecimalABinaria(0.25,10), "\n")
 cat("Segundo Numero a Base 10: ","0",".",calculoParteDecimalABinaria(0.6666666666,10), "\n")
 cat("Tercer Numero a Base 10: ", calculoParteEnteraABinaria(30),".",calculoParteDecimalABinaria(0.6,10), "\n")
 cat("Cuarto Numero a Base 10: ", calculoParteEnteraABinaria(99),".",calculoParteDecimalABinaria(0.9,10), "\n")
 
 #Sub-Matriz tringular superior
 sizes = c(2,3,5,10,20,30,40,50,60,70)
 
 matrizTriangularSuperior = function(tams)
 {
    resultados = c()
    
    for (tamano in tams) 
    {
       matriz = matrix(1,ncol = tamano, nrow = tamano)
       contador = 0
       
       for (i in 1:tamano) 
       {
          for (j in 1:i) 
          {
             contador = contador + matriz[i,j]
          }   
       }
       
       resultados = c(resultados,contador)
       contador = 0
    }
    
    return(resultados)
 }
 
 graficar = matrizTriangularSuperior(sizes)
 
 plot(sizes,graficar,xlab = "TamaÃ±os",ylab = "Resultados", type = 'b',col = 1)
 
 
 #Suma primeros numeros naturales
 
 pruebas = c(10,20,30,40,50,60,70,80,90,100)
 
 sumaNNumerosNaturales = function(valores)
 {
    contador=0
    resultados = c()
    for (valor in valores) 
    {
       contador = 0
       for (i in 1:valor) 
       {
          contador = contador + i^2
       }
       resultados = c(resultados,contador)
    }
    return(resultados)
 }
 
 graficar = sumaNNumerosNaturales(pruebas)
 
 plot(pruebas,graficar, xlab = "Pruebas", ylab = "Resultados",type = 'b')
 
 
 
 #Metodo de Steffensen
 f = function(x)
 {
   return (x^2-cos(x))
 }
 
 Gx = function(x) return (sqrt(cos(x)))
 
 vectorAux =0
 
 puntoFijo = function(a,b,i)
 {
   if((Gx(a)-a)*(Gx(b)-b) < 0)
   {
     x=(a+b)/2
     iteraciones = 0
     vectorAux = 0
     dx = 0
     tol = 1e-8
     
     while (Gx(x) != x & iteraciones < i) 
     {
       dx=abs(a-b)/2
       
       if(dx > tol)
       {
         if (Gx(x) < x)
         {
           b = x
         }
         else {a = x}
       }
       else 
       {
         break
       }  
       x=(a+b)/2
       vectorAux = c(vectorAux,x)
       iteraciones = iteraciones+1
     }
     vectorAux <<- vectorAux
     return(x)
     
   }
   else
   {
     cat("No tiene raíz la funcion en ese intervalo\n")
   }
 }
 
 
 # Método aitken
 aitken = function(x,xPos1,xPos2)
 {
   
   resultado = xPos2 - (((xPos2 - xPos1)^2)/(xPos2 -2*xPos1+x)) 
   
   return(resultado)
 }
 
 i =18
 iteracion = 3
 puntoFijo(0,1,i)
 x2 = 0.824132919311523
 x3 = 0.824137369791667
 while(iteracion < i ){
   cat("i= ",iteracion," x=", aitken(vectorAux[iteracion-2],vectorAux[iteracion-1],vectorAux[iteracion]),"\n")
   iteracion = iteracion +1
 }
 cat("Resultado sin aceleracion: ", puntoFijo(0,1,i), "\n")
 cat("Resultado con aceleracion: ", aitken(vectorAux[i-3],vectorAux[i-2],vectorAux[i-1]), "\n")
 cat("Valor real de la solucion: 0.8241323123")
 cat("Resultado sin aceleracion: ", x2 , "\n")
 cat("Resultado con aceleracion: ", x3, "\n")