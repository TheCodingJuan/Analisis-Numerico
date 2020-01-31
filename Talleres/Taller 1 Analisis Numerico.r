rm(list = ls())

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
