# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Juan Jose Camacho   
# * Gabriela Maria Camacho
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 2001
# -------------------------------------------------------------------------------------

#polinomio f(x)=(1+i)x3+2
#Cancelacion de coeficientes
coef=c(1+1i,0,0,2)
posAEl = c()

for(i in 1:length(coef))
{
  cat("This: ",coef[i],"\n")
  num = 0
  for(j in 1:length(coef))
  {
    if(i != j)
    {
      a = coef[i]
      b = coef[j]
      porc = a/b
      if(porc < 0.01)
        num = num + 1
      print(porc)
    }
  }
  if(num > length(coef)*0.60)
    posAEl = c(posAEl, i)
}

print(posAEl)

coef[posAEl] = 0


print(coef)


#metodo de hroner evaluado en la deribada

grado_pol = 3
dividendo = 1-1i
horner = c(0)
deri = c(0)
horner[1] = coef[1]
for(i in 2:(grado_pol+1)){
  horner[i]=coef[i]+horner[i-1]*dividendo
}

