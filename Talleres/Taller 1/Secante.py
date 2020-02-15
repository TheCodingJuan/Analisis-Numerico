import math as mt
import numpy as np
import matplotlib as mtp
import matplotlib.pyplot as plt
def f(x):
    return mt.log(x+2) - mt.sin(x)

def secante(E,x_1,x_2):
    error=mt.inf
    errorx = []
    cont=0
    cont2 = 1 
    while(error>E):
        print(x_1, x_2)
        cont=cont+1
        nx=(x_1-((f(x_1)*(x_1-x_2))/(f(x_1)-f(x_2))))
        error=mt.fabs(f(nx)-0)
        errorx.append(error)
        cont2 = cont2+1
        print(error)
        x_2=x_1
        x_1=nx
    return format(nx, ".10g"),cont,nx,f(nx),errorx

print(secante(10**-8,-1.9,-1.1)) # se eligio este punto ya que  (-1.9, 0 pero no covergia ya que el punto 0 tiene una pendiente contraria a la de -1.9 y al tener maximos, no se permitia convergencia, mientras que con -1.1 y -1.9 no ocurre 
form,conta,nnx,ffx,errorT = secante(10**-8,-1.9,-1.1)
x = range(len(errorT)) #Eje x de la gráfica
fig, ax = plt.subplots()
ax.scatter(x, errorT,alpha=0.3, edgecolors='none')
ax.grid(True)
ax.set(xlabel='Iteraciones', ylabel='Error',
       title='Iteraciones en el error')

## Punto 2

def f(x):
    return mt.log(x+2) - mt.sin(x)

def secante(E,x_1,x_2):
    error=mt.inf
    cont=0
    errorx = []
    cont2 = 1 
    while(error>E):
        print(x_1,x_2)
        cont=cont+1
        nx=(x_1-(f(x_1)*((x_1-x_2)/(f(x_1)-f(x_2)))))
        error=mt.fabs(f(nx)-0)
        errorx.append(error)
        print(error)
        x_2=x_1
        x_1=nx
        
    return format(nx, ".10g"),cont, errorx

print(secante(10**-8,-1.9,-1.1))
form1, cont1, errorx1 = secante(10**-8,-1.9,-1.1)
fig, ax = plt.subplots()
ax.scatter(x, errorx1,alpha=0.3, edgecolors='none')
ax.grid(True)
ax.set(xlabel='Iteraciones', ylabel='Error',
       title='Iteraciones en el error')
