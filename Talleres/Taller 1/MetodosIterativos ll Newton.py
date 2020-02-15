# -*- coding: utf-8 -*-
"""
Editor de Spyder
Gabriela camacho Y Juan Camacho
Este es un archivo temporal.
"""
import numpy as np
import math as mt
import matplotlib.pyplot as plot 
import scipy as sp
from scipy import optimize as op 
 
#Convergencia de metodos iterativos
def f(x):
    return mt.e**x-x-1
def df(x):
    return mt.e**x-1
def ddf(x):
    return mt.e**x
# Punto 2
i=1
p=10
x=0.5
print("Metodo de Newton\n")
for i in range(p):
    
    if(x != 1):
        print(op.newton(f,x))
    x=x+0.1
# Punto 3
i=1
X=0.5
print("Metodo de Newton rapshon\n")
for i in range(p):
    
    if(x != 1):
        print(op.newton(f,x, fprime=df))
    x=x+0.1
 


i=1
X=0.5
print("Metodo de Newton rapshon mejorado\n")
for i in range(p):
    
    if(x != 1):
        print(op.newton(f,x, fprime2=ddf))
    x=x+0.1
