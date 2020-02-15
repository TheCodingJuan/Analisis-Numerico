"""
Created on Thu Feb 13 16:35:32 2020
@author: Gabriela Camacho y Juan Camacho
"""
#import numpy as np
#import math as mt
#import matplotlib.pyplot as plot 
#import scipy as sp
from scipy import optimize as op 
#Cohete
def f(t):
    return 6+2.13*t**2-0.0013*t**4
def df(t):
    return 4.24*t-0.0052*t**3
def ddf(t):
    return 4.24-0.0156*t**2

x=0.5
alt_max=0
temp_alt=0
while(op.newton(f,x, fprime=df)!=0):
    i=0
    i=df(x)
    if(op.newton(f,x, fprime=df)==0 and i<0):
        temp_alt=x
        if(temp_alt>alt_max):
            alt_max=temp_alt
    x=x+0.5
print(alt_max)
