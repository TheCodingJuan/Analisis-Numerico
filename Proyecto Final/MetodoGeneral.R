#n = Total de poblacion en madrid 3.266.126 https://www.enterat.com/actualidad/habitantes-madrid.php
#s = Suceptible, al inicio todos 
#i = Infectados, ya tiene o tuvieron el virus (ya no son suceptibles)
#beta=beta velocida de infeccion
#t= tiempo en dias 
library(deSolve)
library(phaseR)
sir<-function(tiempo,estado, parametros){
 
  with(as.list(c(estado,parametros)),{
    dS<- -beta*s*i
    dI<- beta*s*i-gamma*i
    dR<- gamma*i
    return(list(c(dS,dI,dR)))
  })
}
#s=1-I
inic <- c(s= 1-1e-6,i=1e-6,r=0.0)
#Beta = numero de contactos que ha tenido una persona por dia
#Gamma = fraccion de infectados que se recuperaran en un dia
parametros <- c(beta=1.4247, gamma=0.14286)
tiempo <-seq(1,82, by=1)
#Solucion general 
out <- ode(y = inic, times = tiempo, func = sir, parms = parametros)
out <- as.data.frame(out)
out$time <- NULL
head(out, 10)