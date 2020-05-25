library(deSolve)

SI <- function(t, x, parametros){
  with(as.list(c(parametros, x)),{
    dS <- - r*beta*S*I/(S+I)
    dI <- + r*beta*S*I/(S+I)
    derivadas <- c(dS, dI)
    #Devolvemos las derivadas a traves de una lista
    return(list(derivadas))
  })
}

#Definimos los parametros del modelo, r y beta
parametros <- c(r=5, beta=0.2)
#Definimos los valores iniciales, S e I
v_iniciales <- c(S=999, I=1)
#Definimos  t 
dt <- seq(0, 20, 0.1)
#Mediante"ode"resolvemos y generamos un data frame 
simulacion.si <- as.data.frame(ode(y=v_iniciales,  times=dt, func=SI,parms=parametros))

#attach:referencia directa a las columnas en simulacion.si 
attach(simulacion.si)


#Calculamos del tamaño de la población: N
N <- sum(v_iniciales)
#Representamos graficamente 
plot(dt, S, type="l", col="blue", 
     ylim=c(0,sum(v_iniciales)), 
     xlab="tiempo (en d?as)", 
     ylab="número de individuos")
lines(dt, I, type="l", col="red")


title("Modelo SI")
legend(13, 950, legend=c("Susceptibles", "Infectados"),
       col=c("blue", "red"), lty=rep(1, 2)) 