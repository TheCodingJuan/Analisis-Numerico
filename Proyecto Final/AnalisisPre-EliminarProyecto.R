library(pracma)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(deSolve)
library(phaseR)


hallarDivisor <- function(x){
  val <- 1
  repeat{
    temp <- x %/% val
    if(temp >= 0 && temp <= 10){break}
    else{ val <- val*10}
  }
  return(val)
}

sir<-function(tiempo,estado, parametros){
  
  with(as.list(c(estado,parametros)),{
    dS<- -beta*s*i
    dI<- beta*s*i-gamma*i
    dR<- gamma*i
    return(list(c(dS,dI,dR)))
  })
}

#Lectura de los datos 
altasMadrid = read.csv(file = "Altas_Madrid.csv",header = TRUE,sep = ",")
ggplot(altasMadrid, aes(x = fecha ,y = total,group = 1), na.rm = TRUE)+geom_line()+theme_tufte()+labs(x = "Fechas Sorry por el formato", y = "Casos", title = "Variación de Altas Madrid")


casosMadrid = read.csv(file = "Casos_Madrid.csv",header = TRUE)
ggplot(casosMadrid, aes(x = fecha ,y = total,group = 1), na.rm = TRUE)+geom_line()+theme_tufte()+labs(x = "Fechas Sorry por el formato", y = "Casos", title = "Variación de Casos Madrid")


divisor <- hallarDivisor(3266126)

inic <- c(s = 3266126/divisor , i = 1 / divisor, r = 0)

parametros <- c(beta = (2.5 /10), gamma = (2/ 10))
tiempo <- seq(1 , 77, by=1)

out <- lsode(y = inic, times = tiempo, func = sir, parms = parametros)
out <- as.data.frame(out)
out$time <- NULL
#ead(out, 77)

errorR = c()
errorI = c()

for (alta in c(1:77)){
  errorR = c(errorR,abs((altasMadrid$total[alta] - out$r[alta])/altasMadrid$total[alta]))
  errorI = c(errorI,abs((casosMadrid$total[alta] - out$i[alta])/casosMadrid$total[alta]))
}

print(errorR)
print(errorI)
