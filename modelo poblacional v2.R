# Programa para la construcción del modelo porbalcional del mercado de creditos hipotecarios

library(ggplot2)
library(dplyr)

source('C:/Users/Public/Documents/R/CONEXIONES/CONEXION LocalHost.R')

x<-dbGetQuery(con,
              "select*
              from t6
              ")


cota.inf=150e6
percent=.8
edad<-seq(20,79)

# Calculo del valor asegurado promedio por edad
vas.prom<-vector(length = 60)
for(i in 20:79){
  vas.prom[i-19]<-mean(x[x[,'edad']==i,]$valor_credito/1e6)
}

# Calculo del percentil 75 de la distribucion por edad
p75<-vector(length = 60)
  for(i in 20:79){
  p75[i-19]<-quantile(x[x[,'edad']==i,]$valor_credito/1e6,percent)
}

# Calculo del valor asegurado maximo por edada
vas.max<-vector(length = 60)
for(i in 20:79){
  vas.max[i-19]<-max(x[x[,'valor_credito']==i,]$valor_credito/1e6)
}

# Numero total de creditos por edad
n.total<-vector(length = 60)
for(i in 20:79){
  n.total[i-19]<-length(x[x[,'edad']==i,]$valor_credito)
}

# Numero de creditos que superan la cota definida
n.limit<-vector(length = 60)

for(i in 20:79){
  n.limit[i-19]<-length(subset(x,edad== i & valor_credito > cota.inf)$valor_credito)
}

#Tabla de resumen 
t1<-cbind(edad, vas.prom,p75,vas.max,n.limit,n.total,n.total/nrow(x),n.limit/n.total)
t1<-as.data.frame(t1)
# par(mfrow=c(1,1))
# 
# plot(edad, t1[,7],type = 'l')
# plot(edad, t1[,6],type = 'l')


 write.csv(t1,'C:/Users/Public/Documents/VI/ONEROSO/modelo.poblacionall.csv', row.names = F)

# Intento de graficar con ggplot2 
f<-ggplot(t1)
f<-f+geom_point(aes(edad,vas.prom/1e6))
f<-f+geom_point(aes(edad,vas.max/1e6),color="steelblue")
f+geom_point(aes(edad,p75/1e6),color="red")
f+geom_boxplot()
quantile(x$valor_credito,c(.99))

# Generación de percentiles por edad, manera mas eficiente de determinar el porcentaje
# poblacional que supera ciertos limites datos. ej. en a edad 40 el 80% de la poblacion
# tiene un prestamo inferior a $140M equivalente a que el 20% tiene mas de $140M

percentil<-vector(length = 60)
l<-list()

for(j in 50:99){
  for(i in 20:79){
    percentil[i-19]<-quantile(x[x[,'edad']==i,]$valor_credito/1e6,.49+(j-49)/100)
  }
  l[[j-49]]<-percentil
}  
  
l<-as.data.frame(l)  
names(l)<-seq(50,99)


#Calculo del Valor del credito dado que se elige la poblacion que esta por encima del P80
vas.prom.condicional<-vector(length = 60)
for(i in 20:79){
  vas.prom.condicional[i-19]<-mean(subset(x,edad==i & valor_credito>=l[i-19,6])$valor_credito)
}

l[,26]<-vas.prom.condicional

write.csv(l,'C:/Users/Public/Documents/VI/ONEROSO/modelo.poblacional v4.csv', row.names = F)


 