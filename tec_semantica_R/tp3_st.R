###### TRABAJO PRACTICO NRO. 3 ###########
##########################################
source("TOSimGenes.R")
source('TOSim_XXnorm_stats.R')
source('TOSim_BPnorm_stats.txt')
#source('go.R')

#-----1.Instalar en R los paquetes de bioconductor-----

source("http://bioconductor.org/biocLite.R")

biocLite()

#-----2.Instalar paquetes-----

biocLite("GEOquery")
biocLite("GO.db")

#----3.Recuperar los experimentos------

summary(gse6209)

TOSim_BPnorm

#----4.Realizar un agrupamiento------

library(cluster)

cl<-clara(gse6209[,1:10],100)
cl$medoids
plot(cl)

#-----5. Validar numéricamente-----
sse.p.pers <- array()
sil.pers <- array()

for(i in 1:14){
  cl<-clara(gse6209[,1:10],1+i)
  cl.meds <- cl$medoids[cl$clustering]
  sse.p.pers[i] <- sum(dist(gse6209[,1:10])[cbind(row.names(gse6209[,1:10]), cl.meds)]**2)
  sil.pers[i] <- cl$silinfo$avg.width
}

par(mfrow=c(1,1))
plot(2:(14+1), sil.pers, type="b", xlab="k") 
plot(2:(14+1), sse.p.pers, type="b", xlab="k")

#-----6.	Recuperar algún grupo------

c<-names(cl$cluster[cl$cluster[]==1])

TOSim_BPnorm_stats(c,go)

