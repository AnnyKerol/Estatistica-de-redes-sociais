###############################################################################
#                              Exercicios Aula 5
###############################################################################

library(tidyverse)
library(igraph)
library(hrbrthemes)

### Simular varias vezes G(N,p), com N=100

##### G(N,p) Model #####
gnp_model<-function(n,p){
  M <- matrix(0, ncol = n, nrow = n)
  for(i in 1:n){
    for(j in 1:i){
      if(runif(1) <= p){
        M[i,j] <- 1
      } else{
        M[i,j] <- 0
      }
    }
  }
  M
}


N <- 100 # numero de vertices 
p <- c(0.5,0.6,0.7,0.8,0.9) # probabilidades utilizadas para gerar os grafos
R <- 1000 # numero de replicas 

Ca <- tibble(         # matriz que guardará todos os coeficientes de algomeração
  "p=0.5" = rep(0, R),
  "p=0.6" =rep(0, R),
  "p=0.7" = rep(0, R),
  "p=0.8" = rep(0, R),
  "p=0.9" = rep(0, R),
)

for(j in 1:length(p)){
  grafos <- list()
  coef_agl <- c()
for(i in 1:R){
  set.seed(i*j+1234)
  grafos[[i]] <- gnp_model(N,p[j])  %>% graph_from_adjacency_matrix()
  coef_agl[i] <- transitivity(grafos[[i]], type="localaverage")
  
} 
  Ca[,j] <- coef_agl
}


save(Ca,file = "Coeficientes de Aglomeracao.Rdata")

ggplot(data=Ca, aes(x= `p=0.9`))+
geom_histogram(binwidth=0.001, fill="#69b3a2", color="#e9ecef")+
ggtitle("G(100,0.9)") + 
  xlab("Coeficientes de Aglomeração")+ 
  ylab("Frequência")+
  theme_ipsum() +
  theme(plot.title = element_text(size=15), axis.title.x=element_text(size=13),
        axis.title.y=element_text(size=13))
  
#cis <- transitivity(a, type="local")  #Ci of each vertex
#transitivity(a, type="localaverage") #average Ci

######
diameter(a, directed = FALSE, unconnected = FALSE, weights = NULL)

#<>-------------------------------------------------------------------

# Diamentro de um grafo regular 

grafo_regular <- graph.formula(1++2,2++3,3++4,4++5,5++6,6++7,7++8,8++9,9++10,1++10,simplify = TRUE) 
diameter(grafo_regular, directed = FALSE, unconnected = FALSE, weights = NULL)
transitivity(grafo_regular, type="local") #average Ci









