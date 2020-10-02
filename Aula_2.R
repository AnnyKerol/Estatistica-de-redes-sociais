library(igraph)

grafo <- graph.formula(1++2,2++3,3++4,4++5,5++6,6++7,7++8,8++9,9++10,1++10) 
#plot(grafo)

# Exemplo
N <- 10
Tempo <- 100
x <- c(+1,-1) # opiniões
b <- 100
positiva <- c()
positiva_5 <- c()
grafos <- list()
for( j in 1:b){

set.seed(j*123)
X0 <- sample(x,N,replace=TRUE,prob = c(0.3,0.7)) # lista de opiniões inicias

Xn <- matrix(0,nrow = Tempo,ncol = N)


atores <- seq(1:N)
Xn[1,] <- t(X0)
An <- c()
for(i in 1:Tempo){
  set.seed(i*123)
  An[i] <- sample(atores,size = 1) # sorteando o ator que emitiu a opinião no instante n
  influes <- grafo[[An[i]]] %>% unlist() %>% sample(size = 1)
  if(i!=1){
  Xn[i,An[i]] <- Xn[i-1,influes]   
  } else {
      Xn[1,An[i]] <- Xn[1,influes]
  }  
  
for(k in 1:length(atores)){
     if(k != An[i]){
       print(k)
       if(i==1){
         Xn[1,k] <- Xn[1,k]
        } else{
          Xn[i,k] <- Xn[i-1,k]
        }
     }
    
   }
  
}
Xn
An
grafos[[j]] <- Xn
positiva[j] <- sum(Xn[100,10]==+1)
positiva_5[j] <- sum(Xn[100,5]==+1)

}
sum(positiva==1)
sum(positiva_5==1)
# repetindo o experimento sucessivas vezes o ator 10 dará uma opinião positiva em 30/100 vezes

grafos
load(file="inicio.Rdata")
install.packages("animation")

sum(grafos[[1]][100,]==+1)
sum(grafos[[2]][100,]==+1)
sum(grafos[[3]][100,]==+1)
sum(grafos[[4]][100,]==+1)
sum(grafos[[5]][100,]==+1)
