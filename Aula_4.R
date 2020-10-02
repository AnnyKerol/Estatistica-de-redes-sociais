
library(igraph)

# Modelo G(n,p)
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

# G(n,pn) Model
gnpn_model<-function(n,lambda){
  M <- matrix(0, ncol = n, nrow = n)
  pn <- lambda/n
  for(i in 1:n){
    for(j in 1:i){
      if(runif(1) <= pn){
        M[i,j] <- 1
        M[j,i] <- 1
      } else{
        M[i,j] <- 0
        M[j,i] <- 0
      }
    }
  }
  M
}


# Exercicio 2
lambdas <- seq(1:5)
N <- 100
grafos_ErdosR <- list()
for(i in 1:length(lambdas)){
     grafos_ErdosR[[i]] <- gnpn_model(N,lambdas[i])
}





# Exercicio 3

N <- 100

# Valores de lambda < 1
g1 <- gnpn_model(N,0.7) %>% graph_from_adjacency_matrix()
g2 <- gnpn_model(N,0.5) %>% graph_from_adjacency_matrix()
g3 <- gnpn_model(N,0.3) %>% graph_from_adjacency_matrix()
g4 <- gnpn_model(N,0.1) %>% graph_from_adjacency_matrix()

plot(g1, vertex.label= NA, edge.arrow.size=0.3,vertex.size = 0.9, 
            xlab = "G(N,pN) Lambda 0.7")
plot(g2, vertex.label= NA, edge.arrow.size=0.3,vertex.size = 0.9, 
     xlab = "G(N,pN) Lambda 0.5")
plot(g3, vertex.label= NA, edge.arrow.size=0.3,vertex.size = 0.9, 
     xlab = "G(N,pN) Lambda 0.3")
plot(g4, vertex.label= NA, edge.arrow.size=0.3,vertex.size = 0.9, 
     xlab = "G(N,pN) Lambda 0.1")


# Valores de lambda > 1
g1 <- gnpn_model(N,2) %>% graph_from_adjacency_matrix()
g2 <- gnpn_model(N,5) %>% graph_from_adjacency_matrix()
g3 <- gnpn_model(N,7) %>% graph_from_adjacency_matrix()
g4 <- gnpn_model(N,9) %>% graph_from_adjacency_matrix()

plot(g1, vertex.label= NA, edge.arrow.size=0.3,vertex.size = 0.9, 
     xlab = "G(N,pN) Lambda 2")
plot(g2, vertex.label= NA, edge.arrow.size=0.3,vertex.size = 0.9, 
     xlab = "G(N,pN) Lambda 5")
plot(g3, vertex.label= NA, edge.arrow.size=0.3,vertex.size = 0.9, 
     xlab = "G(N,pN) Lambda 7")
plot(g4, vertex.label= NA, edge.arrow.size=0.3,vertex.size = 0.9, 
     xlab = "G(N,pN) Lambda 9")

# quanto maior o tamanho de lambda mais conectado será o gráfico



# Exercicio 4

gnpn_model<-function(n,lambda){
  M <- matrix(0, ncol = n, nrow = n)
  pn <- lambda/n
  for(i in 1:n){
    for(j in 1:i){
      if(runif(1) <= pn){
        M[i,j] <- M[j,i] <- 1 # atores a e b se influenciam mutuamente
        
      } else{
        M[i,j] <- 0
      }
    }
  }
  M
}

set.seed(1994) 
g1 <-  gnpn_model(N,0.5) %>% graph_from_adjacency_matrix()
plot(g1, vertex.label= NA, edge.arrow.size=0.3,vertex.size = 0.9, 
     xlab = "G(N,pN) Lambda 0.5")

g2 <-  gnpn_model(N,5) %>% graph_from_adjacency_matrix()
plot(g2, vertex.label= NA, edge.arrow.size=0.3,vertex.size = 1, xlab = "G(N,pN) Lambda 5")

N <- 100
t <- 1000 #instantes
x <- c(+1,-1) # opiniões
set.seed(123) 
X0 <- sample(x,N,replace=TRUE) # lista de opiniões inicias
Xn <- matrix(0,nrow = t,ncol = N)
An <- c()
opin <- c()

atores <- seq(1:N)
g1 <- g2
Xn[1,] <- t(X0)
for(i in 1:t){
  set.seed(i*123)
  An[i] <- sample(atores,size = 1) # sorteando o ator que emitiu a opinião no instante n
        if((length(g1[[An[i]]])==1) & (unlist(g1[[An[i]]])[1]%in%NA)){ # saber se o ator tem influenciador
          if(i!=1){
                Xn[i,An[i]] <- Xn[1,An[i]] # opinião é mantida na inicial
                Xn[i,!An[i]] <- Xn[i-1,!An[i]] # outros atores atualizam a opinião de acordo com a anterior
              } 
                
          
        } else {
          
          if(length(unlist(g1[[An[i]]]))==1){
            print("Entrou aqui")
            influes <- unlist(g1[[An[i]]])
            if(i==1){
              Xn[i,An[i]] <- Xn[i,influes]
            }else{
              Xn[i,An[i]] <- Xn[i-1,influes]
              Xn[i,!An[i]] <- Xn[i-1,!An[i]] 
            }
          }  
          
            if(length(unlist(g1[[An[i]]])) >1){
              print("Não entrou")
             influes <- unlist(g1[[An[i]]])
             influ <- sample(influes,size = 1)
                if(i==1){
                Xn[i,An[i]] <- Xn[i,influ]
                 }else{
                Xn[i,An[i]] <- Xn[i-1,influ]
                Xn[i,!An[i]] <- Xn[i-1,!An[i]] 
                }
        }  
        }
       
       
       for(k in 1:length(atores)){
         if(k != An[i]){
           print(k)
           if(i==1){
             Xn[i,k] <- Xn[i,k]
           } else{
             Xn[i,k] <- Xn[i-1,k]
           }
         }
         
       }
       
}

dados <- tibble(tempo=c(seq(1:t)),positiva=c(rep(0,t)),negativa=c(rep(0,t)))
dados1 <- reshape2::melt(dados,id="tempo",measure.vars = c("positiva", "negativa"))

for(k in 1:t){
  dados[k,2] <- sum(Xn[k,]==1)
  dados[k,3] <- sum(Xn[k,]== -1)
}

ggplot(dados1, aes(x=tempo,y=value, color=variable))+
  geom_line(size=1)+ ylab("Frequência") +xlab("Tempo")+
  theme_bw()+
  ggtitle("Lambda 0.5")+
  theme(legend.position ="bottom", legend.title=element_blank(),
        axis.text=element_text(size=12, face="bold", colour="gray24"), # modifica os textos dos eixos
        axis.title=element_text(size=12,face="bold"))


