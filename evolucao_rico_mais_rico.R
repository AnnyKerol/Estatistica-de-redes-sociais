
V0 <- seq(1:10)

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


set.seed(123)
M0 <- gnp_model(10,0.7)


t <- 10
vt <- matrix(0,ncol = 20,nrow=10)
vt[1,1:10] <- V0
for(i in 1:t){
  if(i!=1){
    vt[i,] <- vt[i-1,]
    vt[i,V0[10]+i] <- V0[10]+i
    
  } else{
      vt[1,(length(V0)+1)] <- V0[10]+1
  }
     
  
}

