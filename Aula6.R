###############################################################
###     AULA 6 - Grafo rico fica mais rico 
##############################################################
library(tidyverse)
library(igraph)
library(hrbrthemes)

N0 <- c(1,2) # vertices inicias 
M0 <- matrix(c(0,1,1,0), ncol = 2)
g0 <- M0 %>% graph_from_adjacency_matrix()

set.seed(1234)
g <- barabasi.game(100, m =1, start.graph = g0)
plot(g, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Grafo rico fica mais rico")

graus <- degree(g) %>% data.frame()
colnames(graus) <- "valor"
hist(degree(g), probability = TRUE)

ggplot(graus, aes(x= valor))+
  geom_histogram(bins = 30,fill="#69b3a2", color="#e9ecef")+
  ggtitle("Grafo rico fica mais rico") + 
  xlab("Grau")+ 
  ylab("Frequência")+
  theme_ipsum() +
  theme(plot.title = element_text(size=15), axis.title.x=element_text(size=13),
        axis.title.y=element_text(size=13))

# a distribuição do grau é de cauda pesada 
