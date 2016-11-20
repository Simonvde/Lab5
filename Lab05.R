library(igraph)
library(stats)
library(plyr)
library(xtable)

# intro -------------------------------------------------------------------


karate <- graph.famous("Zachary")
wc <- walktrap.community(karate)
modularity(wc)
membership(wc)
sizes(wc)

vertices(karate)
sub5 <- induced_subgraph(graph=karate,v=wc[[5]])
karate
subgraph(graph = karate,v = c(1,))



plot(walktrap.community(g),g)
plot(wc, karate)
plot(karate, vertex.color=membership(wc))

fc <- fastgreedy.community(karate)
dendPlot(fc)
plot(fc, karate)


IBEX<-read.table("Ibex0809.txt",sep="",header=T)
dd <-as.dist(2*(1-cor(IBEX)))
met="ward.D2" ##  complete,single,average,median
hc <-hclust(dd,method=met)
plot(hc,main=paste(met," method"),axes=TRUE,xlab="",sub="")

#compute the cut  at mean height K:
l<-length(hc$height); hh<-sort(hc$height); K<-mean(hh[1:l])
abline(h=K,lty=2,lwd=2) ##draw the cut
#branches below K make clusters, above go to singletons
groups <- cutree(hc, h = K)  ##obtain  clusters
numgp <- max(groups) #number of clusters.
#extract the names of each group and convert to list
W <- list(names(groups[groups==1]))
##recursively concatenate lists
for (i in 2:numgp){W <- c(W,list(names(groups[groups==i])))}
W


# part 1: ‘random’ graphs -------------------------------------------------
TPR <- function(graph,subgraph,commun){
  sum(count_triangles(graph=subgraph,vids = V(subgraph))>0)/length(V(subgraph))
}

expansion <- function(graph,subgraph,commun){
  fc <- sum(degree(graph,commun))-sum(degree(subgraph,V(subgraph)))/2
  return (fc/length(V(subgraph)))
}

conductance <- function(graph,subgraph,commun){
  mc <- sum(degree(subgraph,V(subgraph)))/2
  fc <- sum(degree(graph,commun))-mc
  return(fc/(2*mc+fc))
}

metric <- function(graph,community,FUN){
  result=0
  for(i in 1:length(community)){
    subgraph <- induced_subgraph(graph=graph,v=community[[i]])
    result = result + length(V(subgraph))/length(V(graph))*FUN(graph,subgraph,community[[i]])
    #print(FUN(graph,subgraph,community[[i]]))
  }
  result
}
modularity(wc)

conductance(karate,subgraph,wc[[4]])

metric(karate,fastgreedy.community(karate),expansion)

communities.abstract <- list(edge.betweenness.community,
             fastgreedy.community,
             label.propagation.community,
             leading.eigenvector.community,
             multilevel.community,
             optimal.community,
             spinglass.community,
             walktrap.community,
             infomap.community)


mydata <- read.table("~/Desktop/mydata.txt", quote="\"")
mat <- as.matrix(mydata)



readGraph <- function(name){
  graph <- read.table(paste("~/Desktop/CSN-Labs/Lab5/data/",name,".txt",sep=""), quote="\"")
  graphMatrix <- as.matrix(graph)
  graph_from_adjacency_matrix(graphMatrix,mode=c("undirected"))
}

#HanoiTower(6,2) Is a graph of the states of the Hanoi Tower game with 6 pegs and 2 disks (It can be seen as the cartesian product of C_6 and K_6, as such it has 36 vertices and 150  edges)
HT <- readGraph("HanoiTower62")

#DoubleStarSnark: a graph without triangles (30 vertices, 45 edges)
dSS <- readGraph("doubleStarSnark")

#DorovtsevGoltsevMendes3 Graph is a graph that can be created by starting with K_2 (an edge) and in every iteration changing every edge by a triangle, so for every edge adding one vertex and 2edges. This graph is obtained after 3 iterations and has 15 vertices and 27 edges
DGM <- readGraph("dorovtsevGoltsevMendes3")

#Friendshipgraph(12), or a windmill. Has 25 vertices and 36 edges.
Friend <- readGraph("FriendshipGraph12")

#A perfectly balanced tree with degree root 3 and height 3. Vertices: 40, edges: 39.
Tree <- readGraph("BalancedTree33")

#The complement of a perfectly balanced tree with degree root 3 and height 2. Vertices: 13, edges 66.
CoTree <- readGraph("BalancedTree32Complement")

graphs <- list(HT,dSS,DGM,Friend,Tree,CoTree)
graphNames <- c("HanoiTower(6,2)","Double Star Snark", "DorovtsevGoltsevMendes3 Graph", "Friendship(12) Graph", "Balanced Tree(3,3)", "Complement of a Balanced Tree (3,2)")

communitieList <- list()
for(i in 1:length(graphs)){
  communitieList[[i]] <- llply(communities.abstract,function(x) x(graphs[[i]]))
}

metricTable <- function(graph,communities){
  output <- matrix(nrow=length(communities),ncol=4)
  for(i in 1:length(communities)){
    comi <- communities[[i]]
    output[i,1] <- metric(graph,comi,TPR)
    output[i,2] <- metric(graph,comi,expansion)
    output[i,3] <- metric(graph,comi,conductance)
    output[i,4] <- modularity(comi)
  }
  rownames(output) <- c("edge.betweenness","
             fastgreedy","
             label.propagation","
             leading.eigenvector","
             multilevel","
             optimal","
             spinglass","
             walktrap","
             infomap")
  colnames(output) <- c("TPT","expansion","conductance","modularity")
  output
}




for(i in 1:length(graphs)){
  print(xtable(metricTable(graphs[[i]],communitieList[[i]]),digits=3,caption=paste("Metrics for",graphNames[i])))
}


# part 2: Wikipedia -------------------------------------------------------


