library(igraph)
library(stats)
library(plyr)
library(xtable)
library(wordcloud)

set.seed(2)

# intro

karate <- graph.famous("Zachary")
wc <- walktrap.community(karate)

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
hanoi <- readGraph("HanoiTower52")

#DoubleStarSnark: a graph without triangles (30 vertices, 45 edges)
snark <- readGraph("doubleStarSnark")

#DorovtsevGoltsevMendes3 Graph is a graph that can be created by starting with K_2 (an edge) and in every iteration changing every edge by a triangle, so for every edge adding one vertex and 2edges. This graph is obtained after 3 iterations and has 15 vertices and 27 edges
dgm <- readGraph("dorovtsevGoltsevMendes3")

graphs <- list(hanoi,snark,dgm)

#Randomize graphs by rewiring 20% of edges.
#l_ply(1:length(graphs),function(i) graphs[[i]] <<- rewire(graphs[[i]],each_edge(p = .15, loops = FALSE,multiple=FALSE)))

#Add Barabasi-Albert graph with 40 vertices
graphs[[length(graphs)+1]] <- as.undirected(sample_pa(40))

#Add karate
graphs[[length(graphs)+1]] <- karate

graphNames <- c("HanoiTower(5,2)","Double Star Snark", "DorovtsevGoltsevMendes3 Graph", "Barabasi-Albert","Zachary's karate")

graphs

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

#plot all communities for 1 graph
graphNumber <- 2
l_ply(communitieList[[graphNumber]],function(comm) plot(comm,graphs[[graphNumber]]))
llply(communitieList[[graphNumber]],sizes)

# part 2: Wikipedia -------------------------------------------------------
#Loading
wiki <- read.graph("wikipedia.gml", format="gml")
g <- as.undirected(wiki, mode = "collapse")

#Summary
summary(wiki)

#Information about labels and IDs
vlabel <- vertex_attr(wiki, "label")
vid <- vertex_attr(wiki, "wikiid")

#Clustering
wiki.fg <- fastgreedy.community(g)

#Function for writing first x frequent words for every cluster that has at least y number of members
first.x.words <- function(x, y){
  cat(1:x, "\n", sep = "\t", file = "Names.txt")
  fileName <- file("Names.txt", open = "a")
  mypath <- file.path(paste(getwd(),"/TextClouds.pdf", sep = ""))
  pdf(file=mypath)

  for(i in 1:length(sizes(wiki.fg)))
  {
    if(sizes(wiki.fg)[i]>=x)
    {
      cat(sample(vlabel[wiki.fg$membership == i], y),"\n", sep = "\t", file = fileName)
      
      tmp <- as.vector(V(g)[membership(wiki.fg) == i])
      l <- sample(V(g)$label[wiki.fg$membership == i], y)
      freq <- unlist(lapply(tmp, get.freq))
      wordcloud(l, freq, min.freq = 1)
    }
  }
  dev.off()
  close(fileName)
}

#Function that return frequency of x in graph
get.freq <- function(x){
  return(length(E(g)[from(x)]))
}

first.x.words(50,50)

#Plotting the neighbors of vertices with degree 20 (20 is number selected for this example)
sample <- V(g)[degree(g)==20]
mypath <- file.path(paste(getwd(),"/NeighborsOf20.pdf", sep = ""))
pdf(file=mypath)
for(i in 1:length(sample))
{
  gv <- make_ego_graph(g, order=1, sample[i])[[1]]
  vclust_orig <- wiki.fg$membership[match(vertex_attr(gv, "label"), vlabel)]
  vclust <- as.integer(factor(vclust_orig))
  coords <- layout_as_star(gv, center = V(gv)[max(degree(gv))==degree(gv)])
  plot(structure(list(membership=vclust), class="communities"), gv, layout = coords)
}
dev.off()

#Writing all neighbours that are different that selected vertex (iterates throught all vertices)
cat("Vertex", "Neighbours in different cluster", "\n", sep = "\t", file = "Differences.txt")
fileName <- file("Differences.txt", open = "a")
for(i in 1:length(V(g)))
{
  n <- neighbors(g,V(g)[i])
  names <- vlabel[n]
  if(length(names[membership(wiki.fg)[n] != membership(wiki.fg)[i]])>0)
    cat(vlabel[i], names[membership(wiki.fg)[n] != membership(wiki.fg)[i]] , "\n", sep = "\t", file = fileName)
}
close(fileName)

#Write some statistics after finished first part
modularity(wiki.fg)
metric(g,wiki.fg,expansion)
metric(g,wiki.fg,conductance)
metric(g,wiki.fg,TPR)

membership(wiki.fg)
sizes(wiki.fg)
