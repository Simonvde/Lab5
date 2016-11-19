library(igraph)
library(wordcloud)

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

#Ploting the neighbors of vertices with degree 20 (20 is number selected for this example)
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
modularity(wiki.lp)

membership(wiki.lp)
sizes(wiki.lp)
