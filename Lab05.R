library(igraph)
library(stats)
karate <- graph.famous("Zachary")
wc <- walktrap.community(karate)
modularity(wc)
membership(wc)
sizes(wc)

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
