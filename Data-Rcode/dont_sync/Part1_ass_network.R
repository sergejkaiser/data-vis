---
  title: "Excepts Assignment Network Part1"
author: "Sergej Kaiser"
date: "13/01/2017"
output:
  html_document: default
html_notebook: default
css: markdown7.css
---
  
setwd("/Users/sergej/Google Drive/QASS_Master/Network-Analysis-Week2/Assignment_Network")
rm=ls()
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
# Note that the output of graph.coreness refers simply to the *degree*
# of the k-core, not to the k-core itself; thus, two vertices both with 
# coreness of 3 may not be connected at all and thus may be in separate
# k-cores. 
#
# One way to get a sense of the actual k-core structure is to simply 
# plot the graph and color-code by k-core:
make_k_core_plot <- function (g) {
  lay1 <- layout_with_fr(g)
  plot(g, 
       vertex.color = graph.coreness(g), 
       layout=lay1, 
       edge.arrow.size = .5)
} 
cum.distr.plot<-function(g,attrib ){
  df<-get.data.frame(g,what=c("vertices"))
  namex <- deparse(substitute(attrib))
  call<-substitute(dplyr::select(df, name, attrib))
  v<-eval(call)
  
  v<-sort(v)
  v<-as.data.frame(v)
  call.3<-substitute(plyr::arrange(df,order(attrib)))
  df<-eval(call.3)
  namexs<-c("COU",deparse(substitute(attrib)))
  names(v)<-namexs
#  print(v)
  v$cum.rel.freq<-cumsum(v[,2])/sum(v[,2])
  return(v)
}

plot( x=v[,1], y=v[,2], pch=19, cex=1.2,  col="orange", 
      xlab=namex, ylab="Cumulative Frequency", main="Cummulative distribution")
detachAllPackages()
lapply(c("sna", "ggplot2","intergraph", "GGally", "igraph", "network", "dplyr","ocean"), 
       require, character.only=T)
load("edgelist_weights.Rdata")
load("verticies_additional.Rdata")
From<-c(rep("CAN",5),rep("AUS",5),rep("BEL",5),"ARE",  "AUT", "CHL")
To <- c("ARE", "AUS", "AUT", "BEL",  "CHL", "ARE", "AUT", "BEL", "CAN", "CHL","ARE", "AUS", "AUT", "CAN", "CHL", "BEL","CAN","AUS")

Value<-exports.2005.manufacturing$Value[1:18]
minimal.exp<-data.frame(From,To,Value)
min.graph<-graph_from_data_frame(minimal.exp)
exgr.2005.manufac.graph<-graph.data.frame(select(exports.2005.manufacturing,COU,PAR,Value), vertices=exports.2005.manufacturing.actors)
exgr.2005.manufac.network<-intergraph::asNetwork(exgr.2005.manufac.graph)
gapply(exgr.2005.manufac.networ, MARGIN=1, STATS=sum,
#exclude self loops, result is a simple graph
exgr.2005.manufac.graph<- simplify(exgr.2005.manufac.graph, remove.loops=TRUE, remove.multiple = F)
#diameter, density, mean shortest path
#longest shortest path 2
exgr.2005.manufac.graph$diameter<-diameter(exgr.2005.manufac.graph, directed = TRUE, unconnected = TRUE, weights = NULL)
#very dense 0.7
exgr.2005.manufac.graph$density<-edge_density(exgr.2005.manufac.graph, loops=F)
#low mean distance
exgr.2005.manufac.graph$mean_distance<-mean_distance(exgr.2005.manufac.graph, directed=T)
#degree distribution #measure of influence

V(exgr.2005.manufac.graph)$deg.in <- degree(exgr.2005.manufac.graph, mode="in")

V(exgr.2005.manufac.graph)$deg.out <- degree(exgr.2005.manufac.graph, mode="out")



edge_attr_names(exgr.2005.manufac.graph)

deg.dist <- degree_distribution(exgr.2005.manufac.graph, cumulative=T, mode="all")
plot( x=0:max(degree(exgr.2005.manufac.graph, mode="all")), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Total degree", ylab="Cumulative Frequency", main='Manufacturing World Trade Network')

exgr.2005.manufac.graph$deg.dist.in <- degree_distribution(exgr.2005.manufac.graph, cumulative=T, mode="in")
exgr.2005.manufac.graph$deg.dist.out<-degree_distribution(exgr.2005.manufac.graph, cumulative=T, mode="out")
#heterogeneity 
svg(file="degree_distr.png" units="in",      width=5,      height=4,      pointsize=12,      res=72)
par(mfrow=c(1,2))

plot( x=0:max(V(exgr.2005.manufac.graph)$deg.in), y=1-exgr.2005.manufac.graph$deg.dist.in , pch=19, cex=1.2, col="orange", 
      xlab="In Degree", ylab="Cumulative Frequency",main='Manufacturing World Trade Network')

plot( x=0:max(V(exgr.2005.manufac.graph)$deg.out), y=1-exgr.2005.manufac.graph$deg.dist.out, pch=19, cex=1.2,  col="orange", 
      xlab="Out Degree", ylab="Cumulative Frequency",main='Manufacturing World Trade Network')
dev.off()
par(mfrow=c(1,1))

#value is mean of two countries in trading partnership
# closeness centrality 
V(exgr.2005.manufac.graph)$closeness.centr.in<-closeness(exgr.2005.manufac.graph, mode="in", weights=NA ) 
V(exgr.2005.manufac.graph)$closeness.centr.out<-closeness(exgr.2005.manufac.graph, mode="out", weights=NA ) 
#0.28 implies that network quite far away from being domination by single actor, but also no completly equal 
exgr.2005.manufac.graph$Network.centralization.in<-centr_clo(exgr.2005.manufac.graph, mode="in", normalized=T)$centralization
exgr.2005.manufac.graph$Network.centralization.out<-centr_clo(exgr.2005.manufac.graph, mode="out", normalized=T)$centralization


#hubscores
V(exgr.2005.manufac.graph)$hub.s <- hub_score(exgr.2005.manufac.graph, weights=NA)$vector
V(exgr.2005.manufac.graph)$authr.s <- authority_score(exgr.2005.manufac.graph, weights=NA)$vector


#transitivity clustering
exgr.2005.manufac.graph$trans.global<-transitivity(exgr.2005.manufac.graph,type=c("global"))

V(exgr.2005.manufac.graph)$trans.local<-transitivity(exgr.2005.manufac.graph,type=c("local"))
#plot(cum.distr.plot(g=exgr.2005.manufac.graph, attrib=trans.local)$trans.local,cum.distr.plot(g=exgr.2005.manufac.graph, attrib=trans.local)$cum.rel.freq)
exgr.2005.network<-intergraph::as.network((exgr.2005.manufac.graph))

#par(mfrow(c(1,2)))
#plot(exgr.2005.manufac.graph, vertex.size=V(exgr.2005.manufac.graph)$hub.s*10, main="Hubs", layout=layout_nicely(exgr.2005.manufac.graph),vertex.label.dist = 0.5, vertex.label.cex = 0.7, edge.width = 0.5)
#plot(exgr.2005.manufac.graph, vertex.size=V(exgr.2005.manufac.graph)$authr.s*15, main="Authorities", layout=layout_nicely(exgr.2005.manufac.graph),vertex.label.dist = .6, vertex.label.cex = 0.5, edge.width = 0.1)

l.nice<-layout_nicely(exgr.2005.manufac.graph) 



exgr.2005.manufac.graph.undir<-shortest_paths(exgr.2005.manufac.graph,BEL)
exgr.2005.manufac.graph.undir<-as.undirected(exgr.2005.manufac.graph, mode="mutual")
cluster.exgr.2005<-cluster_louvain(exgr.2005.manufac.graph.undir )
members<-membership(cluster.exgr.2005)
#svg(file = "communities.png" units="in",      width=5,      height=4,      pointsize=12,      res=72)

#categorize k-core values into 6 cat
exgr.2005.manufac.graph$cor_cont<-graph.coreness(exgr.2005.manufac.graph)
exgr.2005.manufac.graph$coreness<-ifelse(graph.coreness(exgr.2005.manufac.graph)>139,140 ,graph.coreness(exgr.2005.manufac.graph))
exgr.2005.manufac.graph$coreness<-ifelse((exgr.2005.manufac.graph$coreness<140 &exgr.2005.manufac.graph$coreness>129),135,exgr.2005.manufac.graph$coreness)
exgr.2005.manufac.graph$coreness<-ifelse((exgr.2005.manufac.graph$coreness<130 &exgr.2005.manufac.graph$coreness>119),125,exgr.2005.manufac.graph$coreness)
exgr.2005.manufac.graph$coreness<-ifelse((exgr.2005.manufac.graph$coreness<120 &exgr.2005.manufac.graph$coreness>100),110,exgr.2005.manufac.graph$coreness)
exgr.2005.manufac.graph$coreness<-ifelse((exgr.2005.manufac.graph$coreness<100 &exgr.2005.manufac.graph$coreness>75),90,exgr.2005.manufac.graph$coreness)
exgr.2005.manufac.graph$coreness<-ifelse((exgr.2005.manufac.graph$coreness<65 &exgr.2005.manufac.graph$coreness>48),57,exgr.2005.manufac.graph$coreness)
par(mfrow=c(1,2))
plot(exgr.2005.manufac.graph, vertex.color =members, main="Community Structure in the manufacturing trade network",vertex.size=0.05*V(exgr.2005.manufac.graph)$deg.out , layout=layout_with_fr(exgr.2005.manufac.graph), vertex.label.dist = 0.5, vertex.label.cex = 0.7, edge.width = 0.2, edge.arrow.size=0.5,edge.arrow.width=0.5,edge.arrow.mode=0)
plot(exgr.2005.manufac.graph, 
     vertex.color = exgr.2005.manufac.graph$coreness, 
     vertex.size=0.05*V(exgr.2005.manufac.graph)$deg.out , layout=layout_with_fr(exgr.2005.manufac.graph),  main="Maximal connected subgraph structure (k-core)",vertex.label.dist = 0.5, vertex.label.cex = 0.7, edge.width = 0.2, edge.arrow.size=0.5,edge.arrow.width=0.5,edge.arrow.mode=0)
# cou.total.exports<-select(exports.2005.manufacturing,COU,PAR,Value) %>% tidyr::spread(.,key=PAR,Value) %>% replace(is.na(.), 0) %>%
#   mutate(sum = rowSums(.[2:143]))%>% select(.,COU,sum)
# vertices<-igraph::as_data_frame(exgr.2005.manufac.graph,what=("vertices"))
# vertices.gdp.inc<-left_join(vertices,unique(select(exports.2005.manufacturing,COU,GDP_pc_2005,Group)),by=c("name"="COU"))
# ggplot(vert.norm, aes(x=k.core_norm,y=GDP_pc_2005_norm))+
#   geom_point()+
#   ylab("GDP per capita (10ths $)") + xlab("max. subclique") + theme_bw()
# 
# library(boot)
# # function to obtain regression weights 
# bs <- function(formula, data, indices) {
#   d <- data[indices,] # allows boot to select sample 
#   fit <- lm(formula, data=d)
#   return(coef(fit)) 
# } 
# # bootstrapping with 1000 replications 
# library(tidyverse)
# res<-purrr::map(c("total.exp","GDP_pc_2005","k.core"),function(x,df=vertices.gdp.inc){
#   y=df[[x]]
#   df2<-data.frame((y-mean(y))/sd(y))
#   names(df2)<-paste0(x,"_norm")
#   cbind(df$name,df2)
# }) %>% bind_cols(.)
# res<-res[,c(1,2,4,6)]
# names(res)[1]<-"name"
# vert.norm<-left_join(vertices.gdp.inc,res)
# results <- boot(data=vert.norm, statistic=bs, 
#                 R=1000, formula=(total.exp_norm )~(k.core_norm))
# 
# # view results
# results
# plot(results, index=1) # intercept 
# plot(results, index=2) # wt 
# 
# # get 95% confidence intervals 
# boot.ci(results, type="bca", index=1) # intercept 
# boot.ci(results, type="bca", index=2) # k-core
# 

#dev.off()
### not relevant
#      adj.exgr.2005<-as.matrix()
#      exgr.2005.manufac.graph.net<-asNetwork(exgr.2005.manufac.graph)
#      # components
#      weak.comp<-clusters(exgr.2005.manufac.graph, mode=c("weak"))
#      strong.comp<-clusters(exgr.2005.manufac.graph, "strong")
#      #biconected no articulation points
#      biconnect<-biconnected.components(exgr.2005.manufac.graph.undir)
#      #coreness
#      exgr.2005.manufac.graph$coreness<-coreness(exgr.2005.manufac.graph, mode="all")
#      
#      subgraph<-V(exgr.2005.manufac.graph)[coreness(exgr.2005.manufac.graph)<140]
#      subgraph.2<-V(exgr.2005.manufac.graph)[coreness(exgr.2005.manufac.graph)>=140]
#      
#      low.coreness<-induced_subgraph(exgr.2005.manufac.graph, subgraph)
#      high.coreness<-induced_subgraph(exgr.2005.manufac.graph, subgraph.2)
#      
#      low.coreness$hub.s<-hub_score(low.coreness)$vector
#      high.coreness$hub.s<-hub_score(high.coreness)$vector
#      high.coreness.undir<-as.undirected(high.coreness, mode="mutual")
#      cluster.high.undirect.2005<-cluster_louvain(high.coreness.undir)
#      members.core<-membership(   cluster.high.undirect.2005)
#      png(file = "communities_high_coreness.png", units="in",      width=5,      height=4,      pointsize=12,      res=72)
#      plot(     high.coreness, vertex.color =members.core, vertex.size=0.05*V(high.coreness)$deg.out , layout=layout_with_fr(exgr.2005.manufac.graph),vertex.label.dist = 0.5, vertex.label.cex = 0.7, edge.width = 0.2, edge.arrow.size=0.5,edge.arrow.width=0.5,edge.arrow.mode=0)
#      dev.off()
#      
#      low.coreness.undir<-as.undirected(low.coreness, mode="mutual")
#      cluster.low.undirect.2005<-cluster_louvain(low.coreness.undir)
#      members.core<-membership(   cluster.low.undirect.2005)
#      png(file = "communities_low_coreness.png", units="in",      width=5,      height=4,      pointsize=12,      res=72)
#      plot(     low.coreness, vertex.color =members.core, vertex.size=0.05*V(exgr.2005.manufac.graph)$deg.out , layout=layout_with_fr(exgr.2005.manufac.graph),vertex.label.dist = 0.5, vertex.label.cex = 0.7, edge.width = 0.5, edge.arrow.size=0.5,edge.arrow.width=0.7,edge.arrow.mode=0)
#      
#      svg(file = "low_coreness.png",  width=5, 
#          height=4, 
#          pointsize=12)
#      plot(low.coreness, vertex.size=10*low.coreness$hub.s , layout=layout_with_drl(low.coreness),vertex.label.dist = 0.6, vertex.label.cex = 0.5, edge.arrow.size=0.1, edge.width = 0.5)
#      dev.off()
#      svg(file = "high_coreness.png" units="in",      width=5,      height=4,      pointsize=12,      res=72)
#      plot(high.coreness, vertex.size=10*high.coreness$hub.s , layout=layout_with_drl(high.coreness), vertex.label.dist = 0.6, vertex.label.cex = 0.5, edge.arrow.size=0.1,edge.width = 0.2)
#      dev.off()
#      lay.2<-layout_with_lgl(exgr.2005.manufac.graph, area=vcount(exgr.2005.manufac.graph)^2.5,maxdelta=vcount(exgr.2005.manufac.graph)^4)
#      coreness <- graph.coreness(exgr.2005.manufac.graph)
#      coreness
#      
#     
#      make_k_core_plot(exgr.2005.manufac.graph)
#      cores<-as.data.frame(graph.coreness(exgr.2005.manufac.graph))
#      cores$COU<-rownames(cores)
#      names(cores)<-c("k.core","COU")
#      
#      cores<-arrange(cores,(k.core))
#      cores$cum.rel.freq<-cumsum(cores$k.core)/sum(cores$k.core)
#      # Code based on Jordi Casas-Roma https://jcasasr.wordpress.com/2015/02/03/plotting-the-coreness-of-a-network-with-r-and-igraph/
#      CorenessLayout <- function(g) {
#        coreness <- graph.coreness(g);
#        xy <- array(NA, dim=c(length(coreness), 2));
#        
#        shells <- sort(unique(coreness));
#        for(shell in shells) {
#          v <- 1 - ((shell-1) / max(shells));
#          nodes_in_shell <- sum(coreness==shell);
#          angles <- seq(0,360,(360/nodes_in_shell));
#          angles <- angles[-length(angles)]; # remove last element
#          xy[coreness==shell, 1] <- sin(angles) * v;
#          xy[coreness==shell, 2] <- cos(angles) * v;
#        }
#        return(xy);
#      }
#      # compute coreness
#      coreness <- graph.coreness(exgr.2005.manufac.graph);
#      # assign colors
#     # require(ocean)
#      colbar <- rainbow(max(coreness))
#     
#      # create layout
#      ll <- CorenessLayout(exgr.2005.manufac.graph);
#      
#      plot(exgr.2005.manufac.graph, layout=ll, vertex.size=5, vertex.color=colbar[coreness], vertex.frame.color=colbar[coreness], main='Coreness',  edge.width = 1,
#           edge.arrow.size = 0.1,vertex.label.dist = 0.5, vertex.label.cex = 0.7);
#     svg(file="k-core.png" units="in",      width=5,      height=4,      pointsize=12,      res=72)
#       plot( x=cores$k.core, y=cores$cum.rel.freq, pch=19, cex=1.2,  col="orange", 
#            xlab="k-core", ylab="Cumulative Frequency", main="Cummulative distribution of k-cores")
# dev.off()
#       #betweness     
# V(exgr.2005.manufac.graph)$between.centr<-betweenness(exgr.2005.manufac.graph, directed=T, normalized=F)
#   exgr.2005.manufac.graph$betweness.centralization<-centr_betw(exgr.2005.manufac.graph, directed=T, normalized=F)$centralization
#   V(exgr.2005.manufac.graph)$prestige<-eigen_centrality(exgr.2005.manufac.graph)$vector
#  influence.trade.df<- data.frame(V(exgr.2005.manufac.graph)$name, V(exgr.2005.manufac.graph)$deg.in, V(exgr.2005.manufac.graph)$deg.out, V(exgr.2005.manufac.graph)$closeness.centr.in, V(exgr.2005.manufac.graph)$closeness.centr.out, 
#                                  V(exgr.2005.manufac.graph)$between.centr, V(exgr.2005.manufac.graph)$hub.s, V(exgr.2005.manufac.graph)$authr.s)
#  cbind(head(influence.trade.df[order(-influence.trade.df$deg.out),1],10),head(influence.trade.df[order(-influence.trade.df$deg.in),1],10),head(influence.trade.df[order(-influence.trade.df$between.centr),1],10))
#  influence.trade.df$rel.degree.in<-influence.trade.df$deg.in/(143-1)
#  influence.trade.df$rel.degree.out<-influence.trade.df$deg.out/(143-1)
#  influence.trade.df$name<-as.character(influence.trade.df$name)
#  names(influence.trade.df)<-gsub("V.exgr.2005.manufac.graph..","",names(influence.trade.df))
#  cor(influence.trade.df$between.centr, influence.trade.df$authr.s,method=c("spearman"))
# 
#  graph.export<-graph.data.frame(exports.2005.manufacturing[,c(1:2,9)], vertices=exports.2005.manufacturing.actors)
#  E(graph.export)$weight<-E(graph.export)$value
#  V(graph.export)$id<-V(graph.export)$name
#  closeness.centr.out<-as.data.frame(V(exgr.2005.manufac.graph)$closeness.centr.out)
#  df<-get.data.frame(exgr.2005.manufac.graph,what=c("vertices"))
# 
#  
#  
#  closeness.centr.out
#  names<-as.character(closeness.centr.out)
#  concept<-vertex_attr(g, names)
#    names<-x
# names(df[,1])<-"names"
# 
#  closeness.centr.out$COU<-V(exgr.2005.manufac.graph)$name
#  names(closeness.centr.out)<-c("out.closeness","COU")
#  
#  closeness.centr.out<-arrange(closeness.centr.out,(out.closeness))
#  closeness.centr.out$cum.rel.freq<-cumsum(closeness.centr.out$out.closeness)/sum(closeness.centr.out$out.closeness)
#  #exclude self loops, result is a simple graph
#  exgr.2005.manufac.graph<- simplify(exgr.2005.manufac.graph, remove.loops=TRUE, remove.multiple = F)
#  write.graph(graph.export, file="exgr_manuf.net", format=c("pajek"))
# 
# 
#  #pajek 
#  #hubness and authorities
#  #0.90592
#  #pajek 
#  #betweness and hubs
#  #0.78039
#  #> cor(influence.trade.df$between.centr, influence.trade.df$deg.in,method=c("spearman"))
#  #[1] 0.9282725
#  #> cor(influence.trade.df$between.centr, influence.trade.df$deg.out,method=c("spearman"))
#  #[1] 0.9359279
#  #> cor(influence.trade.df$between.centr, influence.trade.df$hub.s,method=c("spearman"))
#  #[1] 0.9309974
# #> cor(influence.trade.df$between.centr, influence.trade.df$authr.s,method=c("spearman"))
#               #[1] 0.926832
#  #cor(influence.trade.df$deg.out,influence.trade.df$deg.in,method=c("spearman"))
#  #[1] 0.8240246
