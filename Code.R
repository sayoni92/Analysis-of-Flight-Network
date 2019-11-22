library(igraph)
install.packages("geosphere")
install.packages("maps")
install.packages("tnet")
require(geosphere)
require(maps)
require(tnet)

OFairports <- read.csv("C:/Users/chatt/OneDrive/Desktop/564 Project/airports.dat", header=FALSE, stringsAsFactors=FALSE)
dimnames(OFairports)[[2]] <- c("Airport ID", "Name", "City", "Country", "IATA/FAA", "ICAO", "Latitude", "Longitude", "Altitude", "Timezone", "DST","TZ_DB", "Transport_type","Data_source")

OFairports<-OFairports[c(1:11)]
dim(OFairports)


dim(OFairports)



OF <- read.csv("C:/Users/chatt/OneDrive/Desktop/564 Project/routes.dat", header=FALSE, stringsAsFactors=FALSE)
dimnames(OF)[[2]] <- c("Airline", "Airline ID", "Source airport", "Source airport ID", "Destination airport", "Destination airport ID", "Codeshare", "Stops", "Equipment")
dim(OF)

net2 <- OF[OF[,"Codeshare"]=="",c("Source airport ID", "Destination airport ID")]

# Take out routes from an airport to itself and the missing cases (~1%)
net2 <- net2[net2[,"Source airport ID"]!=net2[,"Destination airport ID"],]
net2 <- net2[net2[,"Source airport ID"]!="\\N",]
net2 <- net2[net2[,"Destination airport ID"]!="\\N",]

# As passengers per route is not available, create a weighted network with the weight equal to number of routes
net2 <- data.frame(i=as.integer(net2[,"Source airport ID"]), j=as.integer(net2[,"Destination airport ID"]))
dim(net2)
net2 <- shrink_to_weighted_network(net2)


#####################################
## Analyse the OpenFlights network ##
#####################################


#basic plot
colbar = rainbow(length(net2)) ## we are selecting different colors to correspond to each word
V(g_valued)$color = colbar
# Set layout here 
L = layout_with_fr(g_valued)  # Fruchterman Reingold
plot(g_valued,vertex.color=V(g_valued)$color, layout = L, edge.arrow.size=.3) 

#L = layout_with_dh(g) ## Davidson and Harel

# L = layout_with_drl(g) ## Force-directed

 
L = layout_with_kk(g) ## Spring

g<-graph.data.frame(net2[c(1,2)],directed = TRUE,vertices= NULL)
#plot.igraph(g)
ecount(g)
vcount(g)
is.simple(g)
is.connected(g)
#plot.igraph(g,edge.arrow.size=.3,vertex.label=OFairports$`IATA/FAA`, layout=L)


numVertices <- vcount(g)
numVertices
numEdges <- ecount(g)
numEdges

maxEdges <- numVertices*(numVertices-1)/2
maxEdges

edge_density(g)
graphDensity <- numEdges/maxEdges # manual calculation
graphDensity
graphDensity1 <- graph.density(g) # using the graph.density function from igraph
graphDensity1


#Degree Centrality================================================================

V(g)$name[degree(g)==max(degree(g,mode="in"))]
V(g)$name[degree(g)==max(degree(g,mode="out"))]
V(g)$name[degree(g)==max(degree(g,mode="all"))]

V(g)$name[degree(g)==max(degree(g))]

dc_all <- degree(g,mode="all")
head(dc_all)
#head(totalDegree)
dc_100<-sort(dc_all,decreasing = TRUE)[1:30]
m_subset<-subset(OFairports,OFairports$`Airport ID`==1602)
m_subset

write.csv(file="C:/Users/chatt/OneDrive/Desktop/564 Project/dc_100.csv", x=dc_100, col.names = c("ID","Degree_centrality"))
degree_all<-read.csv(file="C:/Users/chatt/OneDrive/Desktop/564 Project/dc_100.csv", header = TRUE)
head(degree_all)
nrow(degr)
airport_degree<-data.frame(matrix(ncol = 4, nrow = 0))
col_id<- c("id", "name", "city","country")
colnames(airport_degree)<-col_id
for(i in 1:30){
  
  print(i)

  d_subset<-subset(OFairports,OFairports$`Airport ID`==degree_all[i,1])

  airport_degree[i,1]<-d_subset$`Airport ID`
  airport_degree[i,2]<-d_subset$Name
  airport_degree[i,3]<-d_subset$City
  airport_degree[i,4]<-d_subset$Country
}

head(airport_degree)
a<-cbind(airport_degree,degree_all)

head(sort(airport_degree$id))

#Weighted Degree Centrality
g_w<-graph.strength(g,weights = net2$w)
sort(g_w,decreasing=TRUE)[1:30]

dcw_100<-sort(g_w,decreasing = TRUE)[1:30]

write.csv(file="C:/Users/chatt/OneDrive/Desktop/564 Project/dcw_100.csv", x=dcw_100)
degree_all_w<-read.csv(file="C:/Users/chatt/OneDrive/Desktop/564 Project/dcw_100.csv", header = TRUE)
head(degree_all_w)


head(degree_all)
airport_degree_w<-data.frame(matrix(ncol = 4, nrow = 0))
col_id<- c("id", "name", "city","country")
colnames(airport_degree_w)<-col_id
for(i in 1:30){
  
  print(i)

  d_subset_w<-subset(OFairports,OFairports$`Airport ID`==degree_all_w[i,1])
  
  airport_degree_w[i,1]<-d_subset_w$`Airport ID`
  airport_degree_w[i,2]<-d_subset_w$Name
  airport_degree_w[i,3]<-d_subset_w$City
  airport_degree_w[i,4]<-d_subset_w$Country
}


V(G)$name[degree(G)==max(degree(G))]

axc<-cbind(airport_degree_w,degree_all_w)
axc

#==== Clustering
cluster <- cluster_walktrap(g)
head(cluster)
nrow(cluster)

tail(cluster)


xyz<-subset(OFairports,OFairports$`Airport ID`==5840)
plot(xyz)
# Clustering
transitivity(g)
transitivity(g,, weights = net2$w)


# Find the number of clusters
membership(cluster)   # affiliation list
mc_100<-sort(membership(cluster),decreasing=TRUE)
head(mc_100)

mc_100<-sort(mc,decreasing = TRUE)[1:100]

write.csv(file="C:/Users/chatt/OneDrive/Desktop/564 Project/mc_100.csv", x=mc_100)
cluster_all<-read.csv(file="C:/Users/chatt/OneDrive/Desktop/564 Project/mc_100.csv", header = TRUE)
head(cluster_all)

airport_cluster<-data.frame(matrix(ncol = 4, nrow = 0))
col_id<- c("id", "name", "city","country")
colnames(airport_cluster)<-col_id
for(i in 97:100){

  print(i)

  c_subset<-subset(airport_cluster,is.na(airport_cluster$City))
  sum(is.na(airport_cluster$city))
  
  
  airport_cluster[i,1]<-c_subset$`Airport ID`
  airport_cluster[i,2]<-c_subset$Name
  airport_cluster[i,3]<-c_subset$City
  airport_cluster[i,4]<-c_subset$Country
  airport_cluster
}
head(airport_cluster)



na_list<-subset(airport_cluster,airport_cluster$name==NA)


length(sizes(cluster)) # number of clusters
# Find the size the each cluster 
# Note that communities with one node are isolates, or have only a single tie
sizes(cluster) 


plot(cluster, g, col = V(g)$color,edge.arrow.size=.3,layout = layout_with_kk(g))





#========= Power Laws
deg <- degree(g,v=V(g), mode="all")
deg

# Degree distribution is the cumulative frequency of nodes with a given degree
deg_distr <-degree.distribution(g, cumulative=T, mode="all")
deg_distr
plot(deg_distr, ylim=c(.01,1), bg="black",pch=21, xlab="Degree", ylab="Cumulative Frequency") #You may need to adjust the ylim to a larger or smaller number to make the graph show more data.




power <- power.law.fit(deg_distr)
power
plot(deg_distr, log="xy", ylim=c(.01,2), bg="black",pch=21, xlab="Degree", ylab="Cumulative Frequency")



#===== Small world
# Average clustering coefficient (ACC)
transitivity(g, type = c("average"))

# Characteristic path length (CPL)
average.path.length(g)

accSum <- 0
cplSum <- 0
for (i in 1:100){
  grph <- erdos.renyi.game(numVertices, numEdges, type = "gnm")
  accSum <- accSum + transitivity(grph, type = c("average"))
  cplSum <- cplSum + average.path.length(grph)
}
accSum/100
cplSum/100






#edge weights
E(g_OF)$weight <-net2$w

#Degree
totalDegree <- degree(g_OF,mode="all")
V(g_OF)$size<-totalDegree/10000
plot(g_OF,layout=layout_with_fr(g_OF),edge.width=g_OF$weight)



# Calculate binary and weighted betweenness (on the directed network, net2)
tmp0 <- betweenness_w(net2, alpha=0)
dim(tmp0)
tmp1 <- betweenness_w(net2, alpha=1)
?betweenness_w
x <- 30
out <- data.frame(
  tmp0[order(-tmp0[,"betweenness"]),][1:x,],
  tmp1[order(-tmp1[,"betweenness"]),][1:x,])

# Create output object with top x airports
out <- data.frame(out,
                  tmp0[order(-tmp0[,"betweenness"]),][1:x,],
                  tmp1[order(-tmp1[,"betweenness"]),][1:x,])
dimnames(out)[[2]][5:8] <- c("OF.bb.node", "OF.bb.score", "OF.wb.node", "OF.wb.score")














for(i in 1:x) {
  # Insert label of airport ID (binary)
  tmp2 <- OFairports[OFairports[,"Airport ID"]==out[i,"OF.bb.node"],]
  out[i,"OF.bb.node"] <- paste(tmp2["IATA/FAA"], " (", tmp2["City"], ", ", tmp2["Country"], ")", sep="")
  # Insert label of airport ID (weighted)
  tmp2 <- OFairports[OFairports[,"Airport ID"]==out[i,"OF.wb.node"],]
  out[i,"OF.wb.node"] <- paste(tmp2["IATA/FAA"], " (", tmp2["City"], ", ", tmp2["Country"], ")", sep="")
}
head(out)

###########################
## Comparing FRA and LHR ##
###########################

# Get FRA and LHR's airport ids
ids <- sapply(c("CDG", "LHR"), function(a) OFairports[OFairports[,"IATA/FAA"]==a,"Airport ID"])
# Rank and Score of FRA
tmp1 <- as.data.frame(tmp1[order(-tmp1[,"betweenness"]),])
tmp1[tmp1[,"node"]==ids["CDG"],]
# Degree and Node strength
tmp3 <- degree_w(net2)
tmp3[ids,]
# Weight distribution
sapply(ids, function(a) table(net2[net2[,"i"]==a,3]))
# Airports with strong ties (w>=4)
tmp4 <- lapply(ids, function(a) data.frame(net2[net2[,"i"]==a & net2[,"w"]>=4,], label="", geo.dist=NaN, stringsAsFactors=FALSE))
# Insert labels
for(a in 1:2) {
  for(b in 1:nrow(tmp4[[a]])) {
    tmp2 <- OFairports[OFairports[,"Airport ID"]==tmp4[[a]][b,"j"],][1,]
    tmp4[[a]][b, "label"] <- paste(tmp2["IATA/FAA"], " (", tmp2["City"], ", ", tmp2["Country"], ")", sep="")
  }
}
# Geographical distance
for(a in 1:2) {
  tmp5 <- as.numeric(OFairports[OFairports["Airport ID"]==ids[a],c("Longitude","Latitude")][1,])
  for(b in 1:nrow(tmp4[[a]])) {
    tmp6 <- as.numeric(OFairports[OFairports["Airport ID"]==tmp4[[a]][b,"j"],c("Longitude","Latitude")][1,])
    tmp4[[a]][b, "geo.dist"] <- 6378.7 * acos((sin(tmp5[2]/(180/pi))*sin(tmp6[2]/(180/pi)))+(cos(tmp5[2]/(180/pi))*cos(tmp6[2]/(180/pi))*cos(tmp5[1]/(180/pi)-tmp6[1]/(180/pi))))
  }
}
sapply(1:2, function(a) mean(tmp4[[a]][,"geo.dist"]))
sapply(1:2, function(a) sum(tmp4[[a]][,"geo.dist"]))




#everything done in class
net_2_g<-net2[c(1,2)]
head(net_2_g)
g<-graph.data.frame(net_2_g,directed = TRUE)
plot.igraph(g)
ecount(g)
vcount(g)
is.simple(g)
is.connected(g)
plot.igraph(g,edge.arrow.size=.3,vertex.label=OFairports$City, layout=layout_with_kk(g))


write.csv(file="C:/Users/chatt/OneDrive/Desktop/564 Project/Edges_complete.csv", x=net_2_g)
write.csv(file="C:/Users/chatt/OneDrive/Desktop/564 Project/nodes_complete.csv", x=OFairports[c(1,3)])

write.csv(file="C:/Users/chatt/OneDrive/Desktop/564 Project/nodes_world.csv",x=OFairports[c(1,3,7,8)])



##################################
## Plot the OpenFlights network ##
##################################

# Symmetrise data for visualisationnet2s <- as.data.frame(symmetrise_w(net2, method="SUM"))
dim(net2s)
net2s <- net2s[net2s[,"i"]<net2s[,"j"],]

# Sort data so that weak ties are plotted first
net2s <- net2s[order(net2s[,"w"]),]
head(net2s)

# Set up world map and colors for lines

pdf("C:/Users/chatt/OneDrive/Desktop/564 Project/final.pdf", width=11, height=7)
map("world", col="#eeeeee", fill=TRUE, bg="white", lwd=0.05)
pal <- colorRampPalette(c("#cccccc", "black"))
colors <- pal(length(unique(net2s[,"w"])))
colors <- rep(colors, times=as.integer(table(net2s[,"w"])))

# Plot ties
for(i in 1:nrow(net2s)) {
  # Get longitude and latitude of the two airports
  print(i)
  #i=1
  aid<-net2s[i,"i"]
  asubset<-subset(OFairports,OFairports$`Airport ID`==aid)
  tmp1<-as.numeric(c(asubset$Longitude,asubset$Latitude))
  
  
  bid<-net2s[i,"j"]
  bsubset<-subset(OFairports,OFairports$`Airport ID`==bid)
  tmp2<-as.numeric(c(bsubset$Longitude,bsubset$Latitude))
  
  
  # Get the geographical distance to see how many points on the Great Circle to plot
  a<-cos(tmp1[1]/(180/pi)-tmp2[1]/(180/pi))
  b<-cos(tmp2[2]/(180/pi))
  
  d<-(tmp1[2]/(180/pi))
  e<-(tmp2[2]/(180/pi))
  f<-(tmp1[2]/(180/pi))
  
  tmp3 <- 10*ceiling(as.numeric(log(3963.1 * acos((sin(f)*sin(e))+(cos(d)*b*a) ))))
  c<-round(tmp3)
  # Line coordinates
  inter <- gcIntermediate(p1=tmp1, p2=tmp2, n=c, addStartEnd=TRUE, breakAtDateLine=TRUE)
  
  if(is.matrix(inter)) {
    lines(inter, col=colors[i], lwd=0.6)
  } else {
    for(j in 1:length(inter))
      lines(inter[[j]], col=colors[i], lwd=0.6)
  }
}
dev.off()


g_matrix<-out[c(1,3)]
g1<-graph.data.frame(g_matrix,directed = TRUE)
plot.igraph(g1)
ecount(g1)
vcount(g1)
is.simple(g1)
is.connected(g1)
g1_simpl<-simplify(g1, edge.attr.comb="sum",remove.multiple = T, remove.loops = T)
plot.igraph(g1_simpl,edge.arrow.size =1,vertex.label=OFairports$City, layout=layout.fruchterman.reingold(g1))

write.csv(file="C:/Users/chatt/OneDrive/Desktop/564 Project/Edges.csv", x=g_matrix)