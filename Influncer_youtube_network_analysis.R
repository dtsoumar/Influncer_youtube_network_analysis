#------------------------------------------------------------------------------#
#                       Complex Systems and Networks                           #
#------------------------------------------------------------------------------#
library(poweRlaw)
library(igraph)

y<-read.table("com-youtube.ungraph1.txt", header = T)
g<- graph_from_data_frame(y, directed=F, vertices=NULL )
c3 <- rainbow(n = 20, alpha = 0.6)# 

pdf("all_the_net.pdf")
par(bg="black")
plot(g, vertex.size=1, vertex.label=NA, vertex.color=c3[1:20])
dev.off()


summary(g)       # basic characteristics
is_simple(g)     # do loops or / and multiple edges
is.weighted(g)
# Τάξη δικτύου
gorder(g)
# Μέγεθος δικτύου
gsize(g)

####################### Density ###############################
is.connected(g) # the network is connected
edge_density(g)  # 0.000004639259 < 0.25 strongly sparse 
graph.density(g) # 0.000004639259 < 0.25 strongly sparse 
edge.connectivity(g)
vertex.connectivity(g)

################### transitivity ratio #######################
transitivity(g, type="global")            
transitivity(g, type="average")    #smallworldness           
transitivity(g, type="local", isolates= "zero")             

################### Assortativity ############################

assortativity.degree(g, directed = F)  # -0.03690991  -> DISSASSORTATIVE

#Average Nearest neighbor degree (ANND) 
gknn <- knn(g, weights=NA) 
str(gknn) 
gknn$knnk 

## Πλαίσιο δεδομένων με το βαθμό και τον μέσο βαθμό των γειτόνων
annd.df <- data.frame(Degree = 1:max(degree(g)), 
                      ANND = gknn$knnk)
annd.df

par( mar = c(4,4,4,4))
plot(annd.df$Degree, annd.df$ANND, xlab="Degree", ylab="ANND",
     frame=TRUE)                                 ## dissasortative network   




## degree, with higher node 1072, he might be the influencer
deg <- degree(g, mode="all")
head(sort(deg, decreasing = T),20)
summary(degree(g))


hist(deg, main="Histogram of node degree",   col="limegreen")
hist(log(deg), main="Histogram of node degree",   col="limegreen")


#------------------------------------------------------------------------------#
#                                                                              #
######################### DEGREE DISTRIBUTION ##################################
#                                                                              #
#------------------------------------------------------------------------------#

deg_all <- igraph::degree(g)
hist(log(deg_all), main="Histogram of node degree",   col="limegreen", labels = T)


sort(deg_all)
summary(deg_all)
cv.deg_all <- (sd(deg_all)/mean(deg_all)*100)
cv.deg_all                # 50.38219% High value 
range(deg_all)            #   1 - 198

D <- max(unname(deg_all))

dd_all <- igraph::degree.distribution(g, cumulative = FALSE )
dd_all

cdd_all <- igraph::degree.distribution(g, cumulative = TRUE )
cdd_all

par(bg="white",new=FALSE) 
par(mfrow = c(1, 3), mar = c(4,4,4,4))
plot(dd_all, xlab="degree", ylab="freq")
plot(dd_all, xlab="Log degree", ylab="Log freq", log="xy")
plot(cdd_all, xlab="Log degree", ylab="Log CCD", log="xy")

fit_power_law(deg)


plot(dd_all)

x<-degree(g)[degree(g)>8]

x
plot(log(x))
length(x)
y<-c(1:103784)

y
length(y)==length(x)
plot(log(x), log(y))

length(cdd_all)
y<-c(1:28755)
plot(log(cdd_all), log(y))
abline(lm(log(y)~log(cdd_all)),lw=2, col="red")


## Empeirical Hypothesis testing for POWER LAW DISTRIBUTION FITNESS 

dd_all_pl <- displ$new(deg_all)            # fit degree to powerlaw distribution  
est_all_pl <- estimate_xmin(dd_all_pl)     # package "poweRlaw" 
# sligth differences with igraph's
dd_all_pl$setXmin(est_all_pl)              # integrated "fit_power_law" function
# but overall easier and more automatic
# implementation. 
# Both are used interchangeably 

dev.off()
par(mfrow = c(1, 3))
plot(dd_all, xlab="degree", ylab="freq")
lines(dd_all_pl, col="red", lw=2)

plot(dd_all, xlab="Log degree", ylab="Log freq", log="xy")
lines(dd_all_pl, col="red", lw=2)

plot(cdd_all, xlab="Log degree", ylab="Log CCD", log="xy")
lines(dd_all_pl, col="red", lw=2)


dev.off()



###############################################################
deg.dist <- degree_distribution(g, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange",
      xlab="Degree", ylab="Cumulative Frequency")


plot(log(deg.dist))
eigen<-eigen_centrality(g, directed=F, weights=NA)
head(sort(eigen$vector, decreasing = T),20)


hist(log(eigen$vector),                       
     breaks="Sturges",
     col="orange",
     xlab="Degree", 
     ylab="Frequency",
     main=list("Distribution of eigenvector centrality",
               cex=0.6) )



#------------------------------------------------------------------------
## Ego graph of the Influencer
#------------------------------------------------------------------------

# έχουμε order=1 για να βρούμε τους πρώτους γείτονες τοςυ 1072 δηλαδή τους ακόλουθους του
# και mindist=0 ώστε το ego graph να ξεκινάει απο αυτόν
ego(g,
    order = 1,       
    nodes = c( "1072"),
    mode = "all",
    mindist = 0)     
subg_1072<-make_ego_graph(g,
                      order = 1,      
                      nodes = c( "1072"),
                      mode = "all",
                      mindist = 0)

class(subg_1072)

subg_1072[[1]]


deg_subg_1072 <-degree(subg_1072[[1]], mode = "all")
head(sort(deg_subg_1072, decreasing = T),20)

pdf("ego_with_1072.pdf")
par(bg="black")
plot(subg_1072[[1]], layout=layout_with_lgl,
     vertex.size = ifelse(V(g)$name=="1072", 30, 3),
     vertex.color=ifelse(V(g)$name=="1072", "white", c3[1:20]), vertex.label = NA)
dev.off()
# order=1, mindist=1 ώστε να αφαιρέσουμε τον 1072 απο το δίκτυο 


subg_without_1072<-make_ego_graph(g,
                      order = 1,      
                      nodes = c( "1072"),
                      mode = "all",
                      mindist = 1)
subg_without_1072[[1]]

deg_subg_without_1072 <-degree(subg_without_1072[[1]], mode = "all")
head(sort(deg_subg_without_1072, decreasing = T),20)

pdf("ego_without_1072.pdf")
par(bg="black")
plot(subg_without_1072[[1]], 
     vertex.color= c3[1:20], vertex.label = NA, vertex.size=3)
dev.off()


#we remove the 20 vertices with the biggest degree in the network
g_test<-g
while (max(degree(g_test)>3500)) {
  g_test<-delete.vertices(g_test, V(g_test)[degree(g_test)==max(degree(g_test))] )
}

deg_test <- degree(g_test, mode="all")
head(sort(deg_test, decreasing = T),20)
summary(degree(g_test))
hist(deg_test, main="Histogram of node degree",   col="limegreen", labels = T)


g_test3<- delete.edges(g_test, E(g_test))

pdf("gtest3.pdf")
plot(g_test3, vertex.label=NA,  vertex.size=3, vertex.color=c3[1:20])
dev.off()


vcount(g)-vcount(g_test)


#we remove all the followers and keep the 20 vertices with the biggest degrees
g_test2<-g
V(g_test2)$d<-degree(g_test2)
g_test2<-delete.vertices(g_test2, V(g_test2)[degree(g_test2)<3500])


vcount(g_test2)
V(g_test2)[[]]

deg_test2<-degree(g_test2)
head(sort(deg_test2, decreasing = T),20)


pdf("the first 20.pdf")
par(bg="black")
plot(g_test2,
     layout=layout_in_circle,
     vertex.label=ifelse(degree(g_test2)==max(degree(g_test2)),V(g_test2)$name, NA), 
     vertex.size= 30*(V(g_test2)$d)/max(V(g_test2)$d)+10, 
     edge.arrow.size=.5,
     vertex.color= c3[1:20] ,
     vertex.label.cex=2,
     vertex.label.color="white",
     vertex.label.family="sans")
dev.off()     






