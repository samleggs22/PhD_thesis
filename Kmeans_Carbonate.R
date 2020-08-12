# K means clustering #
#install.packages("tidyverse")
#install.packages("cluster")
#install.packages("factoextra")
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

carbclean <- OSr_teeth
class(as.data.frame(carbclean))
head(carbclean)
carbclean <- data.frame(carbclean)
rownames(carbclean) <- carbclean[,1]
carbclean <- carbclean[,-1]
head(carbclean)
View(carbclean)
carbclean<-carbclean[c(23,27)]
head(carbclean)
#carbclean <- na.omit(carbclean)
#head(carbclean)
carbclean <- scale(carbclean)
carbclean <- na.omit(carbclean)
head(carbclean)

carb2 <- kmeans(carbclean, centers = 2, nstart = 25)
str(carb2)
carb2
fviz_cluster(carb2, data = carbclean)+
  xlab(expression(paste(delta^{18},"O")))+ylab(expression(paste(delta^{13},"C")))


carb3 <- kmeans(carbclean, centers = 3, nstart = 25)
carb4 <- kmeans(carbclean, centers = 4, nstart = 25)
carb5 <- kmeans(carbclean, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(carb2, geom = "point", data = carbclean) + ggtitle("k = 2")
p2 <- fviz_cluster(carb3, geom = "point",  data = carbclean) + ggtitle("k = 3")
p3 <- fviz_cluster(carb4, geom = "point",  data = carbclean) + ggtitle("k = 4")
p4 <- fviz_cluster(carb5, geom = "point",  data = carbclean) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2) 

#working out optimal number of clusters
#elbow method
set.seed(123)
fviz_nbclust(carbclean, kmeans, method = "wss") #optimal number looks to be 3?

#optimal number by the silhouette method
set.seed(123)
fviz_nbclust(carbclean, kmeans, method = "silhouette") #this indicates 2 as optimal, but probably actually 4 as 2 is the overall max

#via the gap statistics method
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(carbclean, FUN = kmeans, nstart = 25, K.max = 10, B = 100)
# Print the result (which is 1 cluster)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat) #1 cluster really??? I think maybe 3??
print(gap_stat, method = "globalmax") #this gives 3 
fviz_gap_stat(gap_stat, method="globalmax")
fviz_nbclust(carbclean, kmeans, method = "gap_stat", k.max = 10, nboot = 100) + theme_minimal() + ggtitle("fviz_nbClust_gap_stat: Gap Statistic")

# optimal number of clusters varies but 3 seems to be likely
fviz_cluster(carb3, geom = "point",  data = carbclean) + ggtitle("k = 3")


#printing results to get cluster members#
carb3

#when printing look at % who is "wrong" cluster - see if I can export this into a csv and check the OSR nos against countries/region

#now to try this for just England 

Engcarbclean<- OSr_teeth
Engcarbclean<- subset(Engcarbclean, Country=="England")
class(as.data.frame(Engcarbclean))
head(Engcarbclean)
Engcarbclean <- data.frame(Engcarbclean)
rownames(Engcarbclean) <- Engcarbclean[,1]
Engcarbclean <- Engcarbclean[,-1]
head(Engcarbclean)
Engcarbclean<-Engcarbclean[c(23,24)]
head(Engcarbclean)


Engcarbclean <- na.omit(Engcarbclean)
head(Engcarbclean)
Engcarbclean <- scale(Engcarbclean)


Engcarb2 <- kmeans(Engcarbclean, centers = 2, nstart = 25)
str(Engcarb2)
Engcarb2
fviz_cluster(Engcarb2, data = Engcarbclean)+
  xlab(expression(paste(delta^{18},"O")))+ylab(expression(paste(delta^{13},"C")))


Engcarb3 <- kmeans(Engcarbclean, centers = 3, nstart = 25)
Engcarb4 <- kmeans(Engcarbclean, centers = 4, nstart = 25)
Engcarb5 <- kmeans(Engcarbclean, centers = 5, nstart = 25)

# plots to compare
Enp1 <- fviz_cluster(Engcarb2, geom = "point", data = Engcarbclean) + ggtitle("k = 2")
Enp2 <- fviz_cluster(Engcarb3, geom = "point",  data = Engcarbclean) + ggtitle("k = 3")
Enp3 <- fviz_cluster(Engcarb4, geom = "point",  data = Engcarbclean) + ggtitle("k = 4")
Enp4 <- fviz_cluster(Engcarb5, geom = "point",  data = Engcarbclean) + ggtitle("k = 5")

grid.arrange(Enp1, Enp2, Enp3, Enp4, nrow = 2)

#working out optimal number of clusters
#elbow method
set.seed(123)
fviz_nbclust(Engcarbclean, kmeans, method = "wss") #optimal number looks to be 4 or 5?

#optimal number by the silhouette method
set.seed(123)
fviz_nbclust(Engcarbclean, kmeans, method = "silhouette") #this indicates 3 as optimal

#via the gap statistics method
# compute gap statistic
set.seed(123)
gap_statEng <- clusGap(Engcarbclean, FUN = kmeans, nstart = 25, K.max = 10, B = 80)
# Print the result (which is 1 cluster)
print(gap_statEng, method = "firstmax")
fviz_gap_stat(gap_statEng) #1 cluster really??? I think maybe 3 or 4?? #check out gapstat methods is "firstmax" always going to give k=1???? 
print(gap_stat, method = "globalmax") #this gives 3
fviz_gap_stat(gap_stat, method="globalmax")

# optimal number of clusters varies but I'd say 3 or 4
grid.arrange(Enp2, Enp3, nrow = 1)
grid.arrange(Enp1, Enp2, Enp3, nrow = 1)

#printing results to get cluster members#
Engcarb2
Engcarb3
Engcarb4

#should I try kmeans on just the d18O dataset since its larger? And/or try it all again with different combos e.g. O&Sr, d18O and bigdeltaMAP?


#For Kent
class(as.data.frame(kent_carb))
head(kent_carb)
kentcarbclean <- data.frame(kent_carb)
rownames(kentcarbclean) <- kentcarbclean[,1]
kentcarbclean <- kentcarbclean[,-1]
head(kentcarbclean)
kentcarbclean<-kentcarbclean[c(23,24)]
head(kentcarbclean)


kentcarbclean <- na.omit(kentcarbclean)
head(kentcarbclean)
kentcarbclean <- scale(kentcarbclean)


kentcarb2 <- kmeans(kentcarbclean, centers = 2, nstart = 25)
str(kentcarb2)
kentcarb2
fviz_cluster(kentcarb2, data = kentcarbclean)+
  xlab(expression(paste(delta^{18},"O")))+ylab(expression(paste(delta^{13},"C")))


kentcarb3 <- kmeans(kentcarbclean, centers = 3, nstart = 25)
kentcarb4 <- kmeans(kentcarbclean, centers = 4, nstart = 25)
kentcarb5 <- kmeans(kentcarbclean, centers = 5, nstart = 25)

# plots to compare
kentp1 <- fviz_cluster(kentcarb2, geom = "point", data = kentcarbclean) + ggtitle("k = 2")
kentp2 <- fviz_cluster(kentcarb3, geom = "point",  data = kentcarbclean) + ggtitle("k = 3")
kentp3 <- fviz_cluster(kentcarb4, geom = "point",  data = kentcarbclean) + ggtitle("k = 4")
kentp4 <- fviz_cluster(kentcarb5, geom = "point",  data = kentcarbclean) + ggtitle("k = 5")

grid.arrange(kentp1, kentp2, kentp3, kentp4, nrow = 2)

#working out optimal number of clusters
#elbow method
set.seed(123)
fviz_nbclust(kentcarbclean, kmeans, method = "wss") #optimal number looks to be 4

#optimal number by the silhouette method
set.seed(123)
fviz_nbclust(kentcarbclean, kmeans, method = "silhouette") #this indicates 4 as optimal

#via the gap statistics method
# compute gap statistic
set.seed(123)
gap_statkent <- clusGap(kentcarbclean, FUN = kmeans, nstart = 25, K.max = 10, B = 80)
# Print the result (which is 1 cluster)
print(gap_statkent, method = "firstmax") 
fviz_gap_stat(gap_statkent) #2 clusters but prints the line at 1? not sure why??
print(gap_stat, method = "globalmax") #this gives 3
fviz_gap_stat(gap_stat, method="globalmax")

# optimal number of clusters varies but I'd say 3 or 4
grid.arrange(kentp2, kentp3, nrow = 1)

#printing results to get cluster members#
kentp2
kentp3

#For Cambridgeshire
cambs_carb
class(as.data.frame(cambs_carb))
head(cambs_carb)
cambscarbclean <- data.frame(cambs_carb)
rownames(cambscarbclean) <- cambscarbclean[,1]
cambscarbclean <- cambscarbclean[,-1]
head(cambscarbclean)
cambscarbclean<-cambscarbclean[c(23,24)]
head(cambscarbclean)


cambscarbclean <- na.omit(cambscarbclean)
head(cambscarbclean)
cambscarbclean <- scale(cambscarbclean)


cambscarb2 <- kmeans(cambscarbclean, centers = 2, nstart = 25)
str(cambscarb2)
cambscarb2
fviz_cluster(cambscarb2, data = cambscarbclean)+
  xlab(expression(paste(delta^{18},"O")))+ylab(expression(paste(delta^{13},"C")))


cambscarb3 <- kmeans(cambscarbclean, centers = 3, nstart = 25)
cambscarb4 <- kmeans(cambscarbclean, centers = 4, nstart = 25)
cambscarb5 <- kmeans(cambscarbclean, centers = 5, nstart = 25)

# plots to compare
cambsp1 <- fviz_cluster(cambscarb2, geom = "point", data = cambscarbclean) + ggtitle("k = 2")
cambsp2 <- fviz_cluster(cambscarb3, geom = "point",  data = cambscarbclean) + ggtitle("k = 3")
cambsp3 <- fviz_cluster(cambscarb4, geom = "point",  data = cambscarbclean) + ggtitle("k = 4")
cambsp4 <- fviz_cluster(cambscarb5, geom = "point",  data = cambscarbclean) + ggtitle("k = 5")

grid.arrange(cambsp1, cambsp2, cambsp3, cambsp4, nrow = 2)

#working out optimal number of clusters
#elbow method
set.seed(123)
fviz_nbclust(cambscarbclean, kmeans, method = "wss") #optimal number looks to be 4 or 5?

#optimal number by the silhouette method
set.seed(123)
fviz_nbclust(cambscarbclean, kmeans, method = "silhouette") #this indicates 7 as optimal??? Sure;y they'd be too small? 

#via the gap statistics method
# compute gap statistic
set.seed(123)
gap_statkent <- clusGap(cambscarbclean, FUN = kmeans, nstart = 25, K.max = 10, B = 80)
# Print the result (which is 1 cluster)
print(gap_statkent, method = "firstmax") 
fviz_gap_stat(gap_statkent) #1 clusters yet again... 
print(gap_stat, method = "globalmax") #this gives 3
fviz_gap_stat(gap_stat, method="globalmax")

# optimal number of clusters varies massively... 
grid.arrange( nrow = 1)

#printing results to get cluster members#
#retrying with region rather than county




