# K means clustering Kent v. 2#
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(readxl)
UKAS2019_Kent_carb <- read_excel("Dropbox/PhD Cantab/RStudio Files/UKAS2019_Kent_carb.xlsx")
#made a new version with environments, sex and gender as coded (couldn't work out how to do it in the data.frame as.numeric so did it in excel)
# Environments - Coastal = 1, Inland = 2, Riverine = 3
# Sex - F=1 F?-2 M=3 M?=4 U=5
# Gender - F=1 M=2 M?=3 U=4
kentclean2<- UKAS2019_Kent_carb
class(as.data.frame(kentclean2))
head(kentclean2)
kentclean2 <- data.frame(kentclean2) #realised while doing PCAs etc. that all the different oxygen measurements were pulling the groups, so eliminating those to just measured carb PDB since ringlemere gets lost later
kentclean2 <- kentclean2[,c(-7,-9,-10,-11,-12,-13,-14,-15, -20, -21, -22, -23, -24, -25)]
head(kentclean2)
rownames(kentclean2) <- kentclean2[,2]
kentclean2 <- kentclean2[,-2]
head(kentclean2)
kentclean2 <- na.omit(kentclean2) #gets rid of Ringlemere
kentclean3 <- kentclean2[,-1]
head(kentclean3)
#kentclean2 <- scale(kentclean2)

kentv2 <- kmeans(kentclean3, centers = 2, nstart = 40)
str(kentv2)
kentv2
fviz_cluster(kentv2, data = kentclean3) 

#working out optimal number of clusters
#elbow method
set.seed(123)
fviz_nbclust(kentclean3, kmeans, method = "wss") #optimal number looks to be 2

#optimal number by the silhouette method
set.seed(123)
fviz_nbclust(kentclean3, kmeans, method = "silhouette") #this indicates 2 as optimal

#via the gap statistics method
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(kentclean3, FUN = kmeans, nstart = 25, K.max = 10, B = 80)
# Print the result (which is 10 clusters)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat) 

#install.packages(rlang)
#install.packages("devtools")
library(devtools)
#devtools::install_github("r-lib/rlang", build_vignettes = TRUE)


kentv3 <- kmeans(kentclean3, centers = 3, nstart = 25)
kentv4 <- kmeans(kentclean3, centers = 4, nstart = 25)
kentv5 <- kmeans(kentclean3, centers = 5, nstart = 25)
kentv6 <- kmeans(kentclean3, centers = 6, nstart = 25)
kentv7 <- kmeans(kentclean3, centers = 7, nstart = 25)
kentv10 <- kmeans(kentclean3, centers = 10, nstart = 25)

# plots to compare
kp1 <- fviz_cluster(kentv2, geom = "point", data = kentclean3) + ggtitle("k = 2")
kp2 <- fviz_cluster(kentv3, geom = "point",  data = kentclean3) + ggtitle("k = 3")
kp3 <- fviz_cluster(kentv4, geom = "point",  data = kentclean3) + ggtitle("k = 4")
kp4 <- fviz_cluster(kentv5, geom = "point",  data = kentclean3) + ggtitle("k = 5")
kp5 <- fviz_cluster(kentv6, geom = "point",  data = kentclean3) + ggtitle("k = 6")
kp6 <- fviz_cluster(kentv7, geom = "point",  data = kentclean3) + ggtitle("k = 7")
kp9 <- fviz_cluster(kentv10, geom = "point",  data = kentclean3) + ggtitle("k = 10")

library(gridExtra)
grid.arrange(kp1, kp2, kp3, kp4, kp5, kp6, nrow = 3)
grid.arrange(kp1,kp9, nrow=2)


#printing kent2 results to get cluster members#
kentv2
kentv10

#PCA for Kent 
library(FactoMineR)
library(factoextra)
install.packages("corrplot")
library(corrplot)
install_github("vqv/ggbiplot")

kentpca<-kent_oxy
kentpca<-kent_oxy[,c(1,3,4,5,6,8,18,19,20,21,22,23,24,25,27)]
kentpca<-na.omit(kentpca)
kentpca<-kentpca[,-6]
kentpca<-kentpca[,c(-8,-9, -10,-11)]
head(kentpca)
kentpca.2<-kentpca[,-1,]
kentpca.2<-kentpca.2[,c(-1,-2,-3)]
kentpca.2<-kentpca.2[,c(-4,-5)]
head(kentpca.2)


res.pca <- prcomp(kentpca.2, scale = TRUE)
fviz_pca_ind(res.pca,  label="none", habillage=as.factor(kentpca$Site),addEllipses = TRUE, legend.title = "Cemetery")
fviz_pca_biplot(res.pca, repel = TRUE)

#PCA version 2 with re-done kent data as above for k means

res.pca <- prcomp(kentclean3, scale = TRUE)
fviz_pca_ind(res.pca,  label="none", habillage=as.factor(kentclean2$Site),addEllipses = TRUE, legend.title = "Cemetery")
fviz_pca_biplot(res.pca, repel = TRUE)
plot(res.pca, choix = "var") #not working not sure why?
res.pca$eig
fviz_pca_var(res.pca) # this works! but with error messages still?
fviz_pca_biplot(res.pca, habillage = kentclean2$Site, addEllipses = TRUE, legend.title = "Cemetery", col.var = "red", alpha.var ="cos2", label = "var") + scale_color_brewer(palette="Dark2")+
  theme_minimal()

#kmeans and PCA with just the carbonate, environment and osteological data
kentclean_justcarb <- kentclean3[,c(-1,-2,-3,-4,-7,-10)]
head(kentclean_justcarb)

#working out optimal number of clusters
#elbow method
set.seed(123)
fviz_nbclust(kentclean_justcarb, kmeans, method = "wss") #optimal number looks to be 2-4?

#optimal number by the silhouette method
set.seed(123)
fviz_nbclust(kentclean_justcarb, kmeans, method = "silhouette") #this indicates 8 as optimal

#via the gap statistics method
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(kentclean_justcarb, FUN = kmeans, nstart = 25, K.max = 20, B = 80)
# Print the result (which is 9 clusters)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat) 


carbclustkent2 <- kmeans(kentclean_justcarb, centers = 2, nstart = 25)
carbclustkent3 <- kmeans(kentclean_justcarb, centers = 3, nstart = 25)
carbclustkent4 <- kmeans(kentclean_justcarb, centers = 4, nstart = 25)
carbclustkent8 <- kmeans(kentclean_justcarb, centers = 8, nstart = 25)
carbclustkent9<- kmeans(kentclean_justcarb, centers = 9, nstart = 25)


# plots to compare
cck2 <- fviz_cluster(carbclustkent2, geom = "point", data = kentclean_justcarb) + ggtitle("k = 2")
cck3 <- fviz_cluster(carbclustkent3, geom = "point",  data = kentclean_justcarb) + ggtitle("k = 3")
cck4 <- fviz_cluster(carbclustkent4, geom = "point",  data = kentclean_justcarb) + ggtitle("k = 4")
cck8 <- fviz_cluster(carbclustkent8, geom = "point",  data = kentclean_justcarb) + ggtitle("k = 8")
cck9 <- fviz_cluster(carbclustkent9, geom = "point",  data = kentclean_justcarb) + ggtitle("k = 9")


library(gridExtra)
grid.arrange(cck2, cck3, cck4, cck8, cck9, nrow = 3)



#printing  results to get cluster members#


justcarbkent.pca <- prcomp(kentclean_justcarb, scale = TRUE)
fviz_pca_ind(justcarbkent.pca,  label="none", habillage=as.factor(kentclean2$Site),addEllipses = TRUE, legend.title = "Cemetery")
fviz_pca_var(res.pca) 
fviz_pca_biplot(justcarbkent.pca, habillage = kentclean2$Site, addEllipses = TRUE, legend.title = "Cemetery", col.var = "red", alpha.var ="cos2", label = "var") + scale_color_brewer(palette="Dark2")+
  theme_minimal()




