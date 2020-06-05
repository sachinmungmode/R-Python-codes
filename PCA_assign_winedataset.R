library(cluster)
library(factoextra)
library(NbClust)
wine <- read.csv("D:/Assignments/PCA/wine.csv")
View(wine)
str(wine)
cor(wine)
# building PCA object
wine_pca <- princomp(wine[,-1],cor = TRUE, scores = TRUE, covmat = NULL)
summary(wine_pca)
screeplot(wine_pca, type = "lines")
abline(h=1)
loadings(wine_pca)
plot(wine_pca)
biplot(wine_pca)
plot(cumsum(wine_pca$sdev*wine_pca$sdev)*100/(sum(wine_pca$sdev*wine_pca$sdev)),type="b")
wine_pca$scores[,1:3]
# binding wine dataset and pca scores
winedata <- cbind(wine,wine_pca$scores[,1:3])
View(winedata)

wine_clust <- winedata[,15:17]
norm_wine_clust <- scale(wine_clust)
dist1 <- dist(norm_wine_clust, method = "euclidean")

# clustering using hierarchial
fit_hier <- hclust(dist1, method = "complete")
plot(fit_hier)

# cluster analysis- all variables
no_of_Clusters = NbClust(wine, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")
fviz_nbclust(no_of_Clusters) + theme_minimal()

# Hierarchical clustering - All Variables
hclust.complete = eclust(wine,"hclust",k = 7,method = "complete", graph = FALSE) 
fviz_dend(hclust.complete, rect = TRUE, show_labels = FALSE) 

# K-Means clustering - All Variables
km_7 = eclust(wine, "kmeans",k = 7, nstart = 25, graph = FALSE)
fviz_cluster(km_7, geom = "point", frame.type = "norm")

# Cluster Analysis - PCA Components
winedf.pca = winedata[,2:14]
no_of_Clusters = NbClust(winedf.pca,distance = "euclidean",min.nc = 2,max.nc = 10,method = "complete",index ="all")
fviz_nbclust(no_of_Clusters) + theme_minimal()

# Hierarchical clustering - PCA Components
hclust.complete = eclust(winedf.pca, "hclust", k = 7, method = "complete", graph = FALSE) 
fviz_dend(hclust.complete, rect = TRUE, show_labels = FALSE) 

# K-Means clustering - PCA Components
km_7 = eclust(winedf.pca, "kmeans", k = 7, nstart = 25, graph = FALSE)
fviz_cluster(km_7, geom = "point", frame.type = "norm")
