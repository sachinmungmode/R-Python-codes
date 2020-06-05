install.packages(c("cluster", "factoextra"))
library(cluster)
library(factoextra)
crime_data <- read.csv("D:/Assignments/clustering assign/Assignments/Assignments/crime_data.csv")
View(crime_data)
summary(crime_data)
fviz_nbclust(crime_data, FUN = hcut, method = "wss")
fviz_nbclust(crime_data, FUN = hcut, method = "silhouette")
crime_normalized <- scale(crime_data[,2:5])
crime_normalized
D <- dist(crime_normalized, method = "euclidean")
D
fit <- hclust(D, method="complete")
str(fit)
plot(fit)
plot(fit, cex= 0.6, hang =  -1)
rect.hclust(fit, k=2, border="red")
groups <- cutree(fit, k=2) #determines clusters
table(groups)
fviz_cluster(list(data = crime_normalized, cluster = groups))
membership<-as.matrix(groups)
plot(membership)
final <- data.frame(crime_data, membership)
View(final)
aggregate(crime_data[,-1],by=list(final$membership),mean)

# k-means clustering
k <- kselection(crime_data[,-1], parallel = TRUE, k_threshold = 0.6)
k
Kfit <- kmeans(crime_normalized, 2)
str(Kfit)
final2<- data.frame(crime_data, Kfit$cluster) # append cluster membership
final2
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
final3
aggregate(crime_data[,2:5], by=list(Kfit$cluster), FUN=mean)

