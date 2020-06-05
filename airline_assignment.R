install.packages(c("cluster", "factoextra"))
library(cluster)
library(factoextra)
EastWestAirlines <- read_excel("EastWestAirlines.xlsx")
View(EastWestAirlines)
str(EastWestAirlines)
summary(EastWestAirlines)
fviz_nbclust(EastWestAirlines, FUN = hcut, method = "wss")
fviz_nbclust(crime_data, FUN = hcut, method = "silhouette")
#Data normalization
Airline_normalize <- scale(EastWestAirlines)
Airline_normalize
#Dist matrix
D <- dist(Airline_normalize, method = "euclidean")
D
fit <- hclust(D, method="complete")
str(fit)
plot(fit)
plot(fit, cex= 0.6, hang =  -1)
rect.hclust(fit, k=2, border="red")
groups <- cutree(fit, k=2)
table(groups)
fviz_cluster(list(data = Airline_normalize, cluster = groups))
membership<-as.matrix(groups)
plot(membership)
final <- data.frame(EastWestAirlines, membership)
View(final)
aggregate(EastWestAirlines,by=list(final$membership),mean)

#K-means clustering
library(kselection)
k <- kselection(EastWestAirlines, parallel = TRUE, k_threshold = 0.9)
k
Kfit <- kmeans(Airline_normalize, 2)
str(Kfit)
print(Kfit)
table(Kfit$cluster)
final2<- data.frame(EastWestAirlines, Kfit$cluster) # append cluster membership
final2
final3 <- final2[,c(ncol(final2),1:(ncol(final2)))]
final3
aggregate(EastWestAirlines, by=list(Kfit$cluster), FUN=mean)
fviz_cluster(Kfit, data = EastWestAirlines)







