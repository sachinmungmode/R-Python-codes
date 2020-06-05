library(arules)
library(arulesViz)
mymovies <- read.csv(file.choose())
View(mymovies)
summary(mymovies)

### apriori algorithm
movie_rules <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.3, confidence = 0.5,minlen=5)))
movie_rules
inspect(head(sort(movie_rules, by = "lift"))) 
head(quality(movie_rules))

###visualization of rules
plot(movie_rules,method = "scatterplot")
plot(movie_rules, method = "grouped")
plot(movie_rules,method = "graph")
