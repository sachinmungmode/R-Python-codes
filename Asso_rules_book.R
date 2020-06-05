library(arules)
library(arulesViz)
book <- read.csv(file.choose())
View(book)
summary(book)

### apriori algorithm
book_rules <- apriori(as.matrix(book),parameter=list(support=0.03, confidence = 0.5,minlen=5))
book_rules
inspect(head(sort(book_rules, by = "lift")))  
head(quality(book_rules))

###visualization of rules
plot(book_rules,method = "scatterplot")
plot(book_rules,method = "grouped")
plot(book_rules,method = "graph")
