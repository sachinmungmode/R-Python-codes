library(arules)
library(arulesViz)
data("Groceries")
View(Groceries)
transactions <- Groceries
summary(transactions)
inspect(transactions[1:10])

### frequency plots
itemFrequencyPlot(transactions, support=0.1, cex.names=0.8)
itemFrequencyPlot(transactions, support=0.05, cex.names=0.8)
itemFrequencyPlot(transactions, topN=20)

### apriori algorithm
asso_rules <- apriori(Groceries, parameter = list(support = 0.009, confidence = 0.25, minlen = 3))
summary(asso_rules)
inspect(head(sort(asso_rules, by ="lift"),5))
inspect(sort(sort(asso_rules, by ="support"),by ="confidence")[1:5])

### visualization of rules
plot(asso_rules,method = "scatterplot")
plot(asso_rules,method = "grouped")
plot(asso_rules,method = "graph")
plot(asso_rules,method = "matrix")

