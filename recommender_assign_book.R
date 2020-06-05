library(recommenderlab)
library(caTools)
book <- read.csv("D:/Assignments/Recomendation system/book.csv")
View(book)
str(book)
# rating distribution
hist(book$Book.Rating)
# real rating matrix for recommender system
book_matrix <- as(book, 'realRatingMatrix')
book_matrix
# model building
book_recomm_model <- Recommender(book_matrix, method = "POPULAR")
book_recomm_model
# predictions for two users
recommended_item <- predict(book_recomm_model,book_matrix[513:514],n=5)
as(recommended_item,"list")

# collaborative filtering method
book_recomm_model1 <- Recommender(book_matrix, method = "UBCF")
book_recomm_model1
# prediction for two user
recommended_item1 <- predict(book_recomm_model1, book_matrix[513:514],n = 5)
as(recommended_item1,"list")
recommended_item2 <- predict(book_recomm_model1, book_matrix[100:200],n = 3)
as(recommended_item2,"list")
