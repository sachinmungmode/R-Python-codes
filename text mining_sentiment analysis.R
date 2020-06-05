library(twitteR)
library(ROAuth)
library(dplyr)
library(tm)
library(SnowballC)
# 1) Extracting tweets of user from twitter
# authentical keys
cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")
library(base64enc)

setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                    "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                    "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                    "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret

#register Twitter OAuth(cred)
Tweets <- userTimeline('PiyushGoyal', n = 100)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)
head(TweetsDF$text)

# remove hashtags, urls

tweets.df2 <- gsub("http.*","",TweetsDF$text)
tweets.df2 <- gsub("https.*","",tweets.df2)
tweets.df2 <- gsub("#.*","",tweets.df2)
tweets.df2 <- gsub("@.*","",tweets.df2)
head(tweets.df2)

# getting sentiment score
library(syuzhet)
word.df <- as.vector(tweets.df2)
emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(tweets.df2, emotion.df) 
head(emotion.df2)

# extract sentiment score
sent.value <- get_sentiment(word.df)
most.positive <- word.df[sent.value == max(sent.value)]
most.positive

most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative 
sent.value
#Segregating positive and negative tweets
positive.tweets <- word.df[sent.value>0]
head(positive.tweets)
negative.tweets <- word.df[sent.value<0]
head(negative.tweets)
neutral.tweets <- word.df[sent.value==0]
head(neutral.tweets)

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)
table(category_senti)

# 2) Extracting reviews from ecommerce site
library(rvest)
library(XML)
library(magrittr)
# amazon reviews
oneplus_amazon <- "https://www.amazon.in/Test-Exclusive-746/dp/B07DJHXTLJ/ref=lp_976392031_1_15?s=computers&ie=UTF8&qid=1590227596&sr=1-15"
oneplus_amazon
amazon_reviews <- NULL
for (i in 1:10){
  oneplus_url <- read_html(as.character(paste(oneplus_amazon,i,sep="=")))
  rev <- oneplus_url %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"oneplus.txt",row.names = F)
head(amazon_reviews)
OP <- readLines("D:\\Assignments\\Text minning\\oneplus.txt")
OP
length(OP)
mydata.corpus <- Corpus(VectorSource(OP))
mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
my_stopwords <- c(stopwords('english'))
mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)
mydata.corpus <- tm_map(mydata.corpus, removeNumbers)
mydata.corpus <- tm_map(mydata.corpus, stripWhitespace)
head(mydata.corpus)
library("syuzhet")
s_v <- get_sentences(OP)
class(s_v)
str(s_v)
head(s_v)

sentiment_vector <- get_sentiment(s_v, method = "bing")
head(sentiment_vector)
sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

# to extract the most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive
# To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative

word.df <- as.vector(OP)
sent.value <- get_sentiment(word.df)
category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)
table(category_senti)

# 3) Extracting reviews for movie from IMDB

aurl <- "https://www.imdb.com/title/tt1477834/reviews?ref_=tt_ov_rt"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)
write.table(IMDB_reviews,"Aquaman.txt",row.names = F)

## sentimental analysis
aqua_lines <- readLines("D:\\Assignments\\Text minning\\Aquaman.txt")
library("syuzhet")
s_v <- get_sentences(aqua_lines)
class(s_v)
str(s_v)
head(s_v)

sentiment_score <- get_nrc_sentiment(s_v)
head(sentiment_score)

# barplot 
barplot(colSums(sentiment_score), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews
        for Aquaman')
