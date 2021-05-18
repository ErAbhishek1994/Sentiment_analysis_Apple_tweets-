apple <- read.csv("apple.csv")
View(apple)


#Build Corpus
library(tm)
corpus <- iconv(apple$text, to="UTF-8")
corpus <-Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#cleaning Data
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

#Removing Punctuation
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

#removing Common Words
corpus <- tm_map(corpus, removeWords, stopwords("english")) 
inspect(corpus[1:5])

#removing URL
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
corpus <- tm_map(corpus, content_transformer(removeURL))
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeWords, c("aapl","apple"))
inspect(corpus[1:5])

cleanset <- tm_map(corpus, gsub,
                   pattern = 'stocks',
                   replacement = 'stock')

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#term Document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

#Bar-Plot

w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = rainbow(50))

#Word Cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          colors = brewer.pal(8,"Dark2"),
          scale = c(5,0.3),
          rot.per = 0.7,
          min.freq = 5)

# Sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

tweets <-iconv(apple$text, to="UTF-8")

#obtain Sentiment scores

s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
#get_nrc_sentiment('delay')
#get_nrc_sentiment('ugly')

barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = "Count",
        main = "Sentiment analysis of Apple Tweets")