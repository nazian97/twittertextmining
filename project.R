# Project

tweets<- read.csv(file.choose(), header = T) 

# subsetting en only original tweets (not retweet)
entweets<- subset(tweets, tweets$lang == "en")
ogtweets<- subset(entweets, entweets$is_retweet == "FALSE")

# subsetting tweets by author
trump<- subset(ogtweets, ogtweets$handle == "realDonaldTrump")
clinton<- subset(ogtweets, ogtweets$handle == "HillaryClinton")

library(tm)
library(SnowballC)
library(wordcloud)
library(syuzhet)
library(RColorBrewer)
library(quanteda)
library(dplyr)
library(tidytext)
library(tidyverse)
library(broom)
library(tidyr)
library(ggplot2)

summary(ogtweets)
dim(ogtweets)
unique(ogtweets$handle)

# convert to corpus format
# define a corpus text
head(clinton)
clintonCorp<- Corpus(VectorSource(clinton$text))
clintonCorp
inspect(clintonCorp)

# Data Cleaning
# remove url
removeURL<- function(x) gsub("http[^[:space:]]*","",x)
clintonCorp<- tm_map(clintonCorp, content_transformer(removeURL))

# remove everything other than english letters
removeSymbols<- function(x) gsub("[^[:alpha:][:space:]]*","",x)
clintonCorp<- tm_map(clintonCorp, content_transformer(removeSymbols))
toSpace<- content_transformer(function(x, pattern) gsub(pattern, "",x)) # define white space
clintonCorp<- tm_map(clintonCorp, toSpace, "â") # remove â

# remove line breaks
removeBreaks<- function(x) gsub("\r?\n|\r", " ", x)
clintonCorp<- tm_map(clintonCorp, content_transformer(removeBreaks))

# convert the text to lower case
clintonCorp<- tm_map(clintonCorp, content_transformer((tolower)))

# text stemming
clintonCorp <- tm_map(clintonCorp, stemDocument)

# remove stop words
clintonCorp<- tm_map(clintonCorp, removeWords, stopwords("english"))
myStopWords<- c("trump","hillari","donald", "realdonaldtrump")
clintonCorp<- tm_map(clintonCorp, removeWords, myStopWords)

# eliminate extra unnecessary spaces in the text
clintonCorp<- tm_map(clintonCorp, stripWhitespace)
inspect(clintonCorp)


# Word Frequency
# document-term matrix
clintonCorp.m <- TermDocumentMatrix(clintonCorp)
mt <- as.matrix(clintonCorp.m)

# the frequency table of words
f <- sort(rowSums(mt), decreasing = T)
df <- data.frame(word=names(f), freq=f)

# plot the first 10 most frequent words
barplot(df[1:10,]$freq, main="Top 10 Frequent Words Tweeted by Hillary Clinton",
        names.arg=df[1:10,]$word)

# Word Cloud
set.seed(12)
wordcloud(words=df$word, freq=df$freq, min.freq=10, max.words=110, 
          colors=brewer.pal(8,"Dark2"))

# Word Association
# analyze association between frequent words

# the frequent term in document-term matrix (occurs at least 50x)
findFreqTerms(clintonCorp.m, lowfreq = 50)

# find association between some particular frequent words
findAssocs(clintonCorp.m, term=c("can","elect","make"), corlimit=0.25)

# find association for words that occur at least 50 times
findAssocs(clintonCorp.m, term=findFreqTerms(clintonCorp.m, lowfreq = 100), corlimit=0.25)

# plot bigrams
clintonCorp.chr<- as.character(clintonCorp)
data_frame(txt = clintonCorp.chr) %>%
  unnest_tokens(bigram, txt, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE) %>%
  top_n(15) %>%
  ggplot(aes(fct_reorder(bigram, n), n)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL)


# Sentiment Analysis
# visualize which words contributed to positive and negative sentiment
clintonTidy<- tidy(clintonCorp.m)
clintonSM<- clintonTidy %>% 
  inner_join(get_sentiments("bing"), by = c(term = "word"))

clintonSM[1:100,] %>%
  count(sentiment, term, wt = count) %>%
  filter(n >= 1) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("First hundred contribution to sentiment")


# summary of the statement
clintonCorp.s <- get_sentiment(clinton$text, method="syuzhet")
summary(clintonCorp.s)
# mean is positive, so the content of text provide a positive overall

