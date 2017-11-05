library(tm)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(dplyr)
library(RWeka)
library(tidytext)
library(stringi)
#help(package = "tm")

twitter <- file("c:/DataScience/1-John Hopkins/Capstone/final/en_US/en_US.twitter.txt")
blogs <- file("c:/DataScience/1-John Hopkins/Capstone/final/en_US/en_US.blogs.txt")
news <- file("c:/DataScience/1-John Hopkins/Capstone/final/en_US/en_US.news.txt")

options(warn=-1)

totaltwitterlines <-length(readLines(twitter)) 
totalblogslines <-length(readLines(blogs)) 
totalnewslines <-length(readLines(news)) 

set.seed(123)
train_corpus <- c(sample(readLines(twitter), totaltwitterlines *0.05),
                  sample(readLines(blogs), totalblogslines *0.05),          
                  sample(readLines(news), totalnewslines *0.1))


train_corpus <- Corpus(VectorSource(train_corpus))
train_corpus <- tm_map(train_corpus, PlainTextDocument)
#train_corpus <- tm_map(train_corpus,removeNumbers)
train_corpus <- tm_map(train_corpus, stripWhitespace)
train_corpus <- tm_map(train_corpus, removePunctuation)

#Added new after milestone


profanityWords <- c('ass','asshole','assholes','bitch','bastard','boob','boobs','butt','clit','cock','cums','cunts','ejaculate','fuck','fucked','fucker','shit','tits','whore','xxx')


train_corpus <- tm_map(train_corpus,content_transformer(function(x)  iconv(x, to="UTF-8", sub="byte")))
train_corpus <- tm_map(train_corpus,content_transformer(function(x) gsub("http[[:alnum:]]*", "", x)))
train_corpus <- tm_map(train_corpus, removeWords, profanityWords)
#train_corpus <- tm_map(train_corpus, stemDocument)
train_corpus <- tm_map(train_corpus, content_transformer(removeNumbers))
train_corpus <- tm_map(train_corpus, removeWords, stopwords("english"))
train_corpus <- tm_map(train_corpus, content_transformer(tolower), lazy = TRUE)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
train_corpus <- tm_map(train_corpus, content_transformer(removeNumPunct), lazy = TRUE)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.bigram = TermDocumentMatrix(train_corpus, control = list(tokenize = BigramTokenizer))

DF <- tidy(tdm.bigram)
DF <- DF %>%
  group_by(term) %>%
  summarize(frequency = sum(count))%>%
  filter(frequency > 4 )

#DF[DF$term == "i want",]
#DF[DF$onegram == "i" &  DF$bigram == "want",]



Cover<-0
for(i in 1:sum(complete.cases(DF))) {
  Cover <- Cover + DF$frequency[i]
  if(Cover >= 0.5*sum(DF$frequency)){break}
}
paste("Words needed to cover 50% instances: ",i)

bigram <- DF %>%
  dplyr::mutate(tempcol = stringr::str_split(term, ' ')) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(unigram = unlist(tempcol)[1], bigram = unlist(tempcol)[2]) %>%
  dplyr::select(unigram,bigram,frequency) %>%
  head(arrange(desc(frequency)), n = i)

arrange(bigram,desc(frequency))

bigram <- bigram %>% 
  mutate(unigram = iconv(unigram, from = "latin1", to = "ASCII")) %>%
  filter(!is.na(unigram))
   
saveRDS(bigram, file = "./bigram.RData")



###trigram
trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm.trigram = TermDocumentMatrix(train_corpus, control = list(tokenize = trigramTokenizer))

DF <- tidy(tdm.trigram)
DF <- str_replace_all(DF$term, "[^[:alnum:]]", " ")
DF <- DF %>%
  group_by(term) %>%
  summarize(frequency = sum(count))%>%
  filter(frequency > 2 )

Cover<-0
for(i in 1:sum(complete.cases(DF))) {
  Cover <- Cover + DF$frequency[i]
  if(Cover >= 0.5*sum(DF$frequency)){break}
}
paste("Words needed to cover 50% instances: ",i)

trigram <- DF %>%
  dplyr::mutate(tempcol = stringr::str_split(term, ' ')) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(unigram = unlist(tempcol)[1]
                  , bigram = unlist(tempcol)[2]
                  , trigram = unlist(tempcol)[3]) %>%
  dplyr::select(unigram,bigram,trigram,frequency) %>%
  head(arrange(desc(frequency)), n = i)

arrange(trigram,desc(frequency))

saveRDS(trigram, file = "./trigram.RData")


###quadgram
quadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
tdm.quadgram = TermDocumentMatrix(train_corpus, control = list(tokenize = quadgramTokenizer))

DF <- tidy(tdm.quadgram)
DF <- DF %>%
  group_by(term) %>%
  summarize(frequency = sum(count))%>%
  filter(frequency > 1 )

Cover<-0
for(i in 1:sum(complete.cases(DF))) {
  Cover <- Cover + DF$frequency[i]
  if(Cover >= 0.5*sum(DF$frequency)){break}
}
paste("Words needed to cover 50% instances: ",i)

quadgram <- DF %>%
  dplyr::mutate(tempcol = stringr::str_split(term, ' ')) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(unigram = unlist(tempcol)[1]
                , bigram = unlist(tempcol)[2]
                , trigram = unlist(tempcol)[3]
                , quadgram = unlist(tempcol)[4]) %>%
  dplyr::select(unigram,bigram,trigram,quadgram,frequency) %>%
  head(arrange(desc(frequency)), n = i)

arrange(quadgram,desc(frequency))

saveRDS(quadgram, file = "./quadgram.RData")

##Testing bigram
textInput <- "happy"
wordInput <- strsplit(textInput, " ")
wordCount <- length(wordInput[[1]])
wordCount
wordInput[[1]][1]

wordPrediction <- as.character(bigram[bigram$unigram==wordInput[[1]][1],][1,]$bigram)
wordPrediction

###Testing trigram
textInput <- "does anyone"
wordInput <- strsplit(textInput, " ")
wordCount <- length(wordInput[[1]])
wordCount
wordInput[[1]][1]
wordInput[[1]][2]

wordPrediction <- as.character(trigram[trigram$unigram==wordInput[[1]][1] & 
                       trigram$bigram==wordInput[[1]][2],][1,]$trigram)

wordPrediction

###Testing quadgram
textInput <- "happy cinco de"
wordInput <- strsplit(textInput, " ")
wordCount <- length(wordInput[[1]])
wordCount
wordInput[[1]][1]
wordInput[[1]][2]
wordInput[[1]][3]

wordPrediction<- as.character(quadgram[quadgram$unigram==wordInput[[1]][1] & 
                        quadgram$bigram==wordInput[[1]][2] & 
                        quadgram$trigram==wordInput[[1]][3],][1,]$quadgram)

wordPrediction
