#Tasks to accomplish

#Exploratory analysis - perform a thorough exploratory analysis of the data, 
#understanding the distribution of words and relationship between the words in the corpora.
#Understand frequencies of words and word pairs - build figures and tables to understand variation in the frequencies of words and word pairs in the data.

#Questions to consider

#Some words are more frequent than others - what are the distributions of word frequencies?
#What are the frequencies of 2-grams and 3-grams in the dataset?
#How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
#How do you evaluate how many of the words come from foreign languages?
#Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of 
#words in the dictionary to cover the same number of phrases?

install.packages("textcat")
library("textcat")
textcat("tamil theriyama")

library(tm)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(dplyr)
library(RWeka)
#help(package = "tm")

twitter <- file("c:/DataScience/1-John Hopkins/Capstone/final/en_US/en_US.twitter.txt")
blogs <- file("c:/DataScience/1-John Hopkins/Capstone/final/en_US/en_US.blogs.txt")
news <- file("c:/DataScience/1-John Hopkins/Capstone/final/en_US/en_US.news.txt")

options(warn=-1)

totaltwitterlines <-length(readLines(twitter)) 
totalblogslines <-length(readLines(blogs)) 
totalnewslines <-length(readLines(news)) 

barplot(c(totaltwitterlines,totalblogslines,totalnewslines))
barplot(c(totaltwitterlines*0.0005,totalblogslines*0.00125,totalnewslines*0.015))


set.seed(123)
train_corpus <- c(sample(readLines(twitter), totaltwitterlines *0.0005),
                  sample(readLines(blogs), totalblogslines *0.00125),          
                  sample(readLines(news), totalnewslines *0.015))


train_corpus <- Corpus(VectorSource(train_corpus))
train_corpus <- tm_map(train_corpus, PlainTextDocument)
train_corpus <- tm_map(train_corpus,removeNumbers)
train_corpus <- tm_map(train_corpus, stripWhitespace)
train_corpus <- tm_map(train_corpus, removePunctuation)
#train_corpus <- tm_map(train_corpus, content_transformer(tolower))


OnegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tdm.onegram = TermDocumentMatrix(train_corpus, control = list(tokenize = OnegramTokenizer))

freq = sort(rowSums(as.matrix(tdm.onegram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F)


sum(complete.cases(freq.df))

head(freq.df$word[grepl("[^ -~]", freq.df$word) == 'TRUE'],1000)

Cover<-0
for(i in 1:sum(complete.cases(freq.df))) {
  Cover <- Cover + freq.df$freq[i]
  if(Cover >= 0.5*sum(freq.df$freq)){break}
}
paste("Words needed to cover 50% instances: ",i)


Cover<-0
for(i in 1:sum(complete.cases(freq.df))) {
  Cover <- Cover + freq.df$freq[i]
  if(Cover >= 0.9*sum(freq.df$freq)){break}
}
paste("Words needed to cover 90% instances: ",i)



x = c('Kält', 'normal', 'normal with, punctuation ~-+!', 'normal with number 1234')
grep(pattern = "[^[:ascii:]]", x, perl=TRUE) 
grep(pattern = "[^[:ascii:]]", x, value=TRUE, perl=TRUE)


BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.bigram = TermDocumentMatrix(train_corpus, control = list(tokenize = BigramTokenizer))

freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

freq.df

wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F)


TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

train_corpus_df <- data_frame(line = 1:length(train_corpus), text = train_corpus)


#train_corpus <- tm_map(train_corpus,removeWords,c(stopwords(),"s","ve"))
train_corpus <- tm_map(train_corpus, tolower)

train_corpus <- tm_map(train_corpus,removeNumbers)
train_corpus <- tm_map(train_corpus, stripWhitespace)
#train_corpus <- tm_map(train_corpus, removeWords,stopwords("english"))
train_corpus <- tm_map(train_corpus, removePunctuation)
#train_corpus <- tm_map(train_corpus, stemDocument)  

corpus <- Corpus(VectorSource(train_corpus)) # change class 
matrix_term <- DocumentTermMatrix(corpus)


BigramTokenizer <- function(x) { 
  unlist(
    lapply(ngrams(words(x), 1), paste, collapse = " "), 
    use.names = FALSE
  ) 
}



library(dplyr)
data.frame(inspect(tdm)) %>% 
  add_rownames() %>% 
  mutate(total = rowSums(.[,-1])) %>% 
  arrange(desc(total))


require(quanteda)

toks <- tokens(train_corpus, removePunct = TRUE, removeHyphens = FALSE)
train.tokens <- tokens(train_corpus_df$text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)

train.tokens <- tokens_ngrams(train.tokens, n = 1:2)
train.tokens[[357]]

word <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))
tdm <- TermDocumentMatrix(train_corpus,control = list(tokenize = tok))
termFreq <- rowSums(as.matrix(tdm))
termFreqVector <- as.list(termFreq)
termFreqVector




#2-gram
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

tdm.bigram = TermDocumentMatrix(train_corpus, control = list(tokenize = BigramTokenizer))

freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

strsplit(as.character(freq.df$word[2])," ")

x <- "Split the words in a sentence."
strsplit(x, " ")

wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F)

ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams")

#3-Gram
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

tdm.trigram = TermDocumentMatrix(train_corpus, control = list(tokenize = TrigramTokenizer))
freq = sort(rowSums(as.matrix(tdm.trigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F)

ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +   
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Trigrams") + ylab("Frequency") +
  ggtitle("Most frequent trigrams")

tdm.onegram<-WordTokenizer(train_corpus)
tdm.onegram <- data.frame(table(tdm.onegram))
removeNumbers(tdm.onegram$tdm.onegram)

onegrams <- data.frame(table(words))
tdm.onegram <- tdm.onegram[order(tdm.onegram$Freq, decreasing = TRUE),]


freq = sort(rowSums(as.matrix(tdm.onegram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F)

ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +   
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Trigrams") + ylab("Frequency") +
  ggtitle("Most frequent trigrams")