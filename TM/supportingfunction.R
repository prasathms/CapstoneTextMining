suppressPackageStartupMessages(c(
  library(shinythemes),
  library(shiny),
  library(tm),
  library(stringr),
  library(markdown),
  library(stylo)))

quadgram <- readRDS(file="./data/quadgram.RData")
trigram <- readRDS(file="./data/trigram.RData")
bigram <- readRDS(file="./data/bigram.RData")

dataCleaner<-function(text){
  
  cleanText <- tolower(text)
  cleanText <- removePunctuation(cleanText)
  cleanText <- removeNumbers(cleanText)
  cleanText <- str_replace_all(cleanText, "[^[:alnum:]]", " ")
  cleanText <- stripWhitespace(cleanText)
  
  return(cleanText)
}

cleanInput <- function(text){
  
  textInput <- dataCleaner(text)
  textInput <- txt.to.words.ext(textInput, 
                                language="English.all", 
                                preserve.case = TRUE)
  
  return(textInput)
}


nextWordPrediction <- function(wordCount,textInput){
  wordInput <- strsplit(textInput, " ")
  if (wordCount==3) {
    
    wordPrediction<- as.character(quadgram[quadgram$unigram==wordInput[[1]][1] & 
                                             quadgram$bigram==wordInput[[1]][2] & 
                                             quadgram$trigram==wordInput[[1]][3],][1,]$quadgram)
  }
  
  else if(wordCount==2) {
 
    wordPrediction <- as.character(trigram[trigram$unigram==wordInput[[1]][1] & 
                                            trigram$bigram==wordInput[[1]][2],][1,]$trigram)
  }
  
  else {
    wordPrediction <- as.character(bigram[bigram$unigram==wordInput[[1]][1],][1,]$bigram)
  }
  
  
  print(wordPrediction)
  
}
