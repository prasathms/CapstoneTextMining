suppressPackageStartupMessages(c(
  library(shinythemes),
  library(shiny),
  library(tm),
  library(stringr),
  library(markdown),
  library(stylo),
  library(stringi)))

source("./supportingfunction.R")

shinyServer(function(input, output) {
  
  wordPrediction <- reactive({
    text <- input$text
    #textInput <- cleanInput(text)
    wordInput <- strsplit(text, " ")
    wordCount <- length(wordInput[[1]])    
    wordPrediction <- nextWordPrediction(wordCount,input$text)})
  
  output$predictedWord <- renderPrint(wordPrediction())
  output$enteredWords <-  renderText({input$text}, quoted = FALSE)
})