suppressPackageStartupMessages(c(
  library(shinythemes),
  library(shiny),
  library(tm),
  library(stringr),
  library(markdown),
  library(stylo)))


shinyUI(fluidPage(
    
  # Application title
  titlePanel("Capstone Project - Swiftkey Shiny Application"),
  
  # Input text 
  sidebarLayout(
    sidebarPanel(
            textInput("text", label = h3("Enter your text here:"), value="home"),
            h4("Uniword samples:"),
            p("able,advance,home,increase,iphone"),
            h4("Biword samples:"),
            p("happy mothers,cinco de,couple years,come join"),  
            h4("Triword samples:"),
            p("go sox lets,happy cinco de,available customer area")   
    ),
    
    # Display predicted word
    mainPanel(
      h4("Entered Word: "),
      h4(textOutput("enteredWords")),
      
      h4("Predicated Word: "),
      h4(textOutput("predictedWord"))
      

      )
  )
))

                            