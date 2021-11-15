#installing libraries 
library(shiny)
library(shinythemes)
library(RColorBrewer)
library(ggplot2)
library(wordcloud)
library(tidyverse)
library(tidytext)                                                                    
library(dplyr)                

#list of books         
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")       
#preprocessing steps
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./Data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  return(text)
}

#creating the ui's layout, and adding inputs  
ui <- fluidPage(
  theme=shinytheme("slate"), #changing the theme to "slate"
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId="books", label="Select a Book", choices=books),
      checkboxInput(inputId="stopwords", label="Stop Words:", value=TRUE),
      actionButton("rerun", "Rerun"),
      hr(),
      h3("Word Cloud Settings"),
      sliderInput(inputId="maxwords", label="Max # of Words:", min=10, max=200,
                  value=100, step=10),
      sliderInput(inputId="largewords", label="Size of Largest Words:", min=1,
                  max=8, value=4),
      sliderInput(inputId="smallwords", label="Size of Smallest Words:", min=0.1,
                  max=4, value=0.5),
      hr(),
      h3("Word Count Settings"),
      sliderInput(inputId="minwords", label="Minimum Words for Counts Chart:",
                  min=10, max=100, value=25),
      sliderInput(inputId="fntsize", label="Word Size for Counts Chart:", min=8,
                  max=30, value=14)
     
    ),
    #creating tabs with ui outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud", plotOutput("cloud", height="600px")), 
        tabPanel("Word Counts", plotOutput("freq", height="600px"))
      )
    )
  )
)
#creating server with reactivty to update word counts based on inputs
server <- function(input, output) {
  # reactive expression
  freq <- eventReactive(input$rerun, {
    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$books, input$stopwords) # ... = replace with the two inputs from Task 2
    })
  })
  #output for cloud tab
  output$cloud <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$largewords, input$smallwords),
          random.order = FALSE, 
          max.words = input$maxwords, 
          colors=pal))
  })
  #output for count tab
  output$freq <- renderPlot({
    v <- freq()
    #pal <- brewer.pal(8,"Dark2")
    v %>% 
      filter(input$minwords < n) %>%
      ggplot(aes(x=reorder(word, n), y=n)) + 
      geom_col() +
      coord_flip() + 
      theme(text=element_text(size=input$fntsize), axis.title.x=element_blank(),
            axis.title.y=element_blank())
  })

  
}

shinyApp(ui = ui, server = server)