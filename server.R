library(shiny)
library(dplyr)
library(tm)
library(stringr)
library(RSQLite)

options(shiny.maxRequestSize=600*1024^2) 

load("grams.Rda")

ngram_backoff <- function(raw) {
    max = 3  # max n-gram - 1
    
    # process sentence
    sentence <- tolower(raw) %>%
        # removeWords(words=stopwords("english")) %>%
        removePunctuation %>%
        removeNumbers %>%
        stripWhitespace %>%
        str_trim %>%
        strsplit(split=" ") %>%
        unlist
    
    for (i in min(length(sentence), max):1) {
        gram <- paste(tail(sentence, i), collapse=" ")
#        sql <- paste("SELECT word, gram, MAX(freq) FROM NGrams WHERE pre=='", paste(gram), "'",
#                     " AND n==", i + 1,sep="")
#        result <- dbSendQuery(conn=db, sql)
#        predicted <- dbFetch(result, n=-1)
        
        predicted <- grams[grams$pre == gram, 3][which.max(grams[grams$pre == gram, 4])]
        if (!identical(predicted, character(0))) return(predicted)
    }
    
    return("")
}



shinyServer(function(input, output) {
    
    pred <- reactive({
        ngram_backoff(input$text)[[1]]
    })

    
    # Output tab            
    output$pred1 <- pred
    
    
})
