
library(shiny)

db <- dbConnect(SQLite(), dbname="grams.db")
ngram_backoff <- function(raw, db) {
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
        sql <- paste("SELECT word, gram, MAX(freq) FROM NGrams WHERE pre=='", paste(gram), "'",
                     " AND n==", i + 1,sep="")
        result <- dbSendQuery(conn=db, sql)
        predicted <- dbFetch(result, n=-1)
        
        if (!is.na(predicted[1])) return(predicted)
    }
    
    return("")
}



shinyServer(function(input, output) {
    
    pred <- reactive({
        ngram_backoff(input$text, db)[[1]]
    })

    
    # Output tab            
    output$pred1 <- pred
    
    
})
