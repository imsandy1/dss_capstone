library(shiny)

shinyUI(fluidPage(
    titlePanel("Next Word Prediction App"),
    
    sidebarLayout(
        
        sidebarPanel(
            textInput("text", h3("Enter text here"), value = "")
            
        ),
        
        
        mainPanel(
            tabsetPanel(
                tabPanel(title = "Prediction", 
                         h5("Predicted next word based on N-gram backoff model"),
                         p(textOutput("pred1")),
                         HTML("<br><br><br>"),
                         p("Thank you for using the app")),
                
                tabPanel(title = "Methodology", 
                         h4('Methodology used for the prediction model'),
                         p("The prediction model is built based on an N-gram backoff approach"),
                         HTML("The important steps in the methodology involved the following:
<ul> <li>Tokenize the combined corpus n-grams (bigrams, trigrams, quadgrams) from the combined corpus of news, blogs, twitter data</li>
<li>Build the frequency tables of all bigrams, trigrams, quadgrams and insert them into an sql db stored locally
<ul>
<li>This table1 contains n-gram, n, last-word, pre-phrase, frequency</li>
<li>Build a table2 grouped by pre-phrase, n, last-word, selecting the max(freqency)</li>
</ul>
</li>
<li>Build the prediction model based on the stupid backoff approach
<ul>
<li>Last 3 words of the input phrase is searched in the quadgram pre-phrase column in table2 and last word is predicted</li>
<li>If not found in quadgram, last 2 words of the input phrase is searched in the trigram pre-phrase column in table2 and last word is predicted</li>
<li>If not found in trigram, last 1 words of the input phrase is searched in the bigram pre-phrase column in table2 and last word is predicted</li>
</ul>
</li>
</ul>
")),
                tabPanel(title = "Author", 
                         h5("Sandeep Muttha"),
                         p("With the help of Data Science Specialization on Coursera offered by John Hopkins University"))
            )
        )
    )
))


