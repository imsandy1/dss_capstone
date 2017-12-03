## Libraries
pacman::p_load(dplyr, tm, clue, wordcloud, RWeka, ggplot2, stringi, slam, RColorBrewer, data.table, stringr, magrittr, quanteda, RSQLite, quanteda, beepr, widyr)
set.seed(0)

## Download files
download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", destfile = "data.zip")
unzip("data.zip")

## Load data
raw_twitter <- readLines("./final/en_US/en_US.twitter.txt", skipNul = TRUE, encoding = 'UTF-8')
raw_blogs <- readLines("./final/en_US/en_US.blogs.txt", skipNul = TRUE, encoding = 'UTF-8')
raw_news <- readLines("./final/en_US/en_US.news.txt", skipNul = TRUE, encoding = 'UTF-8')


# raw <- c(raw_twitter, raw_blogs,  raw_news)
#corpus <- VCorpus(VectorSource(raw_twitter))
#corpus <- VCorpus(VectorSource(raw_blogs))
#corpus <- VCorpus(VectorSource(raw_news))


freq.gram <- function(tdm, cutoff) {
    freq <- col_sums(tdm, na.rm = TRUE)
    freq <- freq[freq >= cutoff]
#   freq <- sort(col_sums(tdm, na.rm = TRUE), decreasing = TRUE)
    phrase <- names(freq)
    pre <- gsub("\\s*\\w*$", "", phrase)
    cur <- stringr::word(phrase, -1)
    data.table(phrase = phrase, 
               pre = pre, 
               cur = cur,
               freq = freq)
}

db <- dbConnect(SQLite(), dbname="grams.db")
dbSendQuery(conn=db,
            "CREATE TABLE NGrams
            (gram TEXT,
            pre TEXT,
            word TEXT,
            freq INTEGER,
            n INTEGER, PRIMARY KEY (gram))") #dbRemoveTable(db, "NGrams") <to remove table>

bulk_insert <- function(sql, key_counts) {
    dbBegin(db)
    dbGetPreparedQuery(db, sql, bind.data = key_counts)
    dbCommit(db)
}

## Build Bigram table

bigrams <- tokenize(raw_blogs, what = "fastestword", remove_symbols = TRUE, remove_numbers = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_url = TRUE, remove_separators = TRUE, ngrams = 2, concatenator = " ")
bigrams <- dfm(bigrams)
bifreq1 <- freq.gram(bigrams, 10)

bigrams <- tokenize(raw_news, what = "fastestword", remove_symbols = TRUE, remove_numbers = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_url = TRUE, remove_separators = TRUE, ngrams = 2, concatenator = " ")
bigrams <- dfm(bigrams)
bifreq2 <- freq.gram(bigrams, 10)

bigrams <- tokenize(raw_twitter, what = "fastestword", remove_symbols = TRUE, remove_numbers = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_url = TRUE, remove_separators = TRUE, ngrams = 2, concatenator = " ")
bigrams <- dfm(bigrams)
bifreq3 <- freq.gram(bigrams, 10)

bifreq <- rbind(bifreq1, bifreq2, bifreq3)
bifreq <- bifreq %>%
    group_by(phrase, pre, cur) %>%
    summarize(freq = sum(freq)) %>%
    arrange(desc(freq))

bulk_insert("INSERT INTO NGrams VALUES ($phrase, $pre, $cur, $freq, 2)", bifreq)
rm(bigrams, bifreq1, bifreq2, bifreq3)

## Build Trigram table

trigrams <- tokenize(raw_blogs, what = "fastestword", remove_symbols = TRUE, remove_numbers = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_url = TRUE, remove_separators = TRUE, ngrams = 3, concatenator = " ")
trigrams <- dfm(trigrams)
trifreq1 <- freq.gram(trigrams, 10)

trigrams <- tokenize(raw_news, what = "fastestword", remove_symbols = TRUE, remove_numbers = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_url = TRUE, remove_separators = TRUE, ngrams = 3, concatenator = " ")
trigrams <- dfm(trigrams)
trifreq2 <- freq.gram(trigrams, 10)

trigrams <- tokenize(raw_twitter, what = "fastestword", remove_symbols = TRUE, remove_numbers = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_url = TRUE, remove_separators = TRUE, ngrams = 3, concatenator = " ")
trigrams <- dfm(trigrams)
trifreq3 <- freq.gram(trigrams, 10)

trifreq <- rbind(trifreq1, trifreq2, trifreq3)
trifreq <- trifreq %>%
    group_by(phrase, pre, cur) %>%
    summarize(freq = sum(freq)) %>%
    arrange(desc(freq))

bulk_insert("INSERT INTO NGrams VALUES ($phrase, $pre, $cur, $freq, 3)", trifreq)
rm(trigrams, trifreq1, trifreq2, trifreq3, trifreq)


## Build Quadgram table

quadgrams <- tokenize(raw_blogs, what = "fastestword", remove_symbols = TRUE, remove_numbers = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_url = TRUE, remove_separators = TRUE, ngrams = 4, concatenator = " ")
quadgrams <- dfm(quadgrams)
quadfreq1 <- freq.gram(quadgrams, 10)

quadgrams <- tokenize(raw_news, what = "fastestword", remove_symbols = TRUE, remove_numbers = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_url = TRUE, remove_separators = TRUE, ngrams = 4, concatenator = " ")
quadgrams <- dfm(quadgrams)
quadfreq2 <- freq.gram(quadgrams, 10)

quadgrams <- tokenize(raw_twitter, what = "fastestword", remove_symbols = TRUE, remove_numbers = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_url = TRUE, remove_separators = TRUE, ngrams = 4, concatenator = " ")
quadgrams <- dfm(quadgrams)
quadfreq3 <- freq.gram(quadgrams, 10)

quadfreq <- rbind(quadfreq1, quadfreq2, quadfreq3)
quadfreq <- quadfreq %>%
    group_by(phrase, pre, cur) %>%
    summarize(freq = sum(freq)) %>%
    arrange(desc(freq))

bulk_insert("INSERT INTO NGrams VALUES ($phrase, $pre, $cur, $freq, 4)", quadfreq)
rm(quadgrams, quadfreq1, quadfreq2, quadfreq3, quadfreq)








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
        res <- dbSendQuery(conn=db, sql)
        predicted <- dbFetch(res, n=-1)
        
        if (!is.na(predicted[1])) return(predicted)
    }
    
    return("Nothing")
}

#########################################################################################
## Testing
#########################################################################################


ngram_backoff("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", db)
ngram_backoff("You're the reason why I smile everyday. Can you follow me please? It would mean the", db)#
ngram_backoff("Hey sunshine, can you follow me and make me the", db)
ngram_backoff("Very early observations on the Bills game: Offense still struggling but the", db)
ngram_backoff("Go on a romantic date at the", db)
ngram_backoff("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", db)#
ngram_backoff("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", db)
ngram_backoff("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", db)
ngram_backoff("Be grateful for the good times and keep the faith during the", db)
ngram_backoff("If this isn't the cutest thing you've ever seen, then you must be", db)
