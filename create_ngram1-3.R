library(quanteda)
library(stringr)
library(data.table)

prof <- read.csv("/home/chris/Downloads/swearWords.csv", header = FALSE)
prof <- as.vector(t(prof))

load("/home/chris/Capstone/training.Rdata")

createNgramTable <- function(ngram_dfm) {
    freq <- docfreq(ngram_dfm)
    base0 <-  unlist(str_split(names(freq),"_[^_]+$"))
    base <- base0[base0 != ""]
    last <- gsub("^.*_", "", names(freq))
    count <- as.numeric(freq) 
    dt <- data.table(ngram = names(freq), base, last, count)
    dt <- dt[!(last %in% prof)]
    dt
}

createDFM <- function(data, n){
    x <- dfm(data, tolower = T, remove_numbers = T, remove_punct = T,
             remove_twitter = T, ngrams = n) 
    return(x)
}

calcGTDiscount = function(ngram_dt){
    ngram_dt$discount = rep(1, nrow(ngram_dt))
    ngram_dt$prob = rep(0, nrow(ngram_dt))
    
    for(i in 5:1){
        curr_i = i
        next_i = i + 1
        
        curr_num = nrow(ngram_dt[count == curr_i])
        next_num = nrow(ngram_dt[count == next_i])
        
        curr_disc = (next_i / curr_i) * (next_num / curr_num)
        
        ngram_dt[count == curr_i, discount := curr_disc]
    }
    return(ngram_dt)
}


dfm_unigram <- createDFM(training, 1)
unigram_dt <- createNgramTable(dfm_unigram)
rm(dfm_unigram)
ngram1 <- unigram_dt[count > 1000]
ngram1 <- calcGTDiscount(ngram1)
save(ngram1, file = "/home/chris/Capstone/ngram1.Rdata")
rm(unigram_dt)
rm(ngram1)
gc()

dfm_bigram <- createDFM(training, 2)
bigram_dt <- createNgramTable(dfm_bigram)
rm(dfm_bigram)
ngram2 <- bigram_dt[count > 2]
ngram2 <- calcGTDiscount(ngram2)
save(ngram2, file = "/home/chris/Capstone/ngram2.Rdata")
rm(bigram_dt)
rm(ngram2)
gc()

dfm_trigram <- createDFM(training, 3)
trigram_dt <- createNgramTable(dfm_trigram)
rm(dfm_trigram)
ngram3 <- trigram_dt[count > 2]
ngram3 <- calcGTDiscount(ngram3)
save(ngram3, file = "/home/chris/Capstone/ngram3.Rdata")
rm(trigram_dt)
rm(ngram3)
gc()

dfm_quadgram <- createDFM(training, 4)
quadgram_dt <- createNgramTable(dfm_quadgram)
rm(dfm_quadgram)
ngram4 <- quadgram_dt[count > 2]
ngram4 <- calcGTDiscount(ngram4)
save(ngram4, file = "/home/chris/Capstone/ngram4.Rdata")
rm(ngram4)
rm(quadgram_dt)
gc()
