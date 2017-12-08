library(quanteda)
library(stringr)
library(data.table)

load("/home/chris/Capstone/testing.Rdata")
load("/home/chris/Capstone/ngram1.Rdata")
load("/home/chris/Capstone/ngram2.Rdata")
load("/home/chris/Capstone/ngram3.Rdata")
load("/home/chris/Capstone/ngram4.Rdata")

predWord <- function(in_txt) {
    tokens_all <- tokens_tolower(tokens(in_txt, remove_numbers = TRUE,
                                        remove_punct = TRUE, remove_twitter = TRUE))
    tokens_char <- as.character(tokens_all)
    
    if (length(tokens_char) > 2) {
        in_quad_base <-
            paste(tokens_char[(length(tokens_char)-2):length(tokens_char)], collapse = "_")
    } else in_quad_base <- ""
    
    in_tri_base <- 
        paste(tokens_char[(length(tokens_char)-1):length(tokens_char)], collapse = "_")
    in_bi_base <- tokens_char[(length(tokens_char))]
    
    sub_ngram4 <- ngram4[base == in_quad_base]
    sub_ngram4_last3 <- paste0(gsub("^.*?_", "", sub_ngram4$base),"_", sub_ngram4$last)
    sub_ngram4_mid <- gsub("^.*?_", "", sub_ngram4$base)[1]
    
    sub_ngram3 <- ngram3[base == in_tri_base]
    sub_ngram3_last2 <- paste0(gsub("^.*_", "", sub_ngram3$base),"_", sub_ngram3$last)
    sub_ngram3_mid <- gsub("^.*_", "", sub_ngram3$base)[1]
    sub_ngram2 <- ngram2[base == in_bi_base]
    sub_ngram1 <- data.table(ngram = character(), base = character(), last = character(),
                             count = numeric(), discount = numeric(),
                             prob = numeric())
    
    if (nrow(sub_ngram4) > 0) {
        sub_ngram4$prob <- sub_ngram4$count * sub_ngram4$discount / sum(sub_ngram4$count)
        lo_prob <- 1 - sum(sub_ngram4$prob)
        sub_ngram3 <- ngram3[!(ngram3$ngram %in% sub_ngram4_last3) & ngram3$base %in% sub_ngram4_mid]
        if (nrow(sub_ngram3) > 0) {
            sub_ngram2 <- ngram2[!(ngram2$ngram %in% sub_ngram3_last2) 
                                 & ngram2$base %in% sub_ngram3_mid]
        }
        if (dim(sub_ngram3)[1] > 0) {
            sub_ngram3$prob <- (sub_ngram3$count * sub_ngram3$discount) * 
                (lo_prob / sum(sub_ngram3$count)) 
        }
        else if (dim(sub_ngram2)[1] > 0) {
            sub_ngram2$prob <- (sub_ngram2$count * sub_ngram2$discount) * 
                (lo_prob / sum(sub_ngram2$count)) 
        }
        else 
            sub_ngram1 <- ngram1[!(ngram1$ngram %in% sub_ngram3$last) & count > 1000]
        sub_ngram1$prob <- (sub_ngram1$count * sub_ngram1$discount) * 
            (lo_prob / sum(sub_ngram1$count))
        
    } else if (nrow(sub_ngram3) > 0) {
        sub_ngram3$prob <- sub_ngram3$count * sub_ngram3$discount / sum(sub_ngram3$count)
        lo_prob <- 1 - sum(sub_ngram3$prob)
        sub_ngram2 <- ngram2[!(ngram2$ngram %in% sub_ngram3_last2) & ngram2$base %in% sub_ngram3_mid]
        
        if (dim(sub_ngram2)[1] > 0) {
            sub_ngram2$prob <- (sub_ngram2$count * sub_ngram2$discount) * 
                (lo_prob / sum(sub_ngram2$count)) 
        }
        else 
            sub_ngram1 <- ngram1[!(ngram1$ngram %in% sub_ngram3$last) & count > 1000]
        sub_ngram1$prob <- (sub_ngram1$count * sub_ngram1$discount) * 
            (lo_prob / sum(sub_ngram1$count))
        
        #} else if (nrow(sub_ngram2) > 0) {
    } else if (dim(sub_ngram2)[1] > 0) {
        sub_ngram2$prob <- sub_ngram2$count * sub_ngram2$discount / sum(sub_ngram2$count)
        lo_prob <- 1 - sum(sub_ngram2$prob)
        sub_ngram1 <- ngram1[!(ngram1$ngram %in% sub_ngram2$last)]
        sub_ngram1$prob <- (sub_ngram1$count * sub_ngram1$discount) * 
            (lo_prob / sum(sub_ngram1$count))
    } else 
        #sub_ngram1 <- ngram1[count == max(count)]
        sub_ngram1 <- head(ngram1[order(-count)],3)
    sub_ngram1$prob <- sub_ngram1$count / sum(sub_ngram1$count)
    
    
    result_set <- as.data.table(rbind(if (nrow(sub_ngram4) > 0) {sub_ngram4},
                                      if (nrow(sub_ngram3) > 0) {sub_ngram3},
                                      if (nrow(sub_ngram2) > 0) {sub_ngram2},
                                      if (nrow(sub_ngram1) > 0) {sub_ngram1}))
    if (length(result_set) > 0) {
        return(head(result_set[order(-prob)], 3)$last)
    } else return(NA)
}

out_txt_dt <- data.table(txt = rep(" ", length(testing)), pred1 = rep(" ", length(testing)),
                         pred2 = rep(" ", length(testing)), pred3 = rep(" ", length(testing)),
                         actual = rep(" ", length(testing)))

for (i in 1:length(testing)) {
    #for (i in 1:1000) {
    #print(i)
    if (i %% 500 == 0){print(i)} 
    tokens_all <- tokens_tolower(tokens(testing[i], remove_numbers = TRUE,
                                        remove_punct = TRUE, remove_twitter = TRUE))
    tokens_char <- as.character(tokens_all)
    
    if (length(tokens_char) == 0) {tokens_char <- " "}
    in_txt <- paste(tokens_char[(length(tokens_char)-length(tokens_char) + 1):
                                    if (length(tokens_char) < 6) {length(tokens_char)-1} else 5], collapse = " ")
    in_txt_actual <- paste(tokens_char[if (length(tokens_char) < 6) {length(tokens_char)} else 6:
                                           if (length(tokens_char) < 6) {length(tokens_char)} else 6])
    pred_word <- predWord(in_txt)
    set(out_txt_dt,i,1L, in_txt)
    set(out_txt_dt,i,2L, pred_word[1])
    set(out_txt_dt,i,3L, pred_word[2])
    set(out_txt_dt,i,4L, pred_word[3])
    set(out_txt_dt,i,5L, in_txt_actual)
}

nrow(out_txt_dt[pred1 == actual]) / nrow(out_txt_dt)

nrow(out_txt_dt[pred1 == actual | pred2 == actual | pred3 == actual]) / nrow(out_txt_dt)
