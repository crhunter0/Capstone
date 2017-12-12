#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(quanteda)
library(stringr)
library(data.table)

load("~/testing.Rdata")
load("~/ngram1.Rdata")
load("~/ngram2.Rdata")
load("~/ngram3.Rdata")
load("~/ngram4.Rdata")

pred_count <- 0
saved_count <- 0
key_count <- 0
text_count <- 0

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


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  observeEvent(input$text, {
      out_text <<-  predWord(input$text)
      out_text[2] <<- if (is.na(out_text[2])) {" "} else out_text[2]
      out_text[3] <<- if (is.na(out_text[3])) {" "} else out_text[3]
      out_text <<- gsub("^i$", "I", out_text)
      
      
      updateActionButton(session, "button1", label = out_text[1])
      updateActionButton(session, "button2", label = out_text[2])
      updateActionButton(session, "button3", label = out_text[3])
      
      
      if (nchar(input$text) > text_count) {     
            key_count_chg <- nchar(input$text) - text_count
      } else 
          {key_count_chg <- 0
           text_count <<- nchar(input$text)
          }
      
      text_count <<- text_count + key_count_chg
      key_count <<- key_count + key_count_chg
      
      output$key_count <- renderText(key_count)
      #output$text_count <- renderText(nchar(input$text))
      
  })
    
    observeEvent(input$button1, {
        x <- input$button1
        updateTextAreaInput(session, "text", value= paste(input$text, out_text[1]))
        session$sendCustomMessage(type="refocus",message=list(NULL))
        saved_count <<- saved_count + nchar(out_text[1]) - 2
        output$saved_count <- renderText(saved_count)
        pred_count <<- pred_count + 1
        output$pred_count <- renderText(pred_count)
        #key_count <<- key_count - nchar(out_text[1]) -2
        #output$key_count <- renderText(key_count)
    })
        
    observeEvent(input$button2, {
        x <- input$button2
        updateTextAreaInput(session, "text", value= paste(input$text, out_text[2]))
        session$sendCustomMessage(type="refocus",message=list(NULL))
        saved_count <<- saved_count + nchar(out_text[2]) - 3
        output$saved_count <- renderText(saved_count)
        pred_count <<- pred_count + 1
        output$pred_count <- renderText(pred_count)
        #key_count <<- key_count - nchar(out_text[2]) -3
        #output$key_count <- renderText(key_count)
    })
    observeEvent(input$button3, {
        x <- input$button3
        updateTextAreaInput(session, "text", value= paste(input$text, out_text[3]))
        session$sendCustomMessage(type="refocus",message=list(NULL))
        saved_count <<- saved_count + nchar(out_text[3]) - 4
        output$saved_count <- renderText(saved_count)
        pred_count <<- pred_count + 1
        output$pred_count <- renderText(pred_count)
        #key_count <<- key_count - nchar(out_text[3]) -4
        #output$key_count <- renderText(key_count)
    })
    
  
})


