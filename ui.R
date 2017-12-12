#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


shinyUI(navbarPage("Word Predictor", #theme = "~/Capstone/text_predict/style.css",
    tabPanel("App",
  
        includeCSS("~/style.css"),
        tags$head(tags$script(
            'Shiny.addCustomMessageHandler("refocus",
                                  function(NULL) {
                                    document.getElementById("text").focus();
                                  });'
        )),
  
  
      #sidebarLayout(
      fluidRow(
        #sidebarPanel(
            column(12, textAreaInput("text", " ", rows = 5, width = "100%"))
            #submitButton("Submit")
        ),
    #)  
      
        #mainPanel(
        fluidRow(  
           column(3, p("Primary Prediction:")), column(2, actionButton("button1", label = " "))
        ),
        br(),
        fluidRow(
           column(3, p("Other Alternatives:")), column(3, actionButton("button2", label = " "),
           actionButton("button3", label = " "))
        ),
        br()
        
      #)
    ),
    tabPanel("Stats",
        fluidRow(
            h3("Statistics For Current Session")
        ),
        fluidRow(
            column(4, h4("Characters entered: ")), 
            column(2, textOutput("key_count"))
        ),
        fluidRow(
            column(4, h4("Keystrokes saved: ")),
            column(2, textOutput("saved_count"))
        ),
        fluidRow(
            column(4, h4("Accepted predictions: ")),
            column(2, textOutput("pred_count"))
        )
    
    ),
    
    tabPanel("Help",
             h3("Instructions for using Word Predictor"),
             tags$ol(
                 tags$li("Just start typing in the text box."),
                 tags$li("Predictions will show up in the buttons below."),
                 tags$li("Click button to append predicted word to phrase in text box."),
                 tags$li("Click the Stats tab at the top of the page to see statistics for current
                         session.")
             )
             
    )
))
