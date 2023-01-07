library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Word Prediction App"),
    sidebarPanel(
        h4("Please enter an English phrase below."),
        textInput("phrase_", label="Submit the phrase to see top predictions."),
        submitButton("Submit")
    ),
    mainPanel(
        verbatimTextOutput("message"),
        plotOutput("barChart")
    )
))
