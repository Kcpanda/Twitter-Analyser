#make sure to run the twitter.R script before running the app
library(shiny)
shinyUI(pageWithSidebar(
  # Application title
  headerPanel('Tweets Analyser'),
  sidebarPanel( textInput('term', 'Enter a term', '#SonuNigam'),
                numericInput('cant', 'Select a number of tweets',1000,0,2000),
                radioButtons('lang','Select the language',c(
                  'English'='en'
                  )),
                submitButton(text='Run')),
  mainPanel(
    
     
    plotOutput('PieC'))
))