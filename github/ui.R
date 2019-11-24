
library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Depression analysis in tweets"),
  
  # Getting User Inputs
  
  sidebarPanel(
    textInput("searchTerm", "Enter username"),
    sliderInput(
      "maxTweets",
      "Number of recent tweets to use for analysis:",
      min = 5,
      max = 1000,
      value = 500
    ),
    submitButton(text = "Analyse")
    
  ),
  
  mainPanel(tabsetPanel(
    
    tabPanel(
      "Histogram",
      HTML
      (
        "<div><h3> Histograms graphically depict the depressed and non depressed remarks about the twitter user's tweets
        </h3></div>"
      ),
      plotOutput("plot")
      ),
    
    
    tabPanel(
      "Table",
      HTML(
        "<div><h3> Storing the Tweets associated with the twitter user in Tabular Format </h3></div>"
      ),
      tableOutput("tabledata"),
      HTML (
        "<div><h4> This table depicts the depressive state of the Tweets associated with the twitter user. </h4></div>"
      )
    ),
    
    tabPanel(
      "WordCloud",
      HTML("<div><h3>Most used words </h3></div>"),
      plotOutput("word"),
      HTML
      (
        "<div><h4> A Wordcloud is a visual representation of text data, typically used to depict keyword metadata (tags) on websites, or to visualize free form text.
        This format is useful for quickly perceiving the most prominent terms and for locating a term alphabetically to determine its relative prominence.
        </h4></div>"
      )
    )
    
    
  )
    
    
      )#end of tabset panel)#end of main panel
    
    ))#end of shinyUI