library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  titlePanel("Yves' & Tiff's hybrid breeding programme to unlock your bucks!"),
  
  fluidRow(
    splitLayout(
      plotOutput("tiffOrig"),
      plotOutput("plotToFind"),
      plotOutput("yvesOrig")
      )),
  fluidRow(
    column(width=1),
    column(width=10,
    h4("Unlock your bucks by cloning our VIH (Very Important Hybrid).
      You can breed your own hybrids with the slider!"),  
    br(),
    
    sliderInput(inputId = "seed",
                label="",
                min = 0,
                max = 999,
                value = 0, step=1),

    actionButton("minus", "-"),
    actionButton("plus", "+"),
    br())),
  
    fluidRow(
    splitLayout(
      plotOutput("approxTiff"),
      plotOutput("approxSam"),
      plotOutput("approxYves")    
      )),
  
  fluidRow(
    column(width=1),
    column(width=10,    
           h4("Hybrid showdown"),
           span(textOutput("printCode"), 
                style ="font-size:15px; font-family:arial"),
           br())
    ),
    
  fluidRow(
    splitLayout(
      plotOutput("plotToFindComp"),
      plotOutput("approxSamComp")    
      )),
  fluidRow(
    column(width=1),
    column(width=10,  
           h6("Yves Gabriëls holds the copyright on the stock-images of the newly-weds on who we had to rely to breed our VIH and to provide our hybrid breeding progamme. The rds files and images of newly-bred hybrids are not allowed to be redistributed without permission of the copyright holder.")
           )
  )
)
