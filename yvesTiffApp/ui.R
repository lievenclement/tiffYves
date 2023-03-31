library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Which Tiff & Yves hybrid unlocks your bucks!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "seed",
                  label = "Change the seed to generate a new plot:",
                  min = 1,
                  max = 999,
                  value = 397)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      h6("From the Tiff & Yves mariage pics we constructed a Tiff & Yves hybrid using the singular value decomposition. 
         We only use the first 30 components. We randomly took 15 components of Tiff and the other 15 from Yves. 
         Unlock your bucks by changing the seed to randomly generate your new Tiff and Yves hybrid"),
      plotOutput(outputId = "searchPlot"),
      h6("The original plots and the Tiff & Yves hybrid you need to reconstruct to unlock your bucks"),
      plotOutput(outputId = "origPlot"),
      h6("Copyright on original images by Yves Gabriëls.")
    )
  )
)