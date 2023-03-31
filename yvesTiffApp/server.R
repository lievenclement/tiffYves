# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  library("ggmap")
  library("gridExtra")
  
  plotFaceVector <- function(faceVector,nrow=192,ncol=168) {
    matrix(faceVector,nrow=nrow,ncol=ncol) %>%
      ggimage()
  }
  
  decimal2binary <- function (x, length) 
  {
    # CODE from GA package
    x <- as.integer(x)
    b <- if (missing(length)) 
      NULL
    else rep(0, length)
    i <- 0
    while (x >= 1) {
      i <- i + 1
      b[i] <- x%%2
      x <- x%/%2
    }
    return(rev(b))
  }

# Construct SVDs of original images
#  tiff <- read.bitmap("media/IMG_1638.jpeg")[,,1]
#  yves <- read.bitmap("media/IMG_1637.jpeg")[,,1]
#  yves <- yves[(1:641),(1:641)]
#  yves2 <- yves
#  yves2[15:641,1:627] <- yves[1:627,15:641]
#  plotFaceVector(c(yves),641,641)
#  yves <- yves2
#  yves <- yves[,101:541]
#  tiff <- tiff[,101:541]
#  yvesSvd <- svd(yves)
#  tiffSvd <- svd(tiff)

  yvesSvd <- readRDS("media/yvesSvd.rds")
  tiffSvd <- readRDS("media/tiffSvd.rds")

  # 30 eigenvectors selected using a binary number of length 30. 
  # 1000 codes can are required. 
  h <- as.integer(seq(0,1073741823,length.out=1002))

  # Our Hybrid
  code <- 409
  # select eigenvectors Yves 
  sY <- which(decimal2binary(h[code+2],length = 30)==1)
  # select eigenvectors Tiff
  sT <- (1:30)[-sY]
  
  # picture Yves
  yvesOrig <- yvesSvd$u %*%diag(yvesSvd$d) %*%t(yvesSvd$v)
  approxYvesToFind <- yvesSvd$u[,sY] %*%
    diag(yvesSvd$d[sY],ncol=length(sY)) %*%
    t(yvesSvd$v[,sY]) 
  
  # picture Tiff
  tiffOrig <- tiffSvd$u %*%diag(tiffSvd$d) %*%t(tiffSvd$v)
  approxTiffToFind <- tiffSvd$u[,sT] %*%
    diag(tiffSvd$d[sT],ncol=length(sT)) %*%
    t(tiffSvd$v[,sT]) 
  
  # combined
  approxToFind <- approxTiffToFind+approxYvesToFind

  #Make plots 
  output$yvesOrig <-  renderPlot({
    plotFaceVector(c(yvesOrig),641,441)
  })
  output$tiffOrig <-  renderPlot({
    plotFaceVector(c(tiffOrig),641,441)
  })
  output$plotToFind <- renderPlot({
    plotFaceVector(approxToFind,641,441)
  })

  observeEvent(input$minus, {
    updateSliderInput(session,"seed", value = input$seed - 1)
  })
  
  observeEvent(input$plus, {
    updateSliderInput(session,"seed", value = input$seed + 1)
  })
  
  output$approxYves <- renderPlot({
    sY <- which(decimal2binary(h[input$seed+2],length = 30)==1)
    approxYvesTest <- yvesSvd$u[,sY] %*%
      diag(yvesSvd$d[sY],ncol=length(sY)) %*%
      t(yvesSvd$v[,sY]) 
    plotFaceVector(approxYvesTest,641,441)
    })
  
  
  output$approxTiff <- renderPlot({
    sY <- which(decimal2binary(h[input$seed+2],length = 30)==1)
    sT <- (1:30)[-sY]
    approxTiffTest <- tiffSvd$u[,sT] %*%
      diag(tiffSvd$d[sT],ncol=length(sT)) %*%
      t(tiffSvd$v[,sT]) 
    plotFaceVector(approxTiffTest,641,441)
  })
  
 
  
  output$approxSam <- renderPlot({
    sY <- which(decimal2binary(h[input$seed+2],length = 30)==1)
    approxYvesTest <- yvesSvd$u[,sY] %*%
      diag(yvesSvd$d[sY],ncol=length(sY)) %*%
      t(yvesSvd$v[,sY]) 
    
    sT <- (1:30)[-sY]
    approxTiffTest <- tiffSvd$u[,sT] %*%
      diag(tiffSvd$d[sT],ncol=length(sT)) %*%
      t(tiffSvd$v[,sT]) 
    plotFaceVector(approxTiffTest + approxYvesTest,641,441)
  })
  
  output$printCode <- renderText({     
    paste0("YOUR CODE: 0", 
           ifelse(input$seed>99,
                  input$seed,
                  ifelse(input$seed>9,
                         paste0("0",input$seed),
                         paste0("00",input$seed)
                  )
           )
    )
  })
}


