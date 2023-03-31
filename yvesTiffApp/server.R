# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  library("ggmap")
  library("gridExtra")
  
  plotFaceVector <- function(faceVector,nrow=192,ncol=168) {
    matrix(faceVector,nrow=nrow,ncol=ncol) %>%
      ggimage()
  }
  
  arrowR <- ggplot() +
    geom_segment(aes(x=5, y=0, xend=8, yend=0), arrow = arrow(length=unit(.5, 'cm'))) +
    ylim(-2,2)+
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank())
  arrowL <- ggplot() +
    geom_segment(aes(x=8, y=0, xend=5, yend=0), arrow = arrow(length=unit(.5, 'cm'))) +
    ylim(-2,2)+
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank())
  arrowLQ <- arrowL + annotate("text", x=6.5, y=0.2, label= "?")
  arrowRQ <- arrowR + annotate("text", x=6.5, y=0.2, label= "?")
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
  approxYvesToFind <- yvesSvd$u[,sY] %*%
    diag(yvesSvd$d[sY],ncol=length(sY)) %*%
    t(yvesSvd$v[,sY]) 
  # picture Tiff
  approxTiffToFind <- tiffSvd$u[,sT] %*%
    diag(tiffSvd$d[sT],ncol=length(sT)) %*%
    t(tiffSvd$v[,sT]) 
  # combined
  approxToFind <- approxTiffToFind+approxYvesToFind

  #Make plots 
  plotToFind <- plotFaceVector(approxToFind,641,441)
  yvesOrig <- yvesSvd$u %*%diag(yvesSvd$d) %*%t(yvesSvd$v)
  tiffOrig <- tiffSvd$u %*%diag(tiffSvd$d) %*%t(tiffSvd$v)
  yvesOrig <- plotFaceVector(c(yvesOrig),641,441)
  tiffOrig <- plotFaceVector(c(tiffOrig),641,441)
  output$origPlot <-  renderPlot({
    grid.arrange(tiffOrig,
                 arrowRQ,
                 plotToFind,
                 arrowLQ,
                 yvesOrig,
                 #nrow=1
                 layout_matrix = matrix(c(1,1,1,2,3,3,3,4,5,5,5),nrow=1)
                 )},
    height = function() {
                   if (session$clientData$output_origPlot_width <= 1000) {
                     100
                   } else { "auto" }
                 })
  observeEvent(input$minus, {
    updateSliderInput(session,"seed", value = input$seed - 1)
  })
  
  observeEvent(input$plus, {
    updateSliderInput(session,"seed", value = input$seed + 1)
  })  
  
  output$searchPlot <- renderPlot({
  
    # make plots for trial code from slider
    sY <- which(decimal2binary(h[input$seed+2],length = 30)==1)
    sT <- (1:30)[-sY]
    approxYves <- yvesSvd$u[,sY] %*%
      diag(yvesSvd$d[sY],ncol=length(sY)) %*%
      t(yvesSvd$v[,sY]) 
    approxTiff <- tiffSvd$u[,sT] %*%
      diag(tiffSvd$d[sT],ncol=length(sT)) %*%
      t(tiffSvd$v[,sT]) 
    approxSam <- approxTiff+approxYves
  
  yvesPlot <- plotFaceVector(approxYves,641,441)
  tiffPlot <- plotFaceVector(approxTiff,641,441)
  samPlot <- plotFaceVector(approxSam,641,441)
  grid.arrange(
    tiffPlot,
    arrowR,
    samPlot,
    arrowL,
    yvesPlot,
    #nrow=1
    layout_matrix = matrix(c(1,1,1,2,3,3,3,4,5,5,5),nrow=1)
    )
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
  }, 
  height = function() {
    if (session$clientData$output_searchPlot_width <= 1000) {
      100
    } else { "auto" }
  })
}


