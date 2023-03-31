# Define server logic required to draw a histogram ----
server <- function(input, output) {
  library("ggmap")
  library("gridExtra")
  plotFaceVector <- function(faceVector,nrow=192,ncol=168) {
    matrix(faceVector,nrow=nrow,ncol=ncol) %>%
      ggimage()
  }
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
  set.seed(11)
  k <- 30
  sY <- sample(k,k/2) %>% sort
  sT <- (1:k)[-sY]
  #sT <- c(sT,(k+1):30)
  
  approxYvesToFind <- yvesSvd$u[,sY] %*%
    diag(yvesSvd$d[sY],ncol=length(sY)) %*%
    t(yvesSvd$v[,sY]) 
  approxTiffToFind <- tiffSvd$u[,sT] %*%
    diag(tiffSvd$d[sT],ncol=length(sT)) %*%
    t(tiffSvd$v[,sT]) 
  approxToFind <- approxTiffToFind+approxYvesToFind
  plotToFind <- plotFaceVector(approxToFind,641,441)
  yvesOrig <- yvesSvd$u %*%diag(yvesSvd$d) %*%t(yvesSvd$v)
  tiffOrig <- tiffSvd$u %*%diag(tiffSvd$d) %*%t(tiffSvd$v)
  yvesOrig <- plotFaceVector(c(yvesOrig),641,441)
  tiffOrig <- plotFaceVector(c(tiffOrig),641,441)
  output$origPlot <-  renderPlot({grid.arrange(tiffOrig,plotToFind,yvesOrig,nrow=1)})
  output$searchPlot <- renderPlot({
  #for (seed in 1:100)
  {
    #correct 211
    set.seed(input$seedH*100+input$seedT*10+input$seedE-200)
    k <- 30
    sY <- sample(k,k/2) %>% sort
    sT <- (1:k)[-sY]
    #sT <- c(sT,(k+1):30)
    
    approxYves <- yvesSvd$u[,sY] %*%
      diag(yvesSvd$d[sY],ncol=length(sY)) %*%
      t(yvesSvd$v[,sY]) 
    approxTiff <- tiffSvd$u[,sT] %*%
      diag(tiffSvd$d[sT],ncol=length(sT)) %*%
      t(tiffSvd$v[,sT]) 
    approxSam <- approxTiff+approxYves
    #saveSam[seed,] <- approxSam
  }
  #approxSam[approxSam < 0.2] <- 0
  #approxSam[approxSam > .5] <- 1
  yvesPlot <- plotFaceVector(approxYves,641,441)
  tiffPlot <- plotFaceVector(approxTiff,641,441)
  samPlot <- plotFaceVector(approxSam,641,441)
  grid.arrange(tiffPlot,samPlot,yvesPlot,nrow=1)
  })
}