library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  # Reactive -> transform input string 
  #             of numbers into integer vector 
  getNumericVector <- reactive({
    inpVec <- strsplit(input$series1, 
                       split = " ")
    inpVecNum <- as.numeric(inpVec[[1]])
  })
  
  
  # Reactive -> get the threshold value
  getTreshold <- reactive({
    thershold <- as.numeric(input$threshold)
    writeToIniFile(thershold, "data/thresholdValue.ini")
    return (thershold)
  })
  
  # Function
  writeToIniFile <- function(value, valuePath) {
    if (!file.exists(valuePath))
      file.create(valuePath)
    cat(value, file = valuePath, sep = "\n", append = FALSE)
  }
  
  # Function
  getSerSize <- function() {
    
    GROUP_NUMBER <- 3
    inputVector <- getNumericVector()
    
    serSize <- length(inputVector) - GROUP_NUMBER + 1
    return (serSize)
  }
  
  
  # Function
  getPatternsList <- function() {
    
    GROUP_NUMBER <- 3
    inputVector <- getNumericVector()
    
    serSize <- getSerSize()
    inpMatrix <- matrix(0, serSize, 3)
    for (i in 1:serSize) {
      inpMatrix[i,] <- c(inputVector[i], inputVector[i+1], inputVector[i+2])
    }
    
    # Patterns list which contains all predefined patterns and corresponding values
    patternsList = list(exUp = 0, exDown = 0, monUp = 0, monDown = 0, con = 0)
    
    h <- getTreshold()
    absEdge <- abs(h * mean(inputVector))
    for (i in 1:serSize) {
      if ((inpMatrix[i,2] - inpMatrix[i,1] > h) && (inpMatrix[i,2] - inpMatrix[i,3] > h)) {
        patternsList$exUp <- patternsList$exUp + 1
      } else if ((inpMatrix[i,1] - inpMatrix[i,2] > h) && (inpMatrix[i,3] - inpMatrix[i,2] > h)) {
        patternsList$exDown <- patternsList$exDown + 1
      } else if ((inpMatrix[i,2] - inpMatrix[i,1] > h) 
                 || (inpMatrix[i,3] - inpMatrix[i,2] > h) || (inpMatrix[i,3] - inpMatrix[i,1] > h)) {
        patternsList$monUp <- patternsList$monUp + 1
      } else if ((inpMatrix[i,1] - inpMatrix[i,2] > h) 
                 || (inpMatrix[i,2] - inpMatrix[i,3] > h) || (inpMatrix[i,1] - inpMatrix[i,3] > h)) {
        patternsList$monDown <- patternsList$monDown + 1
      } else {
        patternsList$con <- patternsList$con +1 
      }
    }
    
    return (patternsList)
  }
  
  
  # Function
  getVectorOfPatternsListFreq <- function() {
    patternsList <- getPatternsList()
    
    # Addition temporal vector 
    # which contains frequency of all patterns
    vecOfPatternsList <- c()
    for (i in patternsList)
      vecOfPatternsList <- append(vecOfPatternsList, i)
    
    # Frequency vector for all numbers 
    # corresponds amount of values from each group
    vecOfPatternsListFreq <- c()
    for (i in 1:length(vecOfPatternsList)) {
      for (j in 1:vecOfPatternsList[i]) {
        vecOfPatternsListFreq <- append(vecOfPatternsListFreq, i)
        if (j == vecOfPatternsList[i])
          vecOfPatternsListFreq <- append(vecOfPatternsListFreq, NA)
      }
    }
    
    return (vecOfPatternsListFreq)
  }
  
  
  # Function
  getPermutationEntropyValue <- function() {
    patternsList <- getPatternsList()
    serSize <- getSerSize()
    
    genSum <- 0
    for (i in names(patternsList)) {
      patternsList[[i]] <- patternsList[[i]] / serSize
      if (patternsList[[i]] != 0) {
        genSum <- genSum - (patternsList[[i]] * log2(patternsList[[i]])) 
      }
    }
    
    return (genSum)
  }
  
  # Output -> plot which show input time series
  output$TimeSeriesPlot <- renderPlot({
    heading = paste("Time Series Plot")  
    y <- getNumericVector()
    x <- seq(from=1, to=length(y))
    qplot(x, y, 
          geom = "line",
          main = heading,
          col = I("blue"),
          alpha = I(.5))
  })
  
  
  # Output -> plot which show every group's percent 
  #           from general one
  output$GroupsFrequencyBar <- renderPlot({
    heading = paste("Group's percent from general amount")  
    y <- getVectorOfPatternsListFreq()
    qplot(y,
          geom = "bar",
          main = heading,
          fill = I("green"),
          col = I("red"),
          alpha = I(.2))
  })
  
  
  # Output
  output$PermutationEntropyValue <- renderText({
    paste("Permutation entropy of input chaotic time series is equal: ", 
          getPermutationEntropyValue())
  })
  
})