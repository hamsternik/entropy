library(shiny)
library(ggplot2)

source("data/permutationByHand.R")

shinyServer(function(input, output) {
  
  getNumericVec <- reactive({
    inpVec <- strsplit(input$series1, 
                       split = ", ")
    inpVecNum <- as.numeric(inpVec[[1]])
  })
  
  output$permutation_plot <- renderPlot({
    qplot(getNumericVec(),
          geom = "histogram",
          binwidth = 5,
          main = "Диаграмма классов",
          fill = I("green"),
          col = I("black"),
          alpha = I(.2),
          xlim = c(0, 10))
  })
})