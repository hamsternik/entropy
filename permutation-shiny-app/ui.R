library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  # Application title
  titlePanel("Перестановочная энтропия"),
  
  # Siderbar with UI elements
  sidebarLayout(
    
    sidebarPanel(
      
      textInput("series1", 
                label = "Числовой ряд", 
                value = ""),
      
      numericInput("threshold", 
                   label = "Корректирующий параметр",
                   value = 5,
                   min = 0,
                   max = 50,
                   step = 1),
      
      actionButton("submit", 
                   label = "Вычисление",
                   icon = NULL),
      
      # Use html 'br' tags for indentation
      br(), br(),
      
      selectInput(
        "plotType", "Тип графика",
        c(Scatter = "scatter",
          Histogram = "hist")),
      
      # Maybee, it's not need {{{
      
        # Only show this panel if the plot type is a histogram
        conditionalPanel(
          condition = "input.plotType == 'hist'",
          selectInput(
            "breaks", "Breaks",
            c("Sturges",
              "Scott",
              "Freedman-Diaconis",
              "[Custom]" = "custom")),
          
          # Only show this panel if Custom is selected
          conditionalPanel(
            condition = "input.breaks == 'custom'",
            sliderInput("breakCount", "Break Count", min=1, max=1000, value=10)
          )
        )
      
      # }}}
      
    ),
    
    mainPanel(
      plotOutput("TimeSeriesPlot"),
      plotOutput("GroupsFrequencyBar"),
      textOutput("PermutationEntropyValue")
    )
  )
))