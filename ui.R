library(shiny)
shinyUI(
  pageWithSidebar(
    headerPanel("Motor Trend Car Road Tests"),
    sidebarPanel(
      h4('The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973-74 models).'),
      h4('Select he number of cylinders and /or the maximum Horse Power and we shall show you the corresponding average  Miles/Gallon'),
      
      h3("Enter the data"),
      
      radioButtons("cyl", "Number of cylinders:",
                   c("4" = "4",
                     "6" = "6",
                     "8" = "8")),
      
      numericInput('horse', 'Horse Power input: ', 140, min = 70, max = 250, step = 10)			   
    ),
    mainPanel(
      h4("Miles/(US) gallon"),
      verbatimTextOutput("mpgId")
    )
  ))