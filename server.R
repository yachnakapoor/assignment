library(shiny)

fx <- function(x,y) {
  data <- mtcars
  tmp <- data[data$cyl == x   , ]
  tmp <- data[data$cyl == x &
                data$hp <= y, ]
  out <- mean(tmp$mpg)
  return (out)
}

shinyServer(
  function(input, output) {
    output$mpgId <- renderPrint(fx(input$cyl, input$horse))
  }
)