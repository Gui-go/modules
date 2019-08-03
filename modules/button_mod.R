button_UI <- function(id) {
  ns = NS(id)
  
  list(
    textOutput(ns("output_area")),
    actionButton(
      ns("next_num"), 
      "Click to generate a random number"
    )
  )
}

button <- function(input, output, session) {
  observeEvent(input$next_num, {
    output$output_area <- renderText({
      rnorm(1)
    })
  })
}