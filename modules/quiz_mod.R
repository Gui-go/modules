quiz_UI <- function(id) {
  ns = NS(id)
  library(shinyjs)

  fluidPage(
    fluidRow(
      h2(strong("Quiz"))
    ),
    br(),
    fluidRow(
      useShinyjs(),
      inlineCSS(list(.green = "background: green")),
      inlineCSS(list(.red = "background: red")),
      h3(strong("Questão 1")),
      br(),
      h4(id = ns("id1"), "1)"),
      br(),
      h4(id = ns("id2"), "2)"),
      br(),
      h4(id = ns("id3"), "3)"),
      br(),
      h4(id = ns("id4"), "4)"),
      br(),
      actionButton(inputId = ns("go1"), label = "Conferir Resposta")
    )
  )
  
}

quiz_serv <- function(input, output, session) {
  observeEvent(input$go1, {
    toggleClass("id1", "red")
    toggleClass("id2", "red")
    toggleClass("id3", "green")
    toggleClass("id4", "red")
  })
}



# library(shiny)
# library(shinyjs)
# shinyApp(
#   ui = fluidPage(
#     useShinyjs(),  # Set up shinyjs
#     # Add a CSS class for red text colour
#     inlineCSS(list(.red = "background: red")),
#     actionButton("btn", "Click me"),
#     p(id = "element", "Watch what happens to me")
#   ),
#   server = function(input, output) {
#     observeEvent(input$btn, {
#       # Change the following line for more examples
#       toggleClass("element", "red")
#     })
#   }
# )