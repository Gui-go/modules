sobre_UI <- function(id) {
  ns = NS(id)
  
  list(
    br(),
    br(),
    br(),
    h1(strong("Guilherme Viegas")),
    br(),
    h3("guilhermeviegas1993@gmail.com"),
    tags$a(href = "https://guilhermeviegas.portfoliobox.net/", h3("Guigo's Portifólio")),
    br()
  )
}

sobre_serv <- function(input, output, session) {
  
}