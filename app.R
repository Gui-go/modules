library(shiny)
library(tidyverse)
library(plotly)
library(ggfortify)
library(Quandl)
library(DT)
library(moments)
library(tseries)

source("modules/button_mod.R", encoding = "utf8")
source("modules/fc_mod.R", encoding = "utf8")
source("modules/islm_mod.R", encoding = "utf8")
source("modules/obs_mod.R", encoding = "utf8")


ui <- shinyUI(
  navbarPage(title = "MACROdemo",
    tabPanel(
      "Apresentação",
      ap_UI("ap")),
    navbarMenu(
      "Gráficos interatívos",
      tabPanel(
        "Função Consumo",
        fc_UI("funcao_consumo")
       ),
      tabPanel(
        "IS-LM",
        islm_UI("is_lm")),
      tabPanel("Mercado de trabalho"),
      tabPanel("Curva de Philips")
     ),
    navbarMenu(
      "Animações",
      tabPanel("Oferta = Demanda"),
      tabPanel("Mercado de trabalho")
    ),
    tabPanel(
      "Observatório",
      obs_UI("observatorio")
      ),
    tabPanel("Quiz"),
    tabPanel("Sobre")
  )
)

server <- function(input, output, session) {
  callModule(button, "first")
  callModule(button, "second")
  callModule(fc_serv, "funcao_consumo")
  callModule(islm_serv, "is_lm")
  callModule(obs_serv, "observatorio")
}

shinyApp(ui, server)