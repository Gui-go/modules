library(shiny)
library(tidyverse)
library(plotly)
library(ggfortify)
library(Quandl)
library(DT)
library(moments)
library(tseries)
library(forecast)
library(shinydashboard)
library(shinydashboardPlus)

source("modules/ap_mod.R", encoding = "utf8")
source("modules/fc_mod.R", encoding = "utf8")
source("modules/islm_mod.R", encoding = "utf8")
source("modules/foa_fda_is_gif_mod.R", encoding = "utf8")
source("modules/quiz_mod.R", encoding = "utf8")
source("modules/obs_mod.R", encoding = "utf8")
source("modules/sobre_mod.R", encoding = "utf8")




ui <- shinyUI(
  navbarPage(title = "MACROdemo  0.51",
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
      tabPanel(
        "f(OA) = f(DA) >>> IS",
        foa_fda_is_UI("foa_fda_is")),
      tabPanel("Mercado de trabalho"),
      tabPanel("Inflation is always and everywhere amonetary phenomena")
    ),
    tabPanel(
      "Observatório",
      obs_UI("observatorio")
      ),
    tabPanel(
      "Quiz",
      quiz_UI("quiz")),
    tabPanel(
      "Sobre",
      sobre_UI("sobre"))
  )
)






server <- function(input, output, session) {
  callModule(fc_serv, "funcao_consumo")
  callModule(islm_serv, "is_lm")
  callModule(foa_fda_is_serv, "foa_fda_is")
  callModule(quiz_serv, "quiz")
  callModule(obs_serv, "observatorio")
  callModule(sobre_serv, "sobre")
}

shinyApp(ui, server)