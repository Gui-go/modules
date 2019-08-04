obs_UI <- function(id) {
  ns = NS(id)
  
  list(
    fluidPage(
      theme = "www/MACROdemo.css",
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #FFF; top: 17px; width: 2px;
      height: 2px;
      border: 1px solid #FFF;
      background: #DDD;
      border-radius: 27px;
      -moz-border-radius: 27px;
      box-shadow: 1px 1px 3px #FFF;
      cursor: pointer;
  }")),
      column(2,
             selectInput(
               inputId = ns("SOB"),
               label = "Série:",
               choices = c("SELIC" = 4390,
                           "CDI" = 4391,
                           "IPCA" = 433,
                           "INPC" = 188),
               selected = "SELIC"
             ),
             br(),
             selectInput(
               inputId = ns("MARKOB"),
               label = "Marcadores:",
               multiple = T,
               selectize = T,
               choices = c("Média" = "m1",
                           "Mediana" = "m2"),
               selected = c("m1", "m2")),
             br(),
             sliderInput(
               inputId = ns("SLDOB"),
               label = "Período da série:",
               value = c(164, 392), min = 1 , max = 500, step = 1
             ),
             br(),
             tableOutput(
               ns("PIFOB")
               ),
             br(),
             column(7, h5("Máximo", "Limite Superior", "3º Quartil", "Mediana", "Média", "1* Quartil", "Limite Inferior", "Mínimo", "Variância", "Desvio Padrão", "Amplitude", "Amplitude interquartil", "Assimetria", "Curtose", "P-valor de Bera-Jarque", "P-valor de Shapiro-Wilk", "P-valor de Dickey-Fuller")),
             column(1),
             column(4,
                    h5(uiOutput(
                      ns("STATS_MEDIA_OB")
                    )))
      ),
      column(10,
             tabsetPanel(
               tabPanel(
                 "Gráfico de linha",
                 plotlyOutput(
                   ns("GLOB"), 
                   height = "800px")
                 ),
               tabPanel(
                 "Gráfico de subsérie",
                 plotlyOutput(
                   ns("GGSUB"), 
                   height = "800px")
                 ),
               tabPanel(
                 "Gráfico de densidade",
                 plotlyOutput(
                   ns("DENS"), 
                   height = "800px")
                 ),
               tabPanel(
                 "Decomposição clássica",
                 plotlyOutput(
                   ns("DECOMP"),
                   height = "800px")
               ),
               tabPanel(
                 "ACF",
                 plotlyOutput(
                   ns("ACF"),
                   height = "800px")
               ),
               tabPanel(
                 "Tabela",
                 dataTableOutput(
                   ns("TABLE")
                 ))
             )
      ))
  )
}


# Server ------------------------------------------------------------------
obs_serv <- function(input, output, session) {

# Reactive Values ---------------------------------------------------------
  SERIEOBraw <- reactive({
    SOBraw1 <- Quandl::Quandl(paste0("BCB/", input$SOB), type = "raw", collapse = "monthly", api_key = "gGdvN9gXsx9hxHMTWPNL")
    SOBraw1 <- SOBraw1[seq(dim(SOBraw1)[1],1),]
  })
  
  SERIEOBts <- reactive({
    TS <- ts(SERIEOBraw()[,2], start = c(as.numeric(substring(SERIEOBraw()[1,1],1,4)), as.numeric(substring(SERIEOBraw()[1,1],6,7))), frequency = 12)
  })
  
  output$PIFOB <- renderTable({
    PIF <- tibble("Período Inicial" = paste(substring(SERIEOBraw()[input$SLDOB[1],1],6,7), "/", substring(SERIEOBraw()[input$SLDOB[1],1],1,4)),
                  "-" = paste("-"),
                  "Período Final" = paste(substring(SERIEOBraw()[input$SLDOB[2],1],6,7), "/", substring(SERIEOBraw()[input$SLDOB[2],1],1,4)))
  })

# Gráfico de linha --------------------------------------------------------
  output$GLOB <- renderPlotly({
    GLST <- autoplot(ts(SERIEOBts()[input$SLDOB[1]:input$SLDOB[2]], start = c(as.numeric(substring(SERIEOBraw()[input$SLDOB[1],1],1,4)), as.numeric(substring(SERIEOBraw()[input$SLDOB[1],1],6,7))), frequency = 12))+
      labs(x = "Tempo",
           y = "Y",
           caption = "MACROdemo")
    if(is.null(input$MARKOB)){
      GLST
    } else if(all(c("m1", "m2") %in% input$MARKOB)){
      GLST <- GLST +
        geom_hline(yintercept = mean(SERIEOBts()[input$SLDOB[1]:input$SLDOB[2]]), color = "red")+
        geom_hline(yintercept = median(SERIEOBts()[input$SLDOB[1]:input$SLDOB[2]]), color = "blue")
    } else if(input$MARKOB == "m1" & input$MARKOB != "m2"){
      GLST <- GLST + geom_hline(yintercept = mean(SERIEOBts()[input$SLDOB[1]:input$SLDOB[2]]), color = "red")
    } else if(input$MARKOB != "m1" & input$MARKOB == "m2"){
      GLST <- GLST + geom_hline(yintercept = median(SERIEOBts()[input$SLDOB[1]:input$SLDOB[2]]), color = "blue")
    }
    GLST + theme_minimal()
  })

# Gráfico de subsérie -----------------------------------------------------
  output$GGSUB <- renderPlotly({
    ggsubseriesplot(ts(SERIEOBts()[input$SLDOB[1]:input$SLDOB[2]], start = c(as.numeric(substring(SERIEOBraw()[input$SLDOB[1],1],1,4)), as.numeric(substring(SERIEOBraw()[input$SLDOB[1],1],6,7))), frequency = 12))+
      labs(title = "",
           x = "Tempo",
           y = "Y",
           caption = "MACROdemo") + 
      theme_minimal()
  })
  
# Gráfico de densidade ----------------------------------------------------
  output$DENS <- renderPlotly({
    SS <- data.frame(C = SERIEOBts()[input$SLDOB[1]:input$SLDOB[2]])  
    DENS <- ggplot(SS, aes(as.numeric(SS$C)))+
      geom_density(size = 0.8, fill = "gray", alpha = 0.5)+
      theme_minimal()+
      labs(title = "",
           x = "Observação",
           y = "Densidade",
           caption = "MACROdemo")
    if(is.null(input$MARKOB)){
      DENS
    } else if(all(c("m1", "m2") %in% input$MARKOB)){
      DENS <- DENS +
        geom_vline(xintercept = mean(SS$C), color = "red")+
        geom_vline(xintercept = median(SS$C), color = "blue")
    } else if(input$MARKOB == "m1" & input$MARKOB != "m2"){
      DENS <- DENS + geom_vline(xintercept = mean(SS$C), color = "red")
    } else if(input$MARKOB != "m1" & input$MARKOB == "m2"){
      DENS <- DENS + geom_vline(xintercept = median(SS$C), color = "blue")
    }
    DENS <- ggplotly(DENS)
    DENS
  })

# Decomposição ------------------------------------------------------------

  output$DECOMP <- renderPlotly({
    ggplotly(
      autoplot(
        decompose(
          ts(SERIEOBts()[input$SLDOB[1]:input$SLDOB[2]], start = c(as.numeric(substring(SERIEOBraw()[input$SLDOB[1],1],1,4)), as.numeric(substring(SERIEOBraw()[input$SLDOB[1],1],6,7))), frequency = 12),
          type  = input$SISAD
        )
      )+ theme_minimal()+
        labs(caption = "MACROdemo")
    )
  })  

# ACF ---------------------------------------------------------------------

  output$ACF <- renderPlotly({
    ggplotly(
      ggAcf(
        ts(SERIEOBts()[input$SLDOB[1]:input$SLDOB[2]], start = c(as.numeric(substring(SERIEOBraw()[input$SLDOB[1],1],1,4)), as.numeric(substring(SERIEOBraw()[input$SLDOB[1],1],6,7))), frequency = 12), 
        lag.max = 60)+
        labs(title = "Função de auto-correlação")
    )
  })
    
  output$TABLE <- DT::renderDataTable({
    SP <- tibble(MES = substr(SERIEOBraw()[input$SLDOB[1]:input$SLDOB[2],1], 6 , 7),
                 ANO = substr(SERIEOBraw()[input$SLDOB[1]:input$SLDOB[2],1], 1 , 4),
                 DADOS = SERIEOBraw()[input$SLDOB[1]:input$SLDOB[2], 2])},
    options = list(pageLength = 12)
  )
  
  # output$STATS_MEDIA_OB <- renderText({
  #   round(mean(as.numeric(SERIEOBraw()[input$SLDOB[1]:input$SLDOB[2],2])), 3)
  # })
  # 
  
  output$STATS_MEDIA_OB <- renderUI({
    NN <- SERIEOBraw()[input$SLDOB[1]:input$SLDOB[2],2]
    mylist <- c(round(max(NN), 3),
                round(quantile(NN)[[4]]+1.5*(quantile(NN)[[4]]-quantile(NN)[[2]]), 3),
                round(quantile(NN)[[4]],3), 
                round(median(NN),3), 
                round(mean(NN),3), 
                round(quantile(NN)[[2]],3), 
                round(quantile(NN)[[2]]-1.5*(quantile(NN)[[4]]-quantile(NN)[[2]]),3), 
                round(min(NN),3),
                round(var(NN),3), 
                round(sd(NN),3), 
                round(max(NN)-min(NN),3), 
                round(quantile(NN)[[4]]-quantile(NN)[[2]],3), 
                round(skewness(NN),3), round(kurtosis(NN),3), 
                round(jarque.test(as.vector(NN))[[2]], 3), 
                round(shapiro.test(NN)[[2]], 3) , 
                round(adf.test(NN, k = 12)[[4]], 3))
    HTML(paste(mylist, sep = "", collapse = '<br/>'))
  })
  
  
  
}

