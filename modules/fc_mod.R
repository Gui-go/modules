fc_UI <- function(id) {
  ns = NS(id)
  
  list(
    fluidPage(
      column(2,
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             numericInput(inputId = ns("C0"),label = "Consumo autônomo", value = 30, min = 1, max = 100, step = 10),
             numericInput(inputId = ns("C1"), label = "Propensão Marginal a Consumir", value = 0.6, min = 0, max = 1, step = 0.1)),
      column(10,
             plotOutput(outputId = ns("FC"), height = "800px")
             )
    )
  )
}


fc_serv <- function(input, output, session) {
  output$FC <- renderPlot({
    C = NULL
    C0 = input$C0
    C1 = input$C1
    YD = c(1:200)
    for (i in seq_along(YD)) {
      C[i]  <- input$C0+input$C1*YD[i]
    }
    df<- data.frame(YD, C)
    ggplot(df)+
      geom_line(aes(df$YD, df$C), size =1.3)+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 0)+
      labs(title = "Função Consumo",
           x = "Renda, Y",
           y = "Consumo, C",
           caption = "MACROdemo")+
      xlim(-1, 100)+
      ylim(-1,100)+
      theme_minimal()+
      theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 25, face = "bold"),
            plot.caption = element_text(size = 18, face = "bold"),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  })
}

