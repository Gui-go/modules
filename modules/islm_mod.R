islm_UI <- function(id) {
  ns = NS(id)
  
  list(
    fluidRow(
      column(2,
             tabsetPanel(
               tabPanel("IS_1",
                        column(12,
                               br(),
                               numericInput(ns("ISLMc0_1"), "Consumo Autonomo", min = 0, max = 1000, step = 50, value = 450),
                               numericInput(ns("ISLMc1_1"), "Propensão Marginal a Consumir", min = 0, max = 1, step = 0.1, value = 0.8),
                               #numericInput(ns("ISLMT0_1"), "T0", min = 0, max = 1000, step = 50, value = 300),
                               numericInput(ns("ISLMI0_1"), "Investimento autônomo", min = 0, max = 1000, step = 50, value = 300),
                               numericInput(ns("ISLMG0_1"), "Gastos do governo", min = 0, max = 1000, step = 50, value = 300),
                               numericInput(ns("ISLMa_1"), "a", min = 0, max = 1, step = 0.1, value = 0.1),
                               numericInput(ns("ISLMt_1"), "Tributos", min = 0, max = 1, step = 0.1, value = 0.3))),
               tabPanel("LM_1",
                        column(12,
                               br(),
                               br(),
                               br(),
                               numericInput(ns("ISLMl0_1"), "Precaução", min = 0, max = 1, step = 0.05, value = 0.9),
                               numericInput(ns("ISLMl1_1"), "Sensibilidade da taxa de juros a demanda por moeda", min = 0, max = 1, step = 0.1, value = 0.3),
                               numericInput(ns("ISLMms0p_1"), "oferta de moeda", min = 0, max = 6000, step = 50, value = 3000),
                               numericInput(ns("ISLMvt_1"), "velocity", min = 0, max = 1000, step = 0.1, value = 0.1))),
               tabPanel("IS_2",
                        column(12,
                               br(),
                               numericInput(ns("ISLMc0_2"), "Consumo Autonomo", min = 0, max = 1000, step = 50, value = 450),
                               numericInput(ns("ISLMc1_2"), "Propensão Marginal a Consumir", min = 0, max = 1, step = 0.1, value = 0.8),
                               # numericInput(ns("ISLMT0_2"), "T0", min = 0, max = 1000, step = 50, value = 300),
                               numericInput(ns("ISLMI0_2"), "Investimento autônomo", min = 0, max = 1000, step = 50, value = 300),
                               numericInput(ns("ISLMG0_2"), "Gastos do governo", min = 0, max = 1000, step = 50, value = 300),
                               numericInput(ns("ISLMa_2"), "a", min = 0, max = 1, step = 0.1, value = 0.1),
                               numericInput(ns("ISLMt_2"), "Tributos", min = 0, max = 1, step = 0.1, value = 0.3))),
               tabPanel("LM_2",
                        column(12,
                               br(),
                               br(),
                               br(),
                               numericInput(ns("ISLMl0_2"), "Precaução", min = 0, max = 1, step = 0.05, value = 0.9),
                               numericInput(ns("ISLMl1_2"), "Sensibilidade da taxa de juros a demanda por moeda", min = 0, max = 1, step = 0.1, value = 0.3),
                               numericInput(ns("ISLMms0p_2"), "oferta de moeda", min = 0, max = 10000, step = 50, value = 3000),
                               numericInput(ns("ISLMvt_2"), "velocity", min = 0, max = 1000, step = 0.1, value = 0.1)))
             )),
      column(10,
             plotOutput(ns("ISLMgraph"), height = "800px")
      )
      
    )
  )
}



islm_serv <- function(input, output, session) {
  
  
  output$ISLMgraph <- renderPlot({
    ISii_1 = NULL
    ISc0_1 = input$ISLMc0_1
    ISc1_1 = input$ISLMc1_1
    IST0_1 = input$ISLMT0_1
    ISI0_1 = input$ISLMI0_1
    ISG0_1 = input$ISLMG0_1
    ISa_1 = input$ISLMa_1
    ISt_1 = input$ISLMt_1
    LMii_1 = NULL
    LMl0_1 = input$ISLMl0_1
    LMl1_1 = input$ISLMl1_1
    LMms0p_1 = input$ISLMms0p_1
    LMvt_1 = input$ISLMvt_1
    
    ISii_2 = NULL
    ISc0_2 = input$ISLMc0_2
    ISc1_2 = input$ISLMc1_2
    IST0_2 = input$ISLMT0_2
    ISI0_2 = input$ISLMI0_2
    ISG0_2 = input$ISLMG0_2
    ISa_2 = input$ISLMa_2
    ISt_2 = input$ISLMt_2
    LMii_2 = NULL
    LMl0_2 = input$ISLMl0_2
    LMl1_2 = input$ISLMl1_2
    LMms0p_2 = input$ISLMms0p_2
    LMvt_2 = input$ISLMvt_2
    
    y = c(1:2000)
    
    for (i in seq_along(y)) {
      # ISii_1[i] <- ((input$ISLMc0_1 - (input$ISLMc1_1*input$ISLMT0_1) + input$ISLMI0_1 + input$ISLMG0_1) / input$ISLMa_1)-((1-input$ISLMc1_1*(1-input$ISLMt_1))/input$ISLMa_1)*y[i]
      ISii_1[i] <- c(((input$ISLMc0_1 + input$ISLMI0_1 + input$ISLMG0_1) - (1-input$ISLMc1_1*(1-input$ISLMt_1))*y[i])/input$ISLMa_1)
    }
    for (i in seq_along(y)) {
      # LMii_1[i] <- c((1/input$ISLMl1_1)*(input$ISLMl0_1-input$ISLMms0p_1)+(1/input$ISLMl1_1)*(1/input$ISLMvt_1)*y[i])
      LMii_1[i] <- c(((1/input$ISLMvt_1)*y[i] - input$ISLMms0p_1) / input$ISLMl1_1)
    }
    
    for (i in seq_along(y)) {
      # ISii_2[i] <- ((input$ISLMc0_2 - (input$ISLMc1_2*input$ISLMT0_2) + input$ISLMI0_2 + input$ISLMG0_2) / input$ISLMa_2)-((1-input$ISLMc1_2*(1-input$ISLMt_2))/input$ISLMa_2)*y[i]
      ISii_2[i] <- (((input$ISLMc0_2 + input$ISLMI0_2 + input$ISLMG0_2) - (1-input$ISLMc1_2*(1-input$ISLMt_2))*y[i])/input$ISLMa_2)
    }
    for (i in seq_along(y)) {
      LMii_2[i] <- c((1/input$ISLMl1_2)*(input$ISLMl0_2-input$ISLMms0p_2)+(1/input$ISLMl1_2)*(1/input$ISLMvt_2)*y[i])
    }
    
    dfISLM <- data.frame(ISii_1, LMii_1, y, ISii_2, LMii_2)
    
    # coef(lm(ISii_1 ~ y))[[2]] X + 1Y = coef(lm(ISii_1 ~ y))[[1]]
    # coef(lm(LMii_1 ~ y))[[2]] X + 1Y = coef(lm(LMii_1 ~ y))[[1]]
    
    M1coef_1 <- matrix(c(coef(lm(ISii_1 ~ y))[[2]],
                         1,
                         coef(lm(LMii_1 ~ y))[[2]],
                         1),
                       nrow = 2,
                       byrow = T)
    
    M1cons_1 <- matrix(c(coef(lm(ISii_1 ~ y))[[1]],
                         coef(lm(LMii_1 ~ y))[[1]]),
                       nrow = 2)
    
    M1coef_2 <- matrix(c(coef(lm(ISii_2 ~ y))[[2]],
                         1,
                         coef(lm(LMii_2 ~ y))[[2]],
                         1),
                       nrow = 2,
                       byrow = T)
    
    M1cons_2 <- matrix(c(coef(lm(ISii_2 ~ y))[[1]],
                         coef(lm(LMii_2 ~ y))[[1]]),
                       nrow = 2)
    
    ggplot(dfISLM)+
      geom_line(aes(x = dfISLM$y, y = dfISLM$ISii_2), size =1.3, color = "#949494")+
      geom_line(aes(x = dfISLM$y, y = dfISLM$LMii_2), size =1.3, color = "#949494")+
      geom_line(aes(x = dfISLM$y, y = dfISLM$ISii_1), size =1.3)+
      geom_line(aes(x = dfISLM$y, y = dfISLM$LMii_1), size =1.3)+
      geom_segment(aes(x = -solve(M1coef_2, M1cons_2)[1], xend = -solve(M1coef_2, M1cons_2)[1], y = 0, yend = solve(M1coef_2, M1cons_2)[2]), linetype = 2, color = "#949494")+
      geom_segment(aes(x = 0, xend = -solve(M1coef_2, M1cons_2)[1], y = solve(M1coef_2, M1cons_2)[2], yend = solve(M1coef_2, M1cons_2)[2]), linetype = 2, color = "#949494")+
      geom_segment(aes(x = -solve(M1coef_1, M1cons_1)[1], xend = -solve(M1coef_1, M1cons_1)[1], y = 0, yend = solve(M1coef_1, M1cons_1)[2]), linetype = 2)+
      geom_segment(aes(x = 0, xend = -solve(M1coef_1, M1cons_1)[1], y = solve(M1coef_1, M1cons_1)[2], yend = solve(M1coef_1, M1cons_1)[2]), linetype = 2)+
      geom_point(aes(x = -solve(M1coef_2, M1cons_2)[1], y = solve(M1coef_2, M1cons_2)[2]), size = 6, color = "#949494")+
      geom_point(aes(x = -solve(M1coef_1, M1cons_1)[1], y = solve(M1coef_1, M1cons_1)[2]), size = 6)+
      geom_segment(aes(x = 0, xend = 0, y = solve(M1coef_1, M1cons_1)[2], yend = solve(M1coef_2, M1cons_2)[2]), size = 1.3)+
      geom_segment(aes(x = -solve(M1coef_1, M1cons_1)[1], xend = -solve(M1coef_2, M1cons_2)[1], y = 0, yend = 0), size = 1.3)+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 0)+
      labs(title = "IS - LM",
           x = "Renda",
           y = "Juros",
           caption = "MACROdemo")+
      xlim(-1, 1000)+
      ylim(-1,18000)+
      theme_minimal()+
      theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
            #axis.title = element_text(size = 25, face = "bold"),
            plot.caption = element_text(size = 18, face = "bold"),
            #axis.text = element_blank(),
            axis.ticks = element_blank())
  })
  
  
  
}