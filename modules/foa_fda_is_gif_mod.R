foa_fda_is_UI <- function(id) {
  ns = NS(id)
  
  list(
    fluidPage(
      column(12,
             imageOutput(ns("foa_fda_is_output")), offset = 4)
    )
  )
}

foa_fda_is_serv <- function(input, output, session) {
  output$foa_fda_is_output <- renderImage({
    
    list(src = "animations/eq_is_fd_fo_gif.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = F)}




# rm(list = ls())
# 
# # Libraries ---------------------------------------------------------------
# 
# library("ggplot2")
# library("dplyr")
# library("gganimate")
# library("gifski")
# library("ggthemes")
# library("gridExtra")
# library("magick")
# 
# zeromil <- c(0:1000)
# 
# income <- c(0:1000)
# production <- c(1*income)
# 
# I <- 80
# G <- 80
# c0 <- 150
# c1 <- 0.4
# C <- c(c0+c1*income)
# demand <- c(c0+I+G+c1*income)
# A <- c(310, 510)
# 
# A1 <- matrix(c(1, 1, -1, -0.4), nrow = 2, ncol = 2)
# B1 <- matrix(c(0,310), nrow = 2, ncol = 1)
# EP1x <- solve(A1,B1)[1]
# EP1y <- solve(A1,B1)[2]
# 
# A2 <- matrix(c(1, 1, -1, -0.4), nrow = 2, ncol = 2)
# B2 <- matrix(c(0,510), nrow = 2, ncol = 1)
# EP2x <- solve(A2,B2)[1]
# EP2y <- solve(A2,B2)[2]
# 
# dados <-data.frame(
#   Dzeromil = c(zeromil[1], zeromil[length(zeromil)]),
#   Dincomei = income[1],
#   Dincomef = income[length(income)],
#   Dproductioni = production[1],
#   Dproductionf = production[length(production)],
#   DI = I,
#   DG = G,
#   Dc0 = c0,
#   Dc1 = c1,
#   DCi = C[1],
#   DCf = C[length(C)],
#   DA = A,
#   Ddemand1i = demand[1],
#   Ddemand1f = demand[length(demand)],
#   Ddemand2i = c(demand[1], demand[1]+200),
#   Ddemand2f = c(demand[length(demand)], demand[length(demand)]+200),
#   DEP1x = EP1x,
#   DEP1y = EP1y,
#   DEP2x = c(EP1x, EP2x),
#   DEP2y = c(EP1y, EP2y),
#   Dproduction = "Oferta",
#   Ddemand = "Demanda",
#   Dgasto_autonomo = "Gasto Autônomo",
#   FR = c(1,2)
# )
# 
# 
# # Teste
# ggplot(dados)+ # OA = DA
#   geom_segment(aes(x = 10, xend = 10, y = 1, yend = DA), color = "darkgreen", size = 1.6)+
#   geom_segment(aes(x = Dzeromil[1], xend = Dzeromil[1], y = Dzeromil[1], yend = Dzeromil[2]))+
#   geom_segment(aes(x = Dzeromil[1], xend = Dzeromil[2], y = Dzeromil[1],yend = Dzeromil[1]))+
#   geom_segment(aes(x = Dincomei[1], xend = Dincomef[2], y = Dproductioni[1], yend = Dproductionf[2]), size = 1.2)+
#   geom_segment(aes(x = Dincomei[1], xend = Dincomef[2], y = Ddemand2i, yend = Ddemand2f), size = 1.2)+
#   geom_segment(aes(x = DEP2x, xend = DEP2x, y = 0, yend = DEP2y), linetype = 2)+
#   geom_segment(aes(x = 0, xend = DEP2x, y = DEP2y, yend = DEP2y), linetype = 2)+
#   geom_point(aes(x = c(DEP2x), y = DEP2y), size = 5)+
#   geom_text(aes(c(DEP2x-20), c(DEP2y+45), label = paste("EQ")), parse = TRUE, size = 5)+
#   geom_text(aes(c(30), c(DEP2y/2.3), label = paste("A")), parse = TRUE, size = 5)+
#   geom_text(aes(c(300), c(470, 670), label = paste(Ddemand)), parse = TRUE, angle = 17, size = 5)+
#   geom_text(aes(c(380), c(356), label = paste(Dproduction)), parse = TRUE, angle = 44, size = 5)+
#   theme_minimal()+
#   labs(title = "Equilíbrio nas funções de Oferta e Demanda Agregada",
#        y = "Demanda Agregada, Z; Produção Agregada, Y",
#        x = "Renda, Y",
#        caption = "Econometria \nIndependente")+
#   theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
#         axis.title = element_text(size = 8, face = "bold"),
#         plot.caption = element_text(size = 8, face = "bold"),
#         axis.text = element_blank(),
#         axis.ticks = element_blank())+
#   transition_reveal(FR)
# 
# 
# 
# 
# 
# 
# g1 <- ggplot(dados)+
#   geom_segment(aes(x = 10, xend = 10, y = 1, yend = c(310, 510)), color = "darkgreen", size = 1.6)+
#   geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1000))+
#   geom_segment(aes(x = 0, xend = 1000, y = 0,yend = 0))+
#   geom_segment(aes(x = 0, xend = 1000, y = 0, yend = 1000), size = 1.2)+
#   geom_segment(aes(x = 0, xend = 1000, y = c(310,510), yend = c(710,910)), size = 1.2)+
#   geom_segment(aes(x = c(517, 850), xend = c(517, 850), y = 0, yend = c(517, 850)), linetype = 2)+
#   geom_segment(aes(x = 0, xend = c(517, 850), y = c(517, 850), yend = c(517, 850)), linetype = 2)+
#   geom_point(aes(x = c(517, 850), y = c(517, 850)), size = 5)+
#   geom_text(aes(c(c(517, 850)-20), c(c(517, 850)+45), label = paste("EQ")), parse = TRUE, size = 5)+
#   geom_text(aes(c(30), c(c(517, 850)/2.3), label = paste("A")), parse = TRUE, size = 5)+
#   geom_text(aes(c(300), c(470, 670), label = paste("Demanda")), parse = TRUE, angle = 15, size = 5)+
#   geom_text(aes(c(385), c(356), label = paste("Oferta")), parse = TRUE, angle = 36, size = 5)+
#   theme_minimal()+
#   labs(title = "Equilíbrio no mercado de bens\ndado um aumento nos gastos autônomos",
#        y = "Demanda Agregada, Z; Produção Agregada, Y",
#        x = "Renda, Y")+
#   theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
#         axis.title = element_text(size = 14, face = "bold"),
#         plot.caption = element_text(size = 14, face = "bold"),
#         axis.text = element_blank(),
#         axis.ticks = element_blank())+
#   transition_reveal(FR)
# 
# 
# 
# 
# g1_gif <- animate(g1, width = 600, height = 360)
# 
# g1_mgif <- image_read(g1_gif)
# 
# 
# 
# g2 <- ggplot(dados)+
#   geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1000))+
#   geom_segment(aes(x = 0, xend = 1000, y = 0,yend = 0))+
#   geom_segment(aes(x = 100, xend = 900, y = 900, yend = 100), size = 1.2)+
#   geom_segment(aes(x = c(517, 850), xend = c(517, 850), y = 0, yend = 1000), linetype = 2)+
#   geom_segment(aes(x = 0, xend = c(517, 850), y = c(484, 151), yend = c(484, 151)), linetype = 2)+
#   geom_point(aes(x = c(517, 850), y = c(484, 151)), size = 5)+
#   geom_text(aes(900, 150, label = paste("IS")), parse = TRUE, size = 5)+
#   theme_minimal()+
#   labs(y = "Taxa de juros, i",
#        x = "Renda, Y",
#        caption = "Econometria \nIndependente")+
#   theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
#         axis.title = element_text(size = 14, face = "bold"),
#         plot.caption = element_text(size = 14, face = "bold"),
#         axis.text = element_blank(),
#         axis.ticks = element_blank())+
#   transition_reveal(FR)
# 
# 
# g2_gif <- animate(g2, width = 600, height = 360)
# 
# g2_mgif <- image_read(g2_gif)
# 
# 
# new_gif <- image_append(c(g1_mgif[1], g2_mgif[1]), stack = TRUE)
# for(i in 2:100){
#   combined <- image_append(c(g1_mgif[i], g2_mgif[i]), stack = TRUE)
#   new_gif <- c(new_gif, combined)
# }
# 
# new_gif
# 
# 
# save_animation(new_gif, "eq_is_fd_fo_gif.gif")
# 
# # Tentativa falha
# gridExtra::grid.arrange(g1,g2, nrow = 2)
# 
# # Referencia
# https://github.com/thomasp85/gganimate/issues/140

