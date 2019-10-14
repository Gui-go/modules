# foa_fda_is_UI <- function(id) {
#   ns = NS(id)
#   
#   list(
#     fluidPage(
#       column(12,
#              imageOutput(ns("foa_fda_is_output")), offset = 4)
#     )
#   )
# }
# 
# foa_fda_is_serv <- function(input, output, session) {
#   output$foa_fda_is_output <- renderImage({
#     
#     list(src = "animations/eq_is_fd_fo_gif.gif",
#          contentType = 'image/gif'
#          # width = 400,
#          # height = 300,
#          # alt = "This is alternate text"
#     )}, deleteFile = F)}









# rm(list = ls())
# 
# # Libraries ---------------------------------------------------------------
# library("ggplot2")
# library("dplyr")
# library("gganimate")
# library("gifski")
# library("ggthemes")
# library("gridExtra")
# library("magick")
# library("ggfortify")
# 
# 
# # IS-LM -------------------------------------------------------------------
# eixo_x <- c(0:1000)
# is1 <- c(-0.8*eixo_x+900)
# is2 <- c(-0.8*eixo_x+900)
# is3 <- c(-0.8*eixo_x+900)
# lm1 <- c(0.9*eixo_x+100)
# lm2 <- c(0.9*eixo_x-100)
# lm3 <- c(0.9*eixo_x+100)
# 
# MA1coef <- matrix(c(1, coef(lm(is1 ~ eixo_x))[[2]], 1, coef(lm(lm1 ~ eixo_x))[[2]]), nrow = 2, ncol = 2, byrow = T)
# MA1cons <- matrix(c(coef(lm(is1 ~ eixo_x))[[1]], coef(lm(lm1 ~ eixo_x))[[1]]), nrow = 2, ncol = 1, byrow = T)
# EPA1 <- solve(MA1coef, MA1cons)
# 
# M2coef <- matrix(c(1, coef(lm(is2 ~ eixo_x))[[2]], 1, coef(lm(lm2 ~ eixo_x))[[2]]), nrow = 2, ncol = 2, byrow = T)
# M2cons <- matrix(c(coef(lm(is2 ~ eixo_x))[[1]], coef(lm(lm2 ~ eixo_x))[[1]]), nrow = 2, ncol = 1, byrow = T)
# EPA2 <- solve(M2coef, M2cons)
# 
# M3coef <- matrix(c(1, coef(lm(is3 ~ eixo_x))[[2]], 1, coef(lm(lm3 ~ eixo_x))[[2]]), nrow = 2, ncol = 2, byrow = T)
# M3cons <- matrix(c(coef(lm(is3 ~ eixo_x))[[1]], coef(lm(lm3 ~ eixo_x))[[1]]), nrow = 2, ncol = 1, byrow = T)
# EPA3 <- solve(M3coef, M3cons)
# 
# dadosA <- data.frame(
#   is_x = c(eixo_x[1], eixo_x[1], eixo_x[1]),
#   is_xend = c(eixo_x[1000], eixo_x[1000], eixo_x[1000]),
#   is_y = c(is1[1], is2[1], is3[1]),
#   is_yend = c(is1[1000], is2[1000], is3[1000]),
#   lm_x = c(eixo_x[1], eixo_x[1], eixo_x[1]),
#   lm_xend = c(eixo_x[1000], eixo_x[1000], eixo_x[1000]),
#   lm_y = c(lm1[1], lm2[1], lm3[1]),
#   lm_yend = c(lm1[1000], lm2[1000], lm3[1000]),
#   lm_s_x = c(eixo_x[1], eixo_x[1], eixo_x[1]),
#   lm_s_xend = c(eixo_x[1000], eixo_x[1000], eixo_x[1000]),
#   lm_s_y1 = c(lm1[1], lm1[1], lm1[1]),
#   lm_s_yend1 = c(lm1[1000], lm1[1000], lm1[1000]),
#   lm_s_y2 = c(lm1[1], lm2[1], lm2[1]),
#   lm_s_yend2 = c(lm1[1000], lm2[1000], lm2[1000]),
#   ep_y = c(EPA1[1], EPA2[1], EPA3[1]),
#   ep_x = c(-EPA1[2], -EPA2[2], -EPA3[2]),
#   frames = c(1,2, 3),
#   caption_time = c("Curto prazo", "Curto prazo", "Médio prazo")
# )
# 
# attach(dadosA)
# 
# g1 <- ggplot(dadosA)+
#   geom_segment(aes(x = lm_s_x, xend = lm_s_xend, y = lm_s_y1, yend =lm_s_yend1), size = 1.6, color = "gray")+
#   geom_segment(aes(x = lm_s_x, xend = lm_s_xend, y = lm_s_y2, yend =lm_s_yend2), size = 1.6, color = "gray")+
#   geom_segment(aes(x = is_x, xend = is_xend, y = is_y, yend =is_yend), size = 1.6)+
#   geom_segment(aes(x = lm_x, xend = lm_xend, y = lm_y, yend =lm_yend), size = 1.6)+
#   geom_segment(aes(x = 0, xend = 1000, y = 0,yend = 0), size = 1.1)+
#   geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1000), size = 1.1)+
#   geom_segment(aes(x = ep_x, xend = ep_x, y = 0, yend = ep_y), linetype = 2)+
#   geom_segment(aes(x = 0, xend = ep_x, y = ep_y, yend = ep_y), linetype = 2)+
#   # geom_text(aes(x = 500, y = 900), label = paste(caption_time), size = 6)+
#   coord_cartesian(ylim = c(0, 1000), expand = FALSE, xlim = c(0, 1000))+
#   theme_minimal()+
#   labs(title = "Inflation is always and everywhere\na monetary phenomena",
#        y = "Juros, i",
#        x = "Renda, Y")+
#   theme(plot.title = element_text(size = 21, face = "bold", hjust = 0.5),
#         axis.title = element_text(size = 19, face = "bold"),
#         plot.caption = element_text(size = 13, face = "bold"),
#         axis.text = element_blank(),
#         axis.ticks = element_blank())+
#   transition_reveal(frames)
# 
# g1_gif <- animate(g1, width = 600, height = 360)
# 
# g1_mgif <- image_read(g1_gif)
# 
# 
# # OA-DA -------------------------------------------------------------------
# eixo_x <- c(0:1000)
# da1 <- c(-0.8*eixo_x+900)
# da2 <- c(-0.8*eixo_x+1100)
# da3 <- c(-0.8*eixo_x+1100)
# oa1 <- c(0.9*eixo_x+100)
# oa2 <- c(0.9*eixo_x+100)
# oa3 <- c(0.9*eixo_x+300)
# 
# MB1coef <- matrix(c(1, coef(lm(da1 ~ eixo_x))[[2]], 1, coef(lm(oa1 ~ eixo_x))[[2]]), nrow = 2, ncol = 2, byrow = T)
# MB1cons <- matrix(c(coef(lm(da1 ~ eixo_x))[[1]], coef(lm(oa1 ~ eixo_x))[[1]]), nrow = 2, ncol = 1, byrow = T)
# EPB1 <- solve(MB1coef, MB1cons)
# 
# MB2coef <- matrix(c(1, coef(lm(da2 ~ eixo_x))[[2]], 1, coef(lm(oa2 ~ eixo_x))[[2]]), nrow = 2, ncol = 2, byrow = T)
# MB2cons <- matrix(c(coef(lm(da2 ~ eixo_x))[[1]], coef(lm(oa2 ~ eixo_x))[[1]]), nrow = 2, ncol = 1, byrow = T)
# EPB2 <- solve(MB2coef, MB2cons)
# 
# MB3coef <- matrix(c(1, coef(lm(da3 ~ eixo_x))[[2]], 1, coef(lm(oa3 ~ eixo_x))[[2]]), nrow = 2, ncol = 2, byrow = T)
# MB3cons <- matrix(c(coef(lm(da3 ~ eixo_x))[[1]], coef(lm(oa3 ~ eixo_x))[[1]]), nrow = 2, ncol = 1, byrow = T)
# EPB3 <- solve(MB3coef, MB3cons)
# 
# dadosB <- data.frame(
#   da_x = c(eixo_x[1], eixo_x[1], eixo_x[1]),
#   da_xend = c(eixo_x[1000], eixo_x[1000], eixo_x[1000]),
#   da_y = c(da1[1], da2[1], da3[1]),
#   da_yend = c(da1[1000], da2[1000], da3[1000]),
#   da_s_y = c(da1[1], da1[1], da1[1]),
#   da_s_yend = c(da1[1000], da1[1000], da1[1000]),
#   oa_x = c(eixo_x[1], eixo_x[1], eixo_x[1]),
#   oa_xend = c(eixo_x[1000], eixo_x[1000], eixo_x[1000]),
#   oa_y = c(oa1[1], oa2[1], oa3[1]),
#   oa_yend = c(oa1[1000], oa2[1000], oa3[1000]),
#   oa_s_x = c(eixo_x[1], eixo_x[1], eixo_x[1]),
#   oa_s_xend = c(eixo_x[1000], eixo_x[1000], eixo_x[1000]),
#   oa_s_y = c(oa1[1], oa2[1], oa2[1]),
#   oa_s_yend = c(oa1[1000], oa2[1000], oa2[1000]),
#   ep_y = c(EPB1[1], EPB2[1], EPB3[1]),
#   ep_x = c(-EPB1[2], -EPB2[2], -EPB3[2]),
#   frames = c(1,2,3)
# )
# 
# attach(dadosB)
# 
# g2 <- ggplot(dadosB)+
#   geom_segment(aes(x = oa_s_x, xend = oa_s_xend, y = oa_s_y, yend = oa_s_yend), size = 1.6, color = "gray")+
#   geom_segment(aes(x = da_x, xend = da_xend, y = da_s_y, yend = da_s_yend), size = 1.6, color = "gray")+
#   geom_segment(aes(x = da_x, xend = da_xend, y = da_y, yend = da_yend), size = 1.6)+
#   geom_segment(aes(x = oa_x, xend = oa_xend, y = oa_y, yend = oa_yend), size = 1.6)+
#   geom_segment(aes(x = 0, xend = 1000, y = 0,yend = 0), size = 1.1)+
#   geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1000), size = 1.1)+
#   geom_segment(aes(x = ep_x, xend = ep_x, y = 0, yend = 1000), linetype = 2)+
#   geom_segment(aes(x = 0, xend = ep_x, y = ep_y, yend = ep_y), linetype = 2)+
#   coord_cartesian(ylim = c(0, 1000), expand = FALSE, xlim = c(0, 1000))+
#   theme_minimal()+
#   labs(y = "Nível de preços, P",
#        x = "Renda, Y",
#        caption = "Econometria \nIndependente")+
#   theme(plot.title = element_text(size = 21, face = "bold", hjust = 0.5),
#         axis.title = element_text(size = 19, face = "bold"),
#         plot.caption = element_text(size = 13, face = "bold"),
#         axis.text = element_blank(),
#         axis.ticks = element_blank())+
#   transition_reveal(frames)
# 
# g2_gif <- animate(g2, width = 600, height = 360)
# 
# g2_mgif <- image_read(g2_gif)
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
# save_animation(new_gif, "inflation_mp.gif")
