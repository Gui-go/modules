library(tidyverse)

v_a = -1
v_l = 100
f_D <- function(q){v_a*q + v_l}
q <- 1:100
# qf = ??
D <- as.vector(do.call("rbind", parallel::mclapply(q, f_D)))

c_f = 10
# y = 29.2675 + 0.5727149*x - 0.006683462*x^2
c_v = -15 + 81*q - 0.4*q^2
# c_v = -15 +37*q -0.9*q^2
# c_v = log(q^4)
plot(c_v)
c_t = (c_f + c_v)
plot(c_t)
c_m = c_t/q
plot(c_m)

c_mg = NULL
for (i in q) {
  c_mg[i] <- (c_t[i+1]-c_t[i])/(q[i+1]-q[i])
}

plot(c_mg)


df1 <- data.frame(
  D = D,
  q = q
)

df2 <- data.frame(
  p = D,
  q = q,
  rt = D*q
)

df3 <- data.frame(
  q = q,
  c_m = c_m,
  c_mg = c_mg
)


# Funcao Demanda
ggplot(df1, aes(q, D))+
  geom_line()+
  theme_minimal()

# Funcao Receita
ggplot(df2, aes(q, rt))+
  geom_line()+
  theme_minimal()

# Funcao Custo
ggplot(df3, aes(q))+
  geom_line(aes(y = c_m))+
  geom_line(aes(y = c_mg))





# try again ---------------------------------------------------------------

q = 1:30
TR = (90*q - 2*q^2)
TC = 200 + 10*q +2*q^2
ATC = TC/q
PI = TR-TC
c_mg = NULL; for (i in q) {
  c_mg[i] <- (TC[i+1]-TC[i])/(q[i+1]-q[i])
}
c_mg
r_mg = NULL; for (i in q) {
  r_mg[i] <- (TR[i+1]-TR[i])/(q[i+1]-q[i])
}
r_mg




df <- data.frame(
  q,
  TR,
  TC,
  PI
)

ggplot(df, aes(x = q))+
#  geom_line(aes(y = TR))+
  geom_line(aes(y = ATC), color = "blue")+
#  geom_line(aes(y = TC))
  geom_line(aes(y = c_mg))+
  geom_line(aes(y = r_mg))
#  geom_line(aes(y = PI))

