# ******************************************************************************
#                     Regressao e Classificacao 2024/25
#
# Exercicios: Regressão linear
#
# ******************************************************************************


# ----------------------------- Ex. 2.1 ----------------------------------------
remates <- c(19, 15, 12, 16, 11, 28, 22, 8, 16)     # y
cruzamentos <-c(45, 25, 22, 32, 28, 56, 38, 10, 32) # x

# a) ----
# versao simples
plot(cruzamentos, remates)

# se quiserem gerar o grafico na versao ggplot, os dados tem que estar num data.frame
dados <- data.frame(remates, cruzamentos) 
library(ggplot2)
ggplot(dados, aes(x=cruzamentos, y=remates))+
  geom_point()+
  labs(x="n.º de cruzamentos", y="n.º de remates")+
  theme_bw()

# b) ----
with(dados, cor(cruzamentos,remates))

# c) ----
(reta <- lm(remates~cruzamentos, dados))
# se quiserem output muito mais completo (util para inferencia)
summary(reta)

# e) ----
# versao simples
with(dados, plot(cruzamentos, remates))
abline(reta, 
       col="red",  # cor da linha
       lwd=2,      # espessura da linha
       lty=2)      # tipo de linha (tracejada ou outro tipo)

#versao ggplot
ggplot(dados, aes(x=cruzamentos, y=remates))+
  geom_point()+
  geom_smooth(method="lm", color="red", se=F)+
  labs(x="n.º de cruzamentos", y="n.º de remates")+
  theme_bw()

# f) ----
(n <- dim(dados)[1])
SQT <- var(dados$remates)*(n-1)
SQT

# g) ----
estimados <- fitted(reta)
SQR <- var(estimados)*(n-1)
SQR

# h) ----
residuos <- residuals(reta)
SQRE <- var(residuos)*(n-1)
SQRE

# i) ----
SQT
SQR+SQRE

# j) ----
(R2 <- SQR/SQT)
#ou
summary(reta)$r.squared 
# ou
summary(reta)$r.sq

# extra: 
# obter valor ajustado (ou estimado) para y quando x=45
x <- 45
(yP <-2.7725+0.4238*x)  # 2.7725 e 0.4238 dados pela reta de regressao
# obter o residuo, sabendo que quando x=45 se observou y=19
y=19
(residuo <- y-yP)
# colocar lado a lado a informacao da amostra, os valores estimados para y e os residuos
cbind(dados$cruzamentos, dados$remates, estimados, residuos)
