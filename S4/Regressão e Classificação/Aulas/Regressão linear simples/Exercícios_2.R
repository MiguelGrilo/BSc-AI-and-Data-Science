#### REGRESSÃO LINEAR SIMPLES

# 2.1
#Criar vetores dos dados
remates <- c(19, 15, 12, 16, 11, 28, 22, 8, 16)
cruzamentos <- c(45, 25, 22, 32, 28, 56, 38, 10, 32)
#Criar DataFrame
dados <- data.frame(remates, cruzamentos)

# a)
# Variável que depende da outra será o 'y'
library(ggplot2)
ggplot(dados, aes(x=cruzamentos, y=remates))+
  geom_point()+
  labs(x="Número de Cruzamentos", y="Número de Remates")+
  theme_bw()

# b)
cor(dados$remates, dados$cruzamentos)

# c) 
(reta <- lm(remates ~ cruzamentos, dados)) 
# Escrito tudo entre parênteses guarda no objeto reta 
# e mostra o resultado do comando também.
# Equação da reta ajustada:
# ŷ = b0 + b1*x
# ŷ = 2.7725 + 0.4238*cruzamentos
# ŷ = remates(com acento circunflexo pois indica que são os
# valores da reta e não os y reais)


# d)
# A cada cruzamentos o número de remates aumenta 
# aproximadamente 0.4238 e quando não existem quaisquer
# cruzamentos a média de remates é 2.7725 .

# e)
gyokeres <- "green"   # RANDOM

ggplot(dados, aes(x=cruzamentos, y=remates))+
  geom_point()+
  geom_smooth(method="lm", color=gyokeres, se=FALSE)+
  labs(x="Número de Cruzamentos", y="Número de Remates")+
  theme_bw()

# f)
# s² = SOMATÓRIO( (yi - ỹ)²) / (n-1)
dim(dados)
(n <- dim(dados)[1])
(SQT <- var(dados$remates)*(n-1))

# g)
estimados <- fitted(reta)
(SQR <- var(estimados)*(n-1))

# h)
# Resíduos = e
# ei = yi - ŷi
residuos <- residuals(reta)
(SQRE <-var(residuos)*(n-1)) 

# i)
SQT
SQR+SQRE

# j)
SQR/SQT
# Outro método
str(summary(reta))
summary(reta)$r.squared

# Extra: Quantos remates haverão se forem realizados 45 cruzamentos?
(yPrevisto <- 2.7725 + 0.4238 * 45)
(residuo <- 19-yPrevisto)

cbind(dados$cruzamentos, dados$remates, estimados, residuos)

# Apontamentos
# X e Y v.a. , a e b constantes
# E(a) = a
# E(X + a) = E(X) +- a
# E(a*X) = a*E(X)


# 2.5
library(readxl)
dados <- read_excel("vendas.xlsx")

names(dados) <- c("y", "x")

# a)
(reta <- lm(y ~x, dados))

(b1 <- cov(dados$x, dados$y)/var(dados$x))
mean(dados$x)
mean(dados$y) - b1 * mean(dados$x)

# b)
with (dados, plot( y~x,
                   xlab="preço",
                   ylab="vendas"))
abline(reta, 
       col="green",
       lwd = 2,  # Espessura
       lty = 2)

with(dados, cor(x, y))

# c)
residuals(reta)
rstandard(reta)
rstudent(reta)

# d)
residuos <- residuals(reta)
estimados <- fitted(reta)
plot(residuos~ estimados)

mean(residuos)
library(car)
qqPlot(rstandard(reta))
shapiro.test(rstandard(reta))

par(mfrow=c(2,2))
plot(reta)

# e)
with(dados, cor(x, y))

# f)
(n <- dim(dados)[1])
(SQRE <- var(residuos)*(n-1))
(RQME <- SQRE/(n-2))

# g)
anova(reta)

# h) + i)
confint(reta)
confint(reta, level=.99)

summary(reta)

# k) 
summary(reta)
qt(0.995, 10)  #Valor da t-student para t(n-2);(1-a/2) -> t(10);(0.995)

# l)
summary(reta)$r.sq

# m)
novosDados <- data.frame(x=1.23)
predict(reta, new=novosDados)
predict(reta, new=data.frame(x=1.23), int="conf", level=0.95)
# n)
predict(reta, new=data.frame(x=1.23), int="pred")


# 2.6


# 2.8
# a)
library(MASS)
dados <- Animals
str(dados)
names(dados)

library(ggplot2)
ggplot(Animals, aes(x=body, y=brain))+
  geom_point()
with(dados, plot(brain~body))

# b)
with(Animals, cor(brain,body))

# c)
#  i)
library(ggplot2)
logBrain <- log(Animals$brain)
(g1 <- ggplot(Animals, aes(x=body, y=logBrain))+
  geom_point())
#   ii)
logBody <- log(Animals$body)
(g2 <- ggplot(Animals, aes(x=logBody, y=brain))+
  geom_point())
#   iii)
(g3 <- ggplot(Animals, aes(x=logBody, y=logBrain))+
  geom_point())

library(gridExtra)
grid.arrange(g1, g2, g3, ncol=3)

with(Animals, cor(logBrain,body))
with(Animals, cor(brain,logBody))
with(Animals, cor(logBrain,logBody))

# e)
cor(x, y)
summary(reta)

# f)

# g)
with(dados, plot(log(brain)~log(body)))
identify(dados$brain, dados$body, tolerance=1)

identify(log(dados$brain), log(dados$body))

cbind(dados$brain, dados$body)

# h)
(modelo1 <- lm(log(brain)~log(body), data=dados))
(modelo2 <- lm(log(brain)~log(body), data=dados[-c(6,16,26),]))

(modelo3 <- update(modelo1, data=dados[-c(6,16,26),]))

# i)
with(dados, plot(log(brain)~log(body)))
abline(modelo1, col="pink")
abline(modelo2, col="green")

par(mfrow=c(2,2))
plot(modelo1)
plot(modelo2)













