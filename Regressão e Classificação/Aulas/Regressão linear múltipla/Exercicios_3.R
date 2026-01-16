###   3.1   ###
dados <- read.csv2("vending.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                     sep=";", dec=",", header=T, encoding = "UTF-8")

# a)
plot(dados[,-1])

# b)
cor(dados[,-1])

# c)
(modelo <- lm(tempo~caixas+distancia, data=dados))
# Têmpo = 3.1442 + 1.4217 caixas + 0.0197 distância

# d) 
summary(modelo)
# H0: B1=B2=0   vs    H1: Pelo menos um Bj \= 0, j=1,2
# E.T.  F= MQR/MQRE ~ F p; n-(p+1)
# valor p < 0.001
# Rejeitamos H0, alfa = 5%

# e)
 
# extra
pt(-4.073, 22)

# f)
confint(modelo, level= 0.90)

# g)
novos <- data.frame(caixas=c(3,10,20),
                    distancia=c(100,500,1000))
predict(modelo, novos)

# h)
  # i.
  predict(modelo, novos, int="conf", level=0.95)
  # ii.
  predict(modelo, novos, int="pred", level=0.95)

# j)
lm(tempo~caixas, data=dados)
lm(tempo~distancia, data=dados)



###   3.2   ###
library(datarium)
dados <- marketing
summary(dados)
# a)  Ajuste o modelo de regressão linear considerando apenas os efeitos 
#     principais.
(modelo <- lm(sales~youtube+facebook+newspaper, data=dados))

# b) Teste a significância do modelo geral.
summary(modelo) # Verificar F-statistic e p-value

# c) Teste a significância de cada um dos coeficientes.
summary(modelo) # Verificar a tabela dos coeficientes, colunas t-value e Pr(>|t|)

# d) Com base nos resultados anteriores, ajuste o modelo que lhe parecer 
#    mais adequado.
(modelo1 <- lm(sales~youtube+facebook, data=dados))
# OU
(modelo1 <- update(modelo, ~.-newspaper))
summary(modelo1)

# e) Verifique se obteria um modelo diferente, caso tivesse usado os métodos 
#    sequenciais: forward, backward e stepwise.
drop1(modelo, test="F")
drop1(modelo1, test="F")

(modelo0 <- lm(sales~1, data=dados))
add1(modelo0, scope= ~youtube+facebook, test="F")
(modelo1 <- update(modelo0, ~.+facebook))
add1(modelo1, scope= ~.+facebook+newspaper, test="F")

modelo2a <- update(modelo1, ~.+facebook)
modelo2b <- update(modelo1, ~.+newspaper)

anova(modelo2a, modelo1, test="F")
anova(modelo2b, modelo1, test="F")

step(modelo)
step(modelo, direction="backward")
modeloF <- step(modelo0, scope=~youtube+facebook+newspaper, direction="forward")

# f) Considere o modelo ajustado em d).
#   i) Obtenha intervalos de confiança a 95% para os coeficientes e interprete.
confint(modeloF)

#   ii) Qual a variável com maior contribuição relativa para o modelo?
(modelo.estandartizado <- lm(data.frame(scale(modeloF$model))))
# OU
library(parameters)
standardize_parameters(modeloF)

par(mfrow=c(2,2))
plot(modeloF)
plot(modeloF, which=4)
hist(rstandard(modeloF))

DFBETAS <- dfbetas(modeloF)
plot(abs(DFBETAS[,1]), type="h")
abline(h=2/sqrt(200), col="green")

plot(abs(DFBETAS[,2]), type="h")
abline(h=2/sqrt(200), col="green")

plot(abs(DFBETAS[,3]), type="h")
abline(h=2/sqrt(200), col="green")

DFFITS <- dffits(modeloF)
plot(abs(DFFITS), type="h")
abline(h=2*sqrt(3/200), col="green")

plot(rstandard(modeloF))

#   iii) Qual o valor previsto para as vendas se forem gastos 100 u.m. em 
#        anúncios no youtube, 50 u.m. no Facebook e 80 u.m. em jornais?

#   iv) Verifique se a interação entre os preditores Facebook e Youtube 
#       é significativa.

#   v) Valide os pressupostos do modelo.

# g) Caso o modelo ajustado em d) não satisfaça os pressupostos, tente encontrar um modelo
#    alternativo. Experimente:
#   i) Transformar as variáveis (resposta, preditoras ou ambas).
library(MASS)
lambda <- boxcox(modeloF)
lambda$x[which(lambda$y==max(lambda$y))]

residuos <- rstandard(modeloF)
plot(marketing$facebook, residuos)
plot(marketing$youtube, residuos)

modelo7 <- lm(sales ~ facebook+I(sqrt(youtube)), data=marketing)
par(mfrow=c(2,2))
plot(modelo7)

modelo7b <- lm(sales ~ facebook *youtube, data=marketing)
par(mfrow=c(2,2))
plot(modelo7b)

modelo7c <- lm(sales ~ facebook+poly(youtube,2), data=marketing)
par(mfrow=c(2,2))
plot(modelo7c)

AIC(modeloF, modelo7, modelo7b, modelo7c)

modelo8 <- lm(sales~facebook * I(sqrt(youtube)), data=marketing)
plot(modelo8)

modelo8b <- update(modelo8, data=marketing)
plot(modelo8b)

summary(modelo8b)

#   ii) Incluir de termos polinomiais e/ou interações.

#   iii) Remover pontos influentes, caso existam.

# h) Caso em g) tenha optado por um modelo com variáveis transformadas e/ou interações,
#    interprete o modelo obtido.







