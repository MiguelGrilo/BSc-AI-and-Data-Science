# ******************************************************************************
#                     Regressao e Classificacao 2024/25
#
# Exercicios: Análise discriminante
#
# ******************************************************************************


# ----------------------------- Ex. 5.4 ----------------------------------------

# optativo: definir a diretoria atual como diretoria de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(foreign)
dados <- read.spss("emprestimos.sav", to.data.frame =T)


## criar conjuntos de dados ----------------------------------------------------

summary(dados)  # Ha 150 NA incumprimento
dados.completos <- na.omit(dados)
(nvalido <- dim(dados.completos)[1])
dadosClass <- subset(dados, is.na(incumpridor))   # dados para classificacao

# dividir conjunto de dados: 75% para estimar funcao discriminante e 25% para testar
set.seed(123) # divisao aleatoria dos dados
linhas <- sample(1:2, nvalido,
                 replace = TRUE,
                 prob = c(0.75, 0.25))
dadosA <- dados.completos[linhas==1,]  # dados para ajuste
dadosT <- dados.completos[linhas==2,]  # dados para teste



## selecionar variaveis discriminantes -----------------------------------------

cor(dadosA[,c(1, 3:8)])  # todas as correlacoes <0.8
# metodo stepwise com criterio lambda de Wilks
library(klaR)
mod_step <- greedy.wilks(incumpridor ~ ., data = dadosA, niveau = 0.05)
mod_step  # selecionou apenas txEnd + anosE + valorCC + anosR



## estimar funcao discriminante ------------------------------------------------
# nota: so ha 1 funcao porque m=min(g-1, p)=1
library(MASS)
fit <- lda(incumpridor ~ txEnd + anosE + valorCC + anosR, data=dadosA, na.action="na.omit")
fit
# a variavel que mais discrimina e o valorCC



## validar pressupostos --------------------------------------------------------

# 1. inexistencia de multicolinearidade
cor(dadosA[,c("txEnd", "anosE", "valorCC", "anosR")])  
# todas as correlacoes <0.8


# 2. normalidade multivariada
variaveis <- c("txEnd", "anosE", "valorCC", "anosR")
# via teste de Mardia
library(MVN)
mvn(dadosA[, variaveis], mvnTest="mardia")
# graves problemas de assimetria e achatamento
# txEnd com assimetria elevada e com alguma leptocurtose
# valorCC muito assimetrica e leptocurtica

# via teste de Henze-Zirkler
mvn(dadosA[, variaveis], mvnTest="hz")
# p<0.001 -> rej normalidade multivariada 

# via QQplot
mvn(dadosA[, variaveis], multivariatePlot = "qq")
# grande desvio

# via QQplot para avaliar normalidade e presenca de outliers
# usa o quadrado das distancias de Mahalanobis das observacoes aos centroides
fitmlm <- lm(cbind(txEnd, anosE, valorCC, anosR) ~ incumpridor, 
             data=dadosA)          # aplicar MANOVA
cqplot(fitmlm)
# grande desvio

# via analise dos residuos da MANOVA
residuos <- residuals(fitmlm)     # obter residuos
residuos <- data.frame(residuos)  # converter residuos em data.frame
residuos$grupo <- dadosA$incumpridor     # adicionar o grupo
# testes a normalidade
with(residuos, by(txEnd, grupo, shapiro.test))
with(residuos, by(anosE, grupo, shapiro.test))
with(residuos, by(valorCC, grupo, shapiro.test))
with(residuos, by(anosR, grupo, shapiro.test))
# todos com p<0.05 -> rej. hipotese de normalidade

# conclusao: rej normalidade multivariada
# mas n(grupo menor) >= 20 e p<=5 e nestas condicoes a AD é robusta à violacao do pressuposto 


# 3. igualdade das matrizes de var-cov
# ver matrizes de var-cov
with(dadosA, 
     by(cbind(txEnd, anosE, valorCC, anosR), incumpridor, cov))

# graficamente (recorrendo a elipses):
#library(heplots)
covEllipses(dados$A[,variaveis],  # variaveis independentes
            dadosA$incumpridor,   # variavel grupo
            fill=TRUE, pooled=FALSE, 
            col=c("blue", "red"), # cores
            variables=1:4)        # variaveis independentes a considerar 
# problemas pois nem sempre as elipses tem a mesma dimensao :(

# via teste M de Box
library(heplots)
testeBox <- boxM(cbind(txEnd, anosE, valorCC, anosR) ~ incumpridor, data=dadosA)
testeBox
# p<0.001 -> rej H0 -> nao podemos assumir a igualdade das matrizes de var-covar

# pela analise dos log determinantes: temos que ter n>p em cada grupo
testeBox$logDet  
plot(testeBox)   # os ICs nao se sobrepoem -> diferem

# mas a AD e robusta à violacao dos pressupostos desde que:
# * dimensao da amostra menor > numero de variaveis discriminantes
table(dadosA$incumpridor)
# n menor = 183 > 4 variaveis discriminantes
# * medias dos grupos nao sao proporcionais às suas variancias
medias <- aggregate(dadosA[,variaveis], list(dadosA$incumpridor), mean)  # medias por grupo
desvpad <- aggregate(dadosA[,variaveis], list(dadosA$incumpridor), sd)  # medias por grupo
as.matrix(medias[,-1])/as.matrix(desvpad[,-1]^2)*100
# nao se mantem a constante de proporcionalidade
# Portanto, podemos avancar com a AD



## Avaliar funcao discriminante linear -----------------------------------------

# do ponto do vista do ajuste
previstos <- predict(fit)
library(caret)
confusionMatrix(previstos$class, dadosA$incumpridor)
# 82.8% classificacoes corretas > 73.9% ao acaso
# sensibilidade elevada: 94.6%
# especificidade baixa: 49.3%
# problema na classificacao do grupo sim (errou mais do que acertou), na amostra usada para estimar AD

library(klaR) 
partimat(incumpridor ~ txEnd + anosE + valorCC + anosR, 
         data = dadosA, 
         method = "lda")
# nota: sao 6 graficos

# do ponto de vista da reacao a novas observacoes
previstosT <- predict(fit, newdata = subset(dadosT, select=-incumpridor))
confusionMatrix(previstosT$class, dadosT$incumpridor)
# 78.1% classificacoes corretas > 73.6% ao acaso, mas a diferenca nao é significativa (p=0.1)
# sensibilidade elevada: 95.4%
# especificidade baixa: 29.8%
# problema grave na classificacao do grupo sim (errou mais do que acertou), na amostra usada para testar a AD
# Conclusao: a funcao nao discrimina corretamente novos elementos 



## Construir funcao discriminante quadratica -----------------------------------
fitQ <- qda(incumpridor ~ txEnd + anosE + valorCC + anosR, data=dadosA, na.action="na.omit")
fitQ


## Avaliar funcao discriminante quadratica -------------------------------------

# do ponto do vista do ajuste
previstos <- predict(fitQ)
confusionMatrix(previstos$class, dadosA$incumpridor)
# 80.0% classificacoes corretas > 73.9% ao acaso
# sensibilidade elevada: 92.8%
# especificidade baixa: 41.9%
# problema na classificacao do grupo sim (errou mais do que acertou), na amostra usada para estimar AD

#library(klaR) 
partimat(incumpridor ~ txEnd + anosE + valorCC + anosR, 
         data = dadosA, 
         method = "qda")
# nota: sao 6 graficos


# do ponto de vista da reacao a novas observacoes
previstosT <- predict(fitQ, newdata = subset(dadosT, select=-incumpridor))
confusionMatrix(previstosT$class, dadosT$incumpridor)
# 78.1% classificacoes corretas > 73.6% ao acaso, mas a diferenca nao é significativa (p=0.1)
# sensibilidade elevada: 96.2%
# especificidade baixa: 27.7%
# problema grave na classificacao do grupo sim (errou mais do que acertou), na amostra usada para testar a AD
# Conclusao: a funcao nao discrimina corretamente novos elementos e os resultados foram piores aos da versao linear 



## Construir funcao discriminante linear com variaveis transformadas -----------

# que transformacao considerar?

# por ex. com a análise dos histogramas
summary(dadosA$txEnd)
par(mfrow=c(1,2))
hist(dadosA$txEnd)
library(car)
qqPlot(dadosA$txEnd)
transf <- sqrt(dadosA$txEnd) 
hist(transf); qqPlot(transf)

summary(dadosA$anosE)
hist(dadosA$anosE)
qqPlot(dadosA$anosE)
transf <- log(dadosA$anosE+1)
hist(transf); qqPlot(transf)
transf <- (dadosA$anosE+1)^.5
hist(transf); qqPlot(transf)

summary(dadosA$valorCC)
hist(dadosA$valorCC)
transf <- dadosA$valorCC^.25
hist(transf); qqPlot(transf)
transf <- log(dadosA$valorCC)
hist(transf); qqPlot(transf)

summary(dadosA$anosR)
hist(dadosA$anosR)
transf <- log(dadosA$anosR+1)
hist(transf); qqPlot(transf)
transf <- (dadosA$anosR+1)^.25
hist(transf); qqPlot(transf)

# Atraves do Box-Cox
library(car)
transfBC <- boxCox(dadosA$txEnd ~ 1)
(lambda_otimo <- transfBC$x[which.max(transfBC$y)])  # aprox. 0.25

transfBC <- boxCox(I(dadosA$anosE+1) ~ 1)
(lambda_otimo <- transfBC$x[which.max(transfBC$y)])  # aprox. 0.5

transfBC <- boxCox(dadosA$valorCC ~ 1)
(lambda_otimo <- transfBC$x[which.max(transfBC$y)])  # aprox. 0

transfBC <- boxCox(I(dadosA$anosR+1) ~ 1)
(lambda_otimo <- transfBC$x[which.max(transfBC$y)])  # aprox. 0.25


## Transformar variaveis
dadosA$txEndT <- (dadosA$txEnd)^.25
dadosA$anosET <- log(dadosA$anosE+1)
dadosA$valorCCT <- log(dadosA$valorCC) 
dadosA$anosRT <- (dadosA$anosR+1)^.25


## avaliar pressupostos
# 1. avaliar normalidade
variaveisT <- c("txEndT", "anosET", "valorCCT", "anosRT")
mvn(dadosA[, variaveisT], mvnTest="mardia")
# sem problemas de assimetria

# via teste de Henze-Zirkler
mvn(dadosA[, variaveisT], mvnTest="hz")
# p<0.001 -> rej normalidade multivariada 

# via QQplot
mvn(dadosA[, variaveisT], multivariatePlot = "qq")
# ligeiros problemas, mas da para passar

# via QQplot para avaliar normalidade e presenca de outliers
# usa o quadrado das distancias de Mahalanobis das observacoes aos centroides
fitmlm <- lm(cbind(txEndT, anosET, valorCCT, anosRT) ~ incumpridor, 
             data=dadosA)          # aplicar MANOVA
cqplot(fitmlm)
# passa

# 2. avaliar igualdade das matrizes de var-cov
# ver matrizes de var-cov
with(dadosA, 
     by(cbind(txEndT, anosET, valorCCT, anosRT), incumpridor, cov))

# graficamente (recorrendo a elipses):
#library(heplots)
covEllipses(dadosA[,variaveisT],  # variaveis independentes
            dadosA$incumpridor,   # variavel grupo
            fill=TRUE, pooled=FALSE, 
            col=c("blue", "red"), # cores
            variables=1:4)        # variaveis independentes a considerar 
# pretendemos elipses com a mesma dimensao: parecem OK

# via teste M de Box
library(heplots)
testeBox <- boxM(cbind(txEndT, anosET, valorCCT, anosRT) ~ incumpridor, data=dados)
testeBox
# p=0.055 -> nao rej H0 -> podemos assumir a igualdade das matrizes de var-covar

# pela analise dos log determinantes: temos que ter n>p em cada grupo
testeBox$logDet  # parecidos
plot(testeBox)   # os ICs sobrepoem-se -> passa


# estimar funcao discriminante linear
fitT <- lda(incumpridor ~ txEndT + anosET + valorCCT + anosRT, data=dadosA, na.action="na.omit")
fitT

previstos <- predict(fitT)
confusionMatrix(previstos$class, dadosA$incumpridor)
# 80.1% classificacoes corretas > 74.0% ao acaso
# sensibilidade elevada: 90.93%
# especificidade baixa: 49.26%
# problema na classificacao do grupo sim (errou mais do que acertou), na amostra usada para estimar AD

#library(klaR) 
partimat(incumpridor ~ txEndT + anosET + valorCCT + anosRT, 
         data = dadosA, 
         method = "lda")
# nota: sao 6 graficos

novos <- within(dadosT,{ 
  txEndT=(txEnd)^.25
  anosET=log(anosE+1)
  valorCCT=log(valorCC)
  anosRT=(anosR+1)^.25
})


previstosT <- predict(fitT, newdata = novos)
confusionMatrix(previstosT$class, dadosT$incumpridor)
# 76.4% classificacoes corretas > 73.6% ao acaso, mas a diferenca nao é significativa (p=0.1)
# sensibilidade elevada: 93.1%
# especificidade baixa: 29.8%
# problema grave na classificacao do grupo sim (errou mais do que acertou), na amostra usada para testar a AD
# Conclusao: a funcao nao discrimina corretamente novos elementos e os resultados foram piores aos da versao linear 


## classificacao ---------------------------------------------------------------
# o modelo construido que teve melhor desempenho foi 1 funcao discriminante linear sem transformacoes
previstos <- predict(fit, newdata = dadosClass)
table(previstos$class)
