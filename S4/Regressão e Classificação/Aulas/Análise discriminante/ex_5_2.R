# ******************************************************************************
#                     Regressao e Classificacao 2024/25
#
# Exercicios: Análise discriminante
#
# ******************************************************************************


# ----------------------------- Ex. 5.2 ----------------------------------------

# optativo: definir a diretoria atual como diretoria de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dados <- read.csv2("resultadoMat.csv", row.names=NULL, stringsAsFactors=TRUE)
# eliminar variavel nota da base
dados <- subset(dados, select=-nota)


## a) variaveis nhoras e autoconc -----

### i) -----

# nuvem de pontos com indicacao do grupo
gXY <- ggplot(data=dados, 
              aes(x=nhoras, y=autoconc, color=grupo))+ 
  geom_point(size=2, )+
  theme_classic()
gXY

# nuvem de pontos com letras em vez de pontos
gXY <- ggplot(data=dados, 
              aes(x=nhoras, y=autoconc, label=grupo, color=grupo))+ 
  geom_text()+
  theme_classic()
gXY
# a var n horas diferencia claramente R e A
# a variavel autoconceito diferencia claramente A e AR 
# conclusao: nenhuma das vars diferencia totalmente os 3 grupos


### ii) -----
library(MASS)
fit <- lda(grupo ~nhoras + autoconc, data=dados)
fit
# maior contribuicao para D1: variavel autoconc 
# maior contribuicao para D2: variaveis autoconc e nhoras
# nota: estes coeficientes dependem da escala das variaveis
# proportion of trace = % da variancia explicada em termos de diferenças entre grupos, por cada função discriminante
# ou seja, a importancia relativa de cada funcao discriminante
# 1a função 99.16%
# 2a função 0.84%


### iii) -----
# temos que ajustar como mlm para obtermos os coeficientes estandardizados
fit1 <- lm(cbind(nhoras, autoconc) ~ factor(grupo),  data = dados)
library(candisc)
fitcan <- candisc(fit1)
fitcan$coeffs.std
# maior contribuicao relativa para D1: variavel autoconc 
# maior contribuicao relativa para D2: variaveis nhoras

# curiosidade: coeficientes originais da lda
fitcan$coeffs.raw


### iv) -----
n <- nrow(dados)                  # numero de observacoes
p <- 2                            # numero de variaveis
g <- length(unique(dados$grupo))  # numero de grupos

# Teste lambda de Wilks
# H0: nenhuma funcao tem poder discriminante
# H1: pelo menos uma funcao tem poder discriminante
lambdaT <- prod(1/(1+fitcan$eigenvalues))   # lambda de Wilks
# estatística de teste
(QQ <- -(n-(p+g)/2-1)*log(lambdaT) )
# p-value
(pvalue <- 1-pchisq(QQ,p*(g-1)))
# rej H0 -> pelo menos 1 das funcoes tem poder disciminante -> repetir teste sem funcao 1

# H0: a funcao 2 nao tem poder discriminante
# H1: a funcao 2 tem poder discriminante
lambdaT <- 1/(1+fitcan$eigenvalues[2])   # lambda de Wilks
# estatística de teste
(QQ <- -(n-(p+g)/2-1)*log(lambdaT) )
# p-value
(pvalue <- 1-pchisq(QQ,(p-1)*(g-1-1)))
# nao rej H0 -> a funcao 2 nao tem poder discriminante


### v) -----
library(klaR) 
partimat(factor(grupo)~nhoras+autoconc, data = dados, method = "lda")
# a vermelho identifica os elementos mal classificados
# os pontos representam os centroides das zonas de classificacao


### vi) -----
previstos <- predict(fit)
library(caret)
confusionMatrix(previstos$class, dados$grupo)
# 2 elementos do grupo R foram classificados como AR
# 2 elementos do grupo AR foram classificados como R
# 81.8% dos elementos forem bem classificados (> 36.4% classificações corretas obtidas pelo acaso)
# todos os elementos de A forem bem classificados
# 75% dos elementos de AR forem bem classificados
# 71.4% dos elementos de R forem bem classificados
# 100% dos elementos que nao pertencem a A foram classificados como nao A
# 85.7% dos elementos que nao pertencem a AR foram classificados como nao AR
# 86.7% dos elementos que nao pertencem a R foram classificados como nao R

# leave-one-out cross-validation 
fitV <- lda(grupo ~nhoras + autoconc, data=dados, CV=T)
table(fitV$class, dados$grupo)
# tabela igual a obtida com a matriz de confusao, ou seja, 
# continuamos a não conseguir classificar corretamente 2 AR e 2 R



## b) -----

### i) -----

# 1. normalidade multivariada

# via teste de Mardia
library(MVN)
mvn(dados[,c("nhoras","autoconc","dimensao", "rendimento", 
             "apoiofam","apoioprof")],
    mvnTest="mardia")
# sem problemas de assimetria (p=0.872) e achatamento (p=0.338) multivariados 
# rej. normalidade univariada para a variavel dimensao


# via teste de Henze-Zirkler
mvn(dados[,c("nhoras","autoconc","dimensao", "rendimento", 
             "apoiofam","apoioprof")], mvnTest="hz")
# p=0.307 -> nao rej normalidade multivariada 


# via QQplot
mvn(dados[, c("nhoras","autoconc","dimensao", "rendimento", 
              "apoiofam","apoioprof")], 
    multivariatePlot = "qq")

# via QQplot para avaliar normalidade e presenca de outliers
# usa o quadrado das distancias de Mahalanobis das observacoes aos centroides
cqplot(fitmlm)


# via analise dos residuos da MANOVA
fitmlm <- lm(cbind(nhoras,autoconc,dimensao, rendimento, apoiofam, apoioprof) ~ grupo, 
             data=dados)          # aplicar MANOVA
residuos <- residuals(fitmlm)     # obter residuos
residuos <- data.frame(residuos)  # converter residuos em data.frame
residuos$grupo <- dados$grupo     # adicionar o grupo
# testes à normalidade
with(residuos, by(residuos$nhoras, grupo, shapiro.test))
with(residuos, by(residuos$autoconc, grupo, shapiro.test))
with(residuos, by(residuos$dimensao, grupo, shapiro.test))
with(residuos, by(residuos$rendimento, grupo, shapiro.test))
with(residuos, by(residuos$apoiofam, grupo, shapiro.test))
with(residuos, by(residuos$apoioprof, grupo, shapiro.test))
# todos com p>0.05 -> nao rej. hipotese de normalidade


# 2. igualdade das matrizes de var-cov
# ver matrizes de var-cov
with(dados, 
     by(cbind(nhoras,autoconc,dimensao, rendimento, apoiofam,apoioprof), grupo, cov))

# graficamente (recorrendo a elipses):
#library(heplots)
covEllipses(dados[,c("nhoras","autoconc","dimensao", "rendimento", "apoiofam","apoioprof")],  # variaveis independentes
            dados$grupo,   # variavel grupo
            fill=TRUE, pooled=FALSE, 
            col=c("blue", "red", "darkgreen"), # cores
            variables=1:3)    # grupos a considerar

# pretendemos elipses com a mesma dimensao: grupo A parece diferir um pouco dos restantes
table(dados$grupo)

# via teste M de Box
library(heplots)
testeBox <- boxM(cbind(nhoras,autoconc,dimensao, rendimento, apoiofam,apoioprof) ~ factor(grupo), data=dados)
# igual a 
# fitmlm <- lm(cbind(nhoras,autoconc,dimensao, rendimento, apoiofam,apoioprof) ~ factor(grupo), data=dados)
# testeBox <- boxM(fitmlm)
testeBox
# p=0.10 -> nao rej H0 -> podemos assumir a igualdade das matrizes de var-covar

# extra
testeBox$cov     # devolve as matrizes var-covar de cada grupo
testeBox$pooled  # devolve a matriz Spooled

# pela analise dos log determinantes: temos que ter n>p em cada grupo
testeBox$logDet
# com amostras grandes e n>>p podemos tambem comparar os IC para os logdeterminantes
plot(testeBox)


# 3. não colinearidade
cor(dados[, -1])    # sem a variavel grupo
# correlacoes > 0.8: (rendimento, autoconc) 



### ii) -----
# metodo stepwise com criterio lambda de Wilks
library(klaR)
mod_step <- greedy.wilks(grupo ~ ., data = dados, niveau = 0.05)
mod_step  # selecionou apenas autoconc + rendimento + apoioprof


### iii) -----
fit <- lda(grupo ~autoconc + rendimento + apoioprof, data=dados)
fit
# maior contribuicao para D1: variavel autoconc 
# maior contribuicao para D2: variaveis apoioprof
# 98.27% da variancia (em termos das diferenças entre grupos) é explicada pela função 1 (D1)
# 1.7% da variancia é explicada pela função 2 (D2) 


### iv) -----
# ajustar como mlm
fit1 <- lm(cbind(autoconc, rendimento, apoioprof) ~ factor(grupo),  data = dados)
library(candisc)
fitcan <- candisc(fit1)
fitcan$coeffs.std
# maior contribuicao relativa para D1: variaveis autoconc e rendimento 
# maior contribuicao relativa para D2: variaveis apoioprof


### v) -----
n <- nrow(dados)                  # numero de observacoes
p <- 3                            # numero de variaveis
g <- length(unique(dados$grupo))  # numero de grupos

# Teste lambda de Wilks
# H0: nenhuma funcao tem poder discriminante
# H1: pelo menos uma funcao tem poder discriminante
lambdaT <- prod(1/(1+fitcan$eigenvalues))   # lambda de Wilks
# estatística de teste
(QQ <- -(n-(p+g)/2-1)*log(lambdaT) )
# p-value
(pvalue <- 1-pchisq(QQ,p*(g-1)))
# rej H0 -> pelo menos 1 das funcoes tem poder disciminante -> repetir teste sem funcao 1

# H0: a funcao 2 nao tem poder discriminante
# H1: a funcao 2 tem poder discriminante
lambdaT <- 1/(1+fitcan$eigenvalues[2])   # lambda de Wilks
# estatística de teste
(QQ <- -(n-(p+g)/2-1)*log(lambdaT) )
# p-value
(pvalue <- 1-pchisq(QQ,(p-1)*(g-1-1)))
# nao rej H0 -> a funcao 2 nao tem poder discriminante


### vi) -----
library(klaR) 
partimat(factor(grupo)~autoconc+rendimento+apoioprof, 
         data = dados, 
         method = "lda")
# nota: sao 3 graficos


### vii) -----
library(caret)
previstos <- predict(fit)
confusionMatrix(previstos$class, dados$grupo)
# 1 R mal classificado
# 1 AR mal classificado
# 90.9% foram bem classificados > 36.4% classificação ao ao acaso
# ...

# validacao cruzada
fitV <- lda(grupo ~autoconc+rendimento+apoioprof, data=dados, CV=T)
table(fitV$class, dados$grupo)
# 2 R mal classificados
# 1 AR mal classificado
# pretendiamos que os resultados se mantivessem. 
# Se tal acontecesse, indicaria que as funções mantinham a sua capacidade de classificação perante novos dados.
# Neste caso, isso não acontece, ou seja, as funções discriminantes ajustadas estao a subestimar a taxa de erro de classificação.

