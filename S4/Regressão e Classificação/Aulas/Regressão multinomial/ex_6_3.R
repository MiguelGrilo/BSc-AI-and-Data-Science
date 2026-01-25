# ******************************************************************************
#                     Regressão e Classificação 2024/25
#
# Exercícios: Regressão Multinomial (e logística)
#
# ******************************************************************************



# ******************************************************************************
#                             ----- Ex. 6.3 -----
# ******************************************************************************

## Pacotes necessarios ---------------------------------------------------------
library(Hmisc)
library(crosstable)
library(forcats)
library(mlogit)    # regressao multinomial
library(lmtest)    # teste RV para modelos mlogit
library(mfp)
library(gam)
library(MASS)
library(summarytools)
library(car)
library(aod)
library(generalhoslem)
library(rms) 
library(DescTools)
library(Epi)
library(caret)
library(ggplot2)
library(nnet)
library(gridExtra)


## Ler dados -------------------------------------------------------------------

# optativo: definir a diretoria atual como diretoria de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dados <- read.csv2("AFsub.csv", fileEncoding = "utf-8", 
                   stringsAsFactors = T, 
                   na.strings = "")

# recordar que havia um NA em instrução
dados <- na.omit(dados)  # permite remover linhas com NA



## a) --------------------------------------------------------------------------
## Análise preliminar ------------------------------------------------------

# Útil para verificar se temos observações suficientes por pares de categorias
table(dados$modalidade, dados$sexo)
table(dados$modalidade, dados$instrucao)  # problemas: poucas observacoes no 2o ciclo -> decidir juntar com 1o ou 3o ciclo
by(dados$idade, dados$modalidade, summary)

# problemas com instrucao!!!
# nota summary de Hmisc e crosstable de crosstable dão erro quando não há observações suficientes

# juntar categorias de instrução
#library(forcats)
dados$instrucao2 <- fct_recode(dados$instrucao, 
                               "3.º ciclo ou inferior" = "1.º ciclo ou inferior",
                               "3.º ciclo ou inferior" = "2.º ciclo",
                               "3.º ciclo ou inferior" = "3.º ciclo")

#library(crosstable)
crosstable(dados,                     # nome do conjunto de dados
           c(sexo, instrucao2, idade), # variáveis x
           by=modalidade,              # variável resposta 
           #total=TRUE,               # se quisermos os totais por linha e coluna
           test=TRUE,                 # TRUE para executar testes de independência ou igualdade das distribuições
           showNA='no',               # para mostrar NA, caso existam
           percent_digits=1) %>%      # número de casas decimais
  as_flextable()                      # para mostrar com aspeto mais apelativo
# todas significativas


## b) --------------------------------------------------------------------------
## Modelos univariados ---------------------------------------------------------

# packages para ajustar modelo multinomial:
# i) library(mlogit)
#    fit1 <- mlogit(y ~ 1 | x, refevel="categoriaRef", data = dados, shape="wide")
#    Seja qual for a ordem das categorias de y, permite definir qual é a categoria
#    de referência. Se nada for referido, considera a 1a
# ii) library(nnet)
#     fit1 <- multinom(y ~ x, data = dados)
#     Atencao: neste caso nao devolve automaticamente o teste de Wald a significancia dos coeficientes
#     Considera a 1a ctegoria como a referênica
# iii) library(VGAM)
#      fit1 <- vglm(y ~ x, data = dados, family=multinomial). 
#      Atencao: neste caso nao devolve automaticamente o teste de Wald a significancia dos coeficientes
#      Considera a ultima categoria como referência

### modelo nulo
fit0 <- mlogit(modalidade ~ 1,             # variavel y~1 | covariaveis
               reflevel = "Caminhada",     # indicar qual a categoria y de referencia
               data = dados, shape="wide") # para informar o R que a base de dados tem um individuo por linha
summary(fit0)


#### Variável idade (quantitativa)
fit1 <- mlogit(modalidade ~ 1 | idade,     # variavel y~1 | covariaveis
               reflevel = "Caminhada",     # indicar qual a categoria y de referencia
               data = dados, shape="wide")
summary(fit1)
# signifcancia geral do modelo (teste RV): p-value<2.22e-16
# avaliar significancia geral da variavel Idade:
anova(fit0, fit1)  # não suporta objetos mlogit
# alternativa
library(lmtest)
lrtest(fit0, fit1)

# ou
#library(nnet)
fit1b <- multinom(modalidade ~  idade, data = dados)
summary(fit1b)
# para podermos comparar resultados dos modelos, é preciso garantir que considera a mesma categoria de referência
dados$modalidade <- relevel(dados$modalidade, ref="Caminhada")
fit1b <- multinom(modalidade ~  idade, data = dados)
summary(fit1b)
#library(lmtest)
fit0b <- multinom(modalidade ~  1, data = dados)
lrtest(fit0b, fit1b) # p<0.001
# ou
anova(fit0b, fit1b)  # p<0.001

# ou
#library(VGAM)
fit1c <- vglm(modalidade ~  idade, family=multinomial, data = dados)
summary(fit1c)
# para podermos comparar resultados dos modelos, é preciso garantir que considera a mesma categoria de referência
dados$modalidade <- factor(dados$modalidade, levels=c("BTT/Ciclismo", "Fitness", "Outra", "Caminhada"))
fit1c <- vglm(modalidade ~  idade, family=multinomial, data = dados)
summary(fit1c)
fit0c <- vglm(modalidade ~  1, family=multinomial, data = dados)
lrtest(fit0c, fit1c)  # não suporta objetos vglm
anova(fit0c, fit1c)   # não suporta objetos vglm


#### Variável sexo (dicotómica)
# pela análise preliminar há observações suficientes
fit1 <- mlogit(modalidade ~ 1 | sexo,         # variavel y~1 | covariaveis
               reflevel = "Caminhada",       # opcional: indicar qual a categoria y de referencia
               data = dados, shape="wide")
summary(fit1)


#### Variável instrucao (categórica com >2 categorias)
# pela análise preliminar NÃO há observações suficientes
fit1 <- mlogit(modalidade ~ 1 | instrucao,         # variavel y~1 | covariaveis
               reflevel = "Caminhada",            # opcional: indicar qual a categoria y de referencia
               data = dados, shape="wide")
summary(fit1)  
# reparar no coef e SD de 2º ciclo

#### Variável instrucao2 (categórica com >2 categorias)
# pela análise preliminar HÁ observações suficientes
fit1 <- mlogit(modalidade ~ 1 | instrucao2,         # variavel y~1 | covariaveis
               reflevel = "Caminhada",            # opcional: indicar qual a categoria y de referencia
               data = dados, shape="wide")
summary(fit1)

# Conclusão da análise univariada: todas as vars com p-value<.25 -> incluir todas na multipla



## c) --------------------------------------------------------------------------
## Modelo multinomial múltiplo -------------------------------------------------

### Modelo multiplo preliminar
### inclui todas as vars que na análise univariada tiveram p-value <.20

fit2 <- mlogit(modalidade ~ 1 | idade + sexo + instrucao2,         # variavel y~1 | covariaveis
               reflevel = "Caminhada",            # opcional: indicar qual a categoria y de referencia
               data = dados, shape="wide")
summary(fit2)


### Remover variáveis não significativas 
## (remover por ordem decrescente de p-value do teste TRV no último modelo em estudo)

# pelos p-values sera que instrucao pode ser removida?
fit2a <- mlogit(modalidade ~ 1 | idade + sexo,
               reflevel = "Caminhada",
               data = dados, shape="wide")
summary(fit2a)
lrtest(fit2, fit2a)  # p=0.1482 -> remover instrucao2

# e se removermos sexo?
fit2b <- mlogit(modalidade ~ 1 | idade + instrucao2,
                reflevel = "Caminhada",
                data = dados, shape="wide")
summary(fit2b)
lrtest(fit2, fit2b)  # p<0.001 -> nao remover sexo

# e se removermos idade?
fit2c <- mlogit(modalidade ~ 1 | sexo + instrucao2,
                reflevel = "Caminhada",
                data = dados, shape="wide")
summary(fit2c)
lrtest(fit2, fit2c)  # p<0.001 -> nao remover idade

# conclusão: remover instrucao2

# Nota: se ajustarmos o modelo recorrendo a vglm então podmeos usar o drop1 para
# remover individualmente cada variável 
fit2 <- vglm(modalidade ~  idade + sexo + instrucao2, family=multinomial, data = dados)
drop1.vglm(fit2, test="LRT")


## d) --------------------------------------------------------------------------
## Interações -------------------------------------------------

fit2a <- mlogit(modalidade ~ 1 | idade + sexo,
                reflevel = "Caminhada",
                data = dados, shape="wide")
fit3 <- mlogit(modalidade ~ 1 | idade * sexo,
                reflevel = "Caminhada",
                data = dados, shape="wide")
summary(fit3)
lrtest(fit3, fit2a)  # p=0.1226 -> nao considerar interacao


## e) --------------------------------------------------------------------------
## Junção de categorias e linearidade ------------------------------------------


# construir modelos logisticos: BTT vs caminhada, Fitness vs Caminhada,
# Outra vs Caminhada para:
# a) averiguar possibilidade de juncao de categorias (para categoricas com >2 categorias)
# b) pressuposto de linearidade com logit (para quantitativas)

# BD1) base de dados para BTT vs caminhada
dados.1 <- subset(dados, modalidade %in% c("BTT/Ciclismo", "Caminhada")) 
dados.1$modalidade <- factor(dados.1$modalidade)  # remover niveis sem observacoes
dados.1$modalidade <- relevel(dados.1$modalidade, ref="Caminhada")
## Modelo logístico associado
fit.l1 <- glm(modalidade ~ idade + sexo, family = binomial(link = logit), data=dados.1)
summary(fit.l1)

# BD2) base de dados para BTT vs caminhada
dados.2 <- subset(dados, modalidade %in% c("Fitness", "Caminhada")) 
dados.2$modalidade <- factor(dados.2$modalidade)  # remover niveis sem observacoes
dados.2$modalidade <- relevel(dados.2$modalidade, ref="Caminhada")
## Modelo logístico associado
fit.l2 <- glm(modalidade ~ idade + sexo, family = binomial(link = logit), data=dados.2)
summary(fit.l2)

# BD3) base de dados para BTT vs caminhada
dados.3 <- subset(dados, modalidade %in% c("Outra", "Caminhada")) 
dados.3$modalidade <- factor(dados.3$modalidade)  # remover niveis sem observacoes
dados.3$modalidade <- relevel(dados.3$modalidade, ref="Caminhada")
## Modelo logístico associado
fit.l3 <- glm(modalidade ~ idade + sexo, family = binomial(link = logit), data=dados.3)
summary(fit.l3)

summary(fit2a)
# Nota: os coeficientes dos logits idade e sexo do modelo multinomial são muito 
# idênticos aos coeficientes dos logits dos modelos binomias 2 e 3


## a) juncao de categorias: neste caso nao se aplica
# o que fazer:
# i) comparar coeficientes dentro de cada modelo
# ii) usar teste de wald para comparar
# iii) comparar modelos recorrendo ao AIC ou BIC

library(car)
crPlot(fit.l1, variable="idade")
crPlot(fit.l2, variable="idade")
crPlot(fit.l3, variable="idade")

## b) linearidade
# i) opção: via metodo LOWESS (Mínimos quadrados ponderados localmente)
plot(lowess(predict(fit.l1) ~ dados.1$idade), xlab="idade",  
     ylab="logit = log(OR)", type="l")
plot(lowess(predict(fit.l2) ~ dados.2$idade), xlab="idade",  
     ylab="logit = log(OR)", type="l")
plot(lowess(predict(fit.l3) ~ dados.3$idade), xlab="idade",  
     ylab="logit = log(OR)", type="l")
# todas apresentam um comportamento linear -> ok

# ii) opção: via metodo dos quartis
#     1o categorizar a variável idade em 4 classes com igual número de observacoes (i.e., usar quartis)
dados.1$IdadeCat<- cut(dados.1$idade, breaks=quantile(dados.1$idade), right=FALSE, include.lowest=TRUE)
dados.2$IdadeCat<- cut(dados.2$idade, breaks=quantile(dados.2$idade), right=FALSE, include.lowest=TRUE)
dados.3$IdadeCat<- cut(dados.3$idade, breaks=quantile(dados.3$idade), right=FALSE, include.lowest=TRUE)
table(dados.1$IdadeCat)
table(dados.2$IdadeCat)
table(dados.3$IdadeCat)
#     2o ajustar modelos substituindo a variável quantitativa pela versão categorizada
fit.l1a <- update(fit.l1, ~ . - idade + IdadeCat)
fit.l2a <- update(fit.l2, ~ . - idade + IdadeCat)
fit.l3a <- update(fit.l3, ~ . - idade + IdadeCat)
summary(fit.l1a)
summary(fit.l2a)
summary(fit.l3a)
#     3o avaliar linearidade dos coeficientes
#     um gráfico ajuda a visualizar: para isso precisamos dos pontos médios das classes: (linf + lsup)/2
pontosmedios <- function(variavel){
  c((min(variavel) + quantile(variavel, 0.25))/2, 
         (quantile(variavel, 0.25) + quantile(variavel, 0.5))/2, 
         (quantile(variavel, 0.5) + quantile(variavel, 0.75))/2, 
         (quantile(variavel, 0.75) + max(variavel))/2)
}
x <- pontosmedios(dados.1$idade)
y <- c(0, as.numeric(fit.l1a$coef[3:5])) # coeficientes associados a IdadeCat
plot(x, y, type="b")
# Existe um pequeno desvio da linearidade. 
x <- pontosmedios(dados.2$idade)
y <- c(0, as.numeric(fit.l2a$coef[3:5])) # coeficientes associados a IdadeCat
plot(x, y, type="b")
# desvio da linearidade 
x <- pontosmedios(dados.3$idade)
y <- c(0, as.numeric(fit.l3a$coef[3:5])) # coeficientes associados a IdadeCat
plot(x, y, type="b")
# desvio da linearidade 

# iii) opção: via método dos polinómios fracionários
#library(mfp)
fit.l1b <- mfp(modalidade ~ fp(idade) + sexo,  # usar a função fp na variável quantitativa
             data = dados.1, family = binomial(link = logit))
summary(fit.l1b)
# sugere transformacao linear (/100)
fit.l2b <- mfp(modalidade ~ fp(idade) + sexo,  # usar a função fp na variável quantitativa
               data = dados.2, family = binomial(link = logit))
summary(fit.l2b)
# sugere transformacao não linear (^-.5)
fit.l3b <- mfp(modalidade ~ fp(idade) + sexo,  # usar a função fp na variável quantitativa
               data = dados.3, family = binomial(link = logit))
summary(fit.l3b)
# sugere transformacao não linear (^-1)



# iv) opção: usando a formulação gam
library(mgcv)
fit.l1c <- gam(modalidade ~ s(idade) + sexo,  # usar a função fp na variável quantitativa
               data = dados.1, family = binomial(link = logit))
summary(fit.l1c)
# edf=1 -> transformação linear
# sugere transformacao linear (/100)
fit.l2c <- gam(modalidade ~ s(idade) + sexo,  # usar a função fp na variável quantitativa
               data = dados.2, family = binomial(link = logit))
summary(fit.l2c)
# edf=2.2 -> transformação não linear
fit.l3c <- gam(modalidade ~ s(idade) + sexo,  # usar a função fp na variável quantitativa
               data = dados.3, family = binomial(link = logit))
summary(fit.l3c)
# edf=2.5 -> transformação não linear
# ou graficamente
par(mfrow=c(1,3))
plot(fit.l1c)
plot(fit.l2c)
plot(fit.l3c)

# conclusao: dado que temos transformações diferentes, talvez se justificasse categorizar a variavel idade 
# vamos experimentar com a idade invertida
fit2.o  <- nnet::multinom(modalidade ~ idade + sexo, data = dados)
fit2.t1 <- nnet::multinom(modalidade ~ I(1/idade^(-.5)) + sexo, data = dados)
fit2.t2 <- nnet::multinom(modalidade ~ I(1/idade) + sexo, data = dados)
# comparar os dois modelos usando o valor p calculado a partir de 1-(pchisq(deviance(modelo.original)-deviance(modelo.com.variavel.transformada)), gl)
#    com gl=1 no caso de ser uma transformação simples, ou gl=3 no caso de sugerir a transformação dupla
1-pchisq(deviance(fit2.o)-deviance(fit2.t1), 1)  # p=0.013 -> optar pelo modelo com variavel tansformada
1-pchisq(deviance(fit2.o)-deviance(fit2.t2), 1)  # p<0.0001 -> optar pelo modelo com variavel tansformada
# comparar os AIC
AIC(fit2.o, fit2.t1, fit2.t2)
# optar por 1/idade

# criar variavel transformada
dados$idadeInv <- 1/dados$idade
# ajusta modelo
dados$modalidade <- relevel(dados$modalidade, ref="Caminhada")
fitF <- mlogit(modalidade ~ 1 | idadeInv + sexo, 
               reflevel = "Caminhada",   # indicar qual a categoria y de referencia
               data = dados, shape="wide")
fitFb <- nnet::multinom(modalidade ~ idadeInv + sexo, data = dados)
# para podermos comparar resultados dos modelos, é preciso garantir que considera a mesma categoria de referência
dados$modalidadeVGAM <- factor(dados$modalidade, levels=c("BTT/Ciclismo", "Fitness", "Outra", "Caminhada"))
fitFc <- vglm(modalidadeVGAM ~  idadeInv + sexo, family=multinomial, data = dados)

summary(fitF)


## f) --------------------------------------------------------------------------
## Adequabilidade e bondade

### adequabilidade
summary(fitF)

### bondade do ajustamento
library(generalhoslem)          # ativar pacote necessario
generalhoslem::logitgof(dados$modalidade, fitted(fitF, outcome = FALSE))
generalhoslem::logitgof(dados$modalidade, fitted(fitF, outcome = FALSE))$expected
generalhoslem::logitgof(dados$modalidade, fitted(fitF, outcome = FALSE), g=6)
generalhoslem::logitgof(dados$modalidade, fitted(fitF, outcome = FALSE), g=6)$expected


### averiguar a bondade do ajustamento dos modelos individuais
# recordar que ja ajustamos os modelos logisticos "individuais" mas com idade não foi com idadeInv
fit.l1t <- glm(modalidade ~ I(1/idade) + sexo, family = binomial(link = logit), data=dados.1)
fit.l2t <- glm(modalidade ~ I(1/idade) + sexo, family = binomial(link = logit), data=dados.2)
fit.l3t <- glm(modalidade ~ I(1/idade) + sexo, family = binomial(link = logit), data=dados.3)

# R2 de Nagelkerke (e R2 de McFadden)
pscl::pR2(fit.l1t) [c(4,6)]  # R2 de McFadden e R2 de Nagelkerke
pscl::pR2(fit.l2t) [c(4,6)]  # R2 de McFadden e R2 de Nagelkerke
pscl::pR2(fit.l3t) [c(4,6)]  # R2 de McFadden e R2 de Nagelkerke

# Testes de ajustamento: H0: modelo ajusta-se vs H1: modelo nao se ajusta
# a) Teste de Hosmer e Lemeshow 
generalhoslem::logitgof(dados.1$modalidade, fitted(fit.l1t, outcome = FALSE))
generalhoslem::logitgof(dados.2$modalidade, fitted(fit.l2t, outcome = FALSE)) 
generalhoslem::logitgof(dados.3$modalidade, fitted(fit.l3t, outcome = FALSE)) 
generalhoslem::logitgof(dados.3$modalidade, fitted(fit.l3t, outcome = FALSE))$expected 
generalhoslem::logitgof(dados.3$modalidade, fitted(fit.l3t, outcome = FALSE), g=6) 


# b) Teste de Cessie-van Houwelingen e coef Brier
fit.l1.lrm <- lrm(modalidade ~ idadeInv + sexo, data=dados.1, x=TRUE, y=TRUE)
resid(fit.l1.lrm, 'gof') # Valor p=0.1670925   -> OK
print(fit.l1.lrm) # Brier= 0.163  # queremos que seja pequeno

fit.l2.lrm <- lrm(modalidade ~ idadeInv + sexo, data=dados.2, x=TRUE, y=TRUE)
print(fit.l2.lrm) # Brier= 0.180  # queremos que seja pequeno
resid(fit.l2.lrm, 'gof') # Valor p=0.4664915 

fit.l3.lrm <- lrm(modalidade ~ idadeInv + sexo, data=dados.3, x=TRUE, y=TRUE)
print(fit.l3.lrm) # Brier= 0.165  # queremos que seja pequeno
resid(fit.l3.lrm, 'gof') # Valor p=0.5320719


## g) --------------------------------------------------------------------------
## equacoes --------------------------------------------------------------------
coefficients(fitF)
summary(fitFb)

## h) --------------------------------------------------------------------------
## probabilidades previstas ----------------------------------------------------
novosdados <- data.frame(idade=10:80, sexo=as.factor(rep("Masculino", 71)))
novosdados$idadeInv <- 1/novosdados$idade
probsPreditas <- predict(fitF, newdata=novosdados)
probsPreditas <- predict(fitFb, newdata=novosdados, type="probs")



## i) --------------------------------------------------------------------------
## capacidade discriminativa ---------------------------------------------------
preditos <- predict(fitF)
head(preditos)  # probabilidade predita
# problemas com predict quando se usa o mlogit

probPred <- predict(fitFb, type="probs")
head(probPred)  # probabilidade predita
modalidadePred <- predict(fitFb, type="class")
head(modalidadePred)  # modalidade predita
confusionMatrix(dados$modalidade, modalidadePred)
#o que há a referir?

probPred <- predict(fitFc, type="response")
head(probPred)  # probabilidade predita
confusionMatrix(dados$modalidade, modalidadePred)
#o que há a referir?

table(dados$modalidade)

# experimentar categorizar idade ...


