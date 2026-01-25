# ******************************************************************************
#                     Regressão e Classificação 2024/25
#
# Exercícios: Regressão Multinomial (e logística)
#
# ******************************************************************************



# ******************************************************************************
#                             ----- Ex. 6.2 -----
# ******************************************************************************

## Pacotes necessarios ---------------------------------------------------------
library(summarytools)     # para analise preliminar
library(Hmisc)            # para analise preliminar
library(crosstable)       # para analise preliminar
library(gam)
library(MASS)
library(mfp)              # polinomios fracionarios: mfp e fp
library(car)              # multicoliniearidade GVIF e medidas de influencia: 
library(aod)              # teste de Wald: wald.test
library(generalhoslem)    # teste de Hosmer e Lemeshow
library(rms)              # teste de Cessie-van Houwelingen
library(DescTools)        # pseudo-R2
library(Epi)              # curva ROC e AUC
library(caret)            # medidas de discriminação: confusionMatrix
library(ggplot2)


## Ler dados -------------------------------------------------------------------

# optativo: definir a diretoria atual como diretoria de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dados <- read.csv2("AFsub.csv", fileEncoding = "utf-8", 
                   stringsAsFactors = T, 
                   na.strings = "")
str(dados)



## a) --------------------------------------------------------------------------
## Contar NA -------------------------------------------------------------------

summary(dados) 
# Ha 1 "vazio" em instrucao

# alternativa
apply(dados,                      # conjunto de dados
      2,                          # aplicar a função a cada coluna
      function(x) sum(is.na(x)))  # criar função com o que se pretende obter
# neste caso contar o número de NA por coluna x

# alternativa mais apelativa
library(summarytools)
view(dfSummary(dados)) # gráfico e pequeno resumo de todas as variáveis


# Nota: se existirem valores omissos decidir se:
# - eliminar variável com valores omissos
# - eliminar linhas com valores omissos
# - imputar valores aos NA
# A escolha depende da quantidade de NA e do tipo de NA 

dados <- na.omit(dados)  # permite remover linhas com NA



## b) --------------------------------------------------------------------------
## Criar variavel caminhada ----------------------------------------------------

dados$caminhada <- factor(ifelse(dados$modalidade=="Caminhada", "Sim", "Não"))
#confirmar se a variável foi criada corretamente
table(dados$modalidade, dados$caminhada)



## c) --------------------------------------------------------------------------
## Análise univariada ------------------------------------------------------

# Útil para verificar se temos observações suficientes por pares de categorias
table(dados$caminhada, dados$sexo)
table(dados$caminhada, dados$instrucao)
by(dados$idade, dados$caminhada, summary)

# Alternativa mais expedita
library(Hmisc)
summary(caminhada ~ sexo + instrucao + idade,  # var resposta ~ variáveis x
        data=dados,       # nome do conjunto de dados
        fun=table)        # função a usar

# Alternativa mais expedita e apelativa, e com possibilidade de incluir teste
library(crosstable)
crosstable(dados,                     # nome do conjunto de dados
           c(sexo, instrucao, idade), # variáveis x
           by=caminhada,              # variável resposta 
           #total=TRUE,               # se quisermos os totais por linha e coluna
           test=TRUE,                 # TRUE para executar testes de independência ou igualdade das distribuições
           showNA='no',               # para mostrar NA, caso existam
           percent_digits=1) %>%      # número de casas decimais
  as_flextable()                      # para mostrar com aspeto mais apelativo


# caso se pretenda trocar a categoria de referência de uma variável, por ex., instrucao
# dados$instrucao <- relevel(dados$instrucao, ref="Superior")


### Também se podem ajustar os modelos univariados 
# (equivalente a realizar o teste do qui-quadrado de independencia no caso das variaveis categoricas)

### 1o. ajustar modelo nulo -----

# modelo nulo
fit0 <- glm(caminhada ~ 1,    # ~ 1 indica que so se considera a constante (modelo nulo) 
            data = dados,                     # nome do conjunto de dados
            family = binomial(link = logit))  # distribuicao de Y e função de ligacao
# ver modelo ajustado
summary(fit0)

# Nota: este modelo apenas tem o parametro b0. 
# Modelo ajustado: logit(P(Y=1)) = logit(pi) = logit(P(caminhada=Sim)) = b0 = -0.6890
# obs: na regressao logística, habitualmente b0 não tem significado.

(OR <- exp(fit0$coefficients))
# curiosidade: como surge o valor do OR? ver valores da tabela de contingencia
# table(dados$caminhada) 
# exp(b0) = 121/241 = (n Sim)/(n Não)

(OR-1)*100      # (exp(-0.6890)-1)*100 
# há 49.8% menos chances de ser praticante de caminhada

# ou invertendo
(1/OR)
# Há aprox. 2x mais chances de não ser praticante de caminhadas


### 2o. Ajustar modelos univariados -----

#### Variável idade (quantitativa)
fit1 <- glm(caminhada ~ idade,  
            data = dados, family = binomial(link=logit))
summary(fit1)  # p-value Wald=0.105
# Modelo ajustado: logit(P(Y=1)) = logit(pi) = -3.241395 +0.055644*idade

# Interpretação direta dos coeficientes beta
(betas <- fit1$coefficients)  # coeficientes
# b0=-3.24139531 -> logit(peso<2500gr) predito para residentes com idade 0
# b(idade)=0.05564354 -> por cada aumento unitário na idade o logit(P(caminhada=Sim))
# aumenta, em média, 0.05564354 
# esta interpretação não é "simpática"...

# Interpretação via OR (preferível)
(OR <- exp(fit1$coefficients))
# OR(idade)=1.05722076 > 1 -> (0.9501333 -1)*100% =5.7% 
#   Há cerca de 5.7% mais possibilidades de ser praticante de caminhada por cada  
#   aumento de 1 ano na idade do residente

# Significância da variável
# H0: variável idade significativa vs H1: variável idade não significativa
# i) opção: via teste Wald
#    H0: beta(idade)=0 vs H1: beta(idade) diferente de 0
summary(fit1)  # p-value Wald < 0.001
# ii) opção: via teste da razão de verosimilhanças (TRV)
#     H0: modelo nulo vs H1: modelo com variável idade
anova(fit0, fit1, test = "Chisq") # p-value TRV < 0.001 
#     alternativa: apenas no caso univariado 
#anova(fit1, test="Chisq")
# Conclusão: como p-value < 0.2 -> reter para etapa multipla



#### Variável sexo (dicotómica)
# pela análise preliminar (alinea b) há observações suficientes e significativa
fit1 <- glm(caminhada ~ sexo, 
            data = dados, family = binomial(link = logit))
summary(fit1)  # p-value Wald 
# Modelo ajustado: logit(P(Y=1)) = logit(pi) = -0.2814 - 0.9103*Masculino

# Interpretação direta dos coeficientes beta
(betas <- fit1$coefficients)  # coeficientes
# b0=-0.2814125 -> logit(P(caminhada=Sim)) predito para residentes femininos
# b(Masculino)=-0.9102903 -> se for masculino o logit(P(caminhada=Sim)) diminui 0.91
# esta interpretação não é "simpática"...

# Interpretação via OR (preferível)
(OR <- exp(fit1$coefficients))
# OR(beta(Masculino))= 0.4024074 < 1 -> (0.4024074 -1)*100% = -59.75926
#  Há 59% menos de possibilidades de ser praticante de caminhada se for do sexo 
#  masculino relativamente a ser do sexo feminino
#
# Curiosidade: como surgem os valores dos OR? ver valores da tabela de contingencia
# table(dados$caminhada, dados$sexo)
#   exp(b0) = ..

# Significância da variável
# H0: variável sexo não significativa vs. H1: variável sexo significativa
# Na análise univariada coincide com a significância geral do modelo, i.e.,
# H0: modelo nulo (low ~ 1) vs H1: modelo atual
# i) Opção: via teste de Wald
#    H0: b1=0 vs H1: b1 diferente de 0
summary(fit1)
# ii) Opção: via teste da razão de verosimilhanças (TRV) 
#     H0: modelo nulo (low ~ 1) vs H1: modelo atual
#     anova(modelo1, modelo2, test="Chisq")  # em que um dos modelos está encaixado no outro
#                                            # test="Chisq" ou "LRT" devolvem o TRV
anova(fit0, fit1, test="Chisq")  
#     alternativa: apenas no caso univariado 
anova(fit1, test="Chisq")
#     p-value TRV =  
# Conclusao: p-value < 0.2 -> reter para analise multipla



#### Variável instrucao (categórica com >2 categorias)
# pela análise preliminar (alinea b) há observações suficientes e significativa a 10%
fit1 <- glm(caminhada ~ instrucao, 
            data=dados, family=binomial(link=logit))
summary(fit1)  # Nota: 4 coeficientes para a variável instrucao 
# Modelo ajustado: logit(P(Y=1)) = logit(pi) = 

# Interpretação
(OR <- exp(fit1$coefficients))
#...


# Avaliar a significância global da variável
# H0: variável instrucao não significativa vs H1: variável instrucao não significativa
# i) opção via teste RV
#    H0: modelo nulo vs H1: modelo com variável race
anova(fit0, fit1, test="Chisq") 
# ou 
# anova(fit1, test="Chisq")
#     p-value TRV < 0.001
# ii) opcao via teste de Wald
#     H0: beta2=beta3=beta4=beta5=0 vs H1: pelo menos um dos betas (beta2 a beta5) diferente de zero
library(aod)
wald.test(vcov(fit1),  # matriz e variancias e covariancias
          coef(fit1),  # vector com coeficientes
          Terms = 2:5) # coeficientes a condiderar
#     p-value Wald < 0.001
# Conclusão: como p-value<0.2 -> reter para etapa multipla


# Conclusão da análise univariada: todas as vars com p-value<.25 -> incluir todas na multipla



#### ---- Uma alternativa mais rápida que usa o teste RV
# 1o ajustar modelo nulo (fit0)
# 2o usar a função add1 que vai percorrer todos os modelos univariados que indicarmos
add1(fit0,  # modelo nulo 
     scope = ~ idade + instrucao + sexo, # lista das covariaveis
     test = "Chisq")   # teste TRV
# igual a fazer várias vezes
# anova(fit0, fitX, test="Chisq")  # sendo fitX o nome do modelo ajustado considerando a covar X




## c) --------------------------------------------------------------------------

### i) -------------------------------------------------------------------------
### Modelo logístico múltiplo preliminar ---------------------------------------
### inclui todas as vars que na análise univariada tiveram p-value <.20

fit2 <- glm(caminhada ~ idade + instrucao + sexo,  
            data = dados, family = binomial(link = logit)) 
summary(fit2)

### Remover variáveis não significativas 
## (remover por ordem decrescente de p-value do teste TRV no último modelo em estudo)

drop1(fit2, test="Chisq") # remove individualmente cada variável
# variável não significativa: instrucao -> remover

fit2a <- glm(caminhada ~ idade + sexo,  
             data = dados, family = binomial(link = logit)) 
# igual a 
fit2a <- update(fit2, ~ . - instrucao)
summary(fit2a)
# ATENÇÃO: Devem-se confrontar os coeficientes do modelo com (fit2) e sem a 
# variavel (fit2a). Se existirem alterações substanciais nos coeficientes, então 
# não remover a variável.
fit2$coef
fit2a$coef
# não há alterações relevantes :-)
# opcional pois devolve o p-value obtido na instrução drop1
anova(fit2, fit2a, test="Chisq")  # manter modelo mais simples fit2a 


# ALTERNATIVA: selecionar variaveis com base no criterio AIC (NAO preferível)
# objetivo: minimizar criterio AIC
# metodos: forward, backward, stepwise (=both)
# instrucao: step(modelo, scope, direction = c("both", "backward", "forward"))

# pelo metodo backward: partindo do modelo multivariado
step(fit2, direction = "backward")                # apresenta valores do AIC
# ou
step(fit2)  # por defeito aplica o método backward se não for indicado o argumento scope
step(fit2, test="Chisq")  # também apresenta o teste RV
# obs: nesta opção a variável ui manteve-se no modelo
# se pretendermos guardar o modelo selecionado:
(fit2.back <- step(fit2, test="Chisq"))  # backward via AIC e mostra teste RV
summary(fit2.back)
# modelo escolhido: caminhada ~ idade + sexo


# pelo metodo forward
(fit2.forw <- step(fit0, # ponto de partida: modelo nulo
                   scope = ~ idade + instrucao + sexo,  # modelo mais completo 
                   direction = "forward",
                   test="Chisq"))
summary(fit2.forw)
# modelo escolhido: caminhada ~ idade + sexo  

# pelo metodo stepwise
(fit2.step <- step(fit0, # ponto de partida: modelo nulo
                   scope = ~ idade + instrucao + sexo,  # modelo mais completo 
                   direction = "both",
                   test="Chisq"))
# igual a
(fit2.step <- step(fit0, # ponto de partida: modelo nulo
                   scope = ~ idade + instrucao + sexo,  # modelo mais completo 
                   test="Chisq"))  # por defeito aplica o método stepwise se for indicado o argumento scope
summary(fit2.step)
# modelo escolhido: caminhada ~ idade + sexo



### ii) ------------------------------------------------------------------------
### Avaliar junção de categorias -----------------------------------------------
### (só se aplica a variáveis com 3 ou + categorias)

# i) Opção: via Intervalo de Confiança de Wald (IC)
#    construir IC (de Wald) para a diferenca entre os coefs a juntar

# ii) Opção: via Teste de Hipóteses (TH)
#     via TH (de Wald) (equivalente ao IC anterior)
#     H0: beta.K1=beta.K2 vs H1:beta.K1 diferente de beta.K2
#library(aod) 
#wald.test(vcov(modelo), # matriz e variancias e covariancias
#          coef(modelo), # coeficiente
#          L = cbind(0,0,1,...,-1,0,0,0)) # colocar 1 e -1 nos coeficientes a comparar

# caso se juntem categorias, para comparar os modelos com e sem juncao de categorias
#AIC(modelo.inicial, modelo.com.novas.ategorias)    # devolve o num parametros e o AIC de cada modelo



### iii) -----------------------------------------------------------------------
### Avaliar pressuposto de linearidade -----------------------------------------
### (só se aplica a variáveis quantitativas)

# recordar o modelo modelo atual (alínea f)
summary(fit2a)
# Só há uma variável quantitativa: lwt

# podemos avaliar por 3 métodos: i) lowess, ii) quartis, iii) polinómios fracionários 

# i) opção: via metodo LOWESS (Mínimos quadrados ponderados localmente)
plot(lowess(predict(fit2a) ~ dados$idade),  # valores logit preditos ~ valores observados para a var quantitativa
     type="l",                              # gráfico de linhas
     xlab="idade",                          # título do eixo x (nome da var quantitativa)
     ylab="logit = log(OR)")                # título do eixo y
# Deve apresentar um comportamento linear
# Perfeito! 


# ii) opção: via metodo dos quartis
#     1o categorizar a variável lwt em 4 classes com igual número de observacoes (i.e., usar quartis)
dados$IdadeCat<- cut(dados$idade,                   # variavel a categorizar
                   breaks=quantile(dados$idade),  # pontos de corte das classes (min,Q1,Q2,Q3,max)
                   right=FALSE,                  # classes abertas a direita (mas e indiferente) 
                   include.lowest=TRUE)          # para garantir que min e max estao incluidos nas classes
# igual a:
#dados$IdadeCat<- cut(dados$idade,                # variavel a categorizar
#                    breaks=c(min(dados$idade),  # lista com os pontos de corte das classes: comeca no minimo e acaba no maximo
#                             quantile(dados$idade, 0.25), 
#                             quantile(dados$idade, 0.5), 
#                             quantile(dados$idade, 0.75), 
#                             max(dados$idade)),
#                    right=FALSE, include.lowest=TRUE) 
table(dados$IdadeCat)
#     2o ajustar modelo substituindo a variável quantitativa pela versão categorizada
fit3a <- update(fit2a, ~ . - idade + IdadeCat)
# igual a 
#fit3b <- glm(caminhada ~ sexo + IdadeCat,
#             data = dados, family = binomial(link = logit))
summary(fit3a)
#     3o avaliar linearidade dos coeficientes
#     um gráfico ajuda a visualizar: para isso precisamos dos pontos médios das classes: (linf + lsup)/2
x <- c((min(dados$idade) + quantile(dados$idade, 0.25))/2, 
       (quantile(dados$idade, 0.25) + quantile(dados$idade, 0.5))/2, 
       (quantile(dados$idade, 0.5) + quantile(dados$idade, 0.75))/2, 
       (quantile(dados$idade, 0.75) + max(dados$idade))/2)
y <- c(0, as.numeric(fit3a$coef[3:5])) # coeficientes associados a IdadeCat
plot(x, y, type="b")
# ou com linha suavizada
plot(x, y)
lines(lowess(x,y))  
# Deve apresentar um comportamento linear
# Existe um pequeno desvio da linearidade. 
# Não parece ser suficiente para a colocar verdadeiramente em causa o pressuposto
# Devemos sempre conjugar este resultado com o de outros métodos e só depois decidir


# iii) opção: via método dos polinómios fracionários
library(mfp)
fit3b <- mfp(caminhada ~ fp(idade) + sexo,  # usar a função fp na variável quantitativa
             data = dados, family = binomial(link = logit))
summary(fit3b)
# Transformação sugerida (idade/100)^.5, ou seja, trata-se uma transformacao linear (/100)
# e uma não linear, 1/sqrt! 

# Se for sugerida uma transformacao então:
# 1) comparar os dois modelos usando o valor p calculado a partir de 1-(pchisq(deviance(modelo.original)-deviance(modelo.com.variavel.transformada)), gl)
#    com gl=1 no caso de ser uma transformação simples, ou gl=3 no caso de sugerir a transformação dupla
1-pchisq(deviance(fit2a)-deviance(fit3b), 1)  # p=0.005 -> optar pelo modelo com vaiavel tansformada
1-pchisq(deviance(fit2a)-deviance(fit3a), 1)  # p=0.004 -> optar pelo modelo com vaiavel tansformada
# 2) comparar também os AIC dos dois modelos: AIC(modelo.original, modelo.com.variavel.transformada) 
AIC(fit2a, fit3b)
# nota: se não há concordância entre os métodos, devemos sempre ter em atenção a interpretação 


### iv) ------------------------------------------------------------------------
### Multicolinariedade ---------------------------------------------------------
library(car)
vif(fit2a)
# todos os VIF estao proximos de 1 logo nao ha problemas de multicolinearidade


### v) -------------------------------------------------------------------------
### Intracções -----------------------------------------------------------------

fit4 <- update(fit2a, ~ . + sexo:idade)
summary(fit4)  
anova(fit2a, fit4, test="Chisq")
# interacao não significativa

# modelo final para proximas etapas
fitF <- fit2a


### vi) ------------------------------------------------------------------------
### Adequabilidade e bondade do ajustamento ------------------------------------

# --- significância geral do modelo
# H0: modelo nulo vs H1: modelo atual
anova(fit0, fitF)
# há pelo menos 1 parametro significativo


# --- bondade do ajustamento
# opções: 
# i) Teste de Hosmer-Lemeshow: mais útil quando há vários preditores e/ou preditores quantitativos
# ii) Teste de Cessie-van Houwelingen: quando há pelo menos 1 preditor quantitativo
# iii) estatistica deviance: quando só temos variáveis categóricas
# iv) Coeficientes pseudo-R2 de Nagelkerke e de McFadden (os mais usuais)
# v) Coeficiente Brier 

# i) opção: Teste de Hosmer-Lemeshow
# H0: o modelo ajusta-se aos dados vs H1: o modelo não se ajusta aos dados
library(generalhoslem)          # ativar pacote necessario
(hl<-logitgof(dados$caminhada,  # valores observados y (dependente)
              fitted(fitF),    # valores ajustados y^ 
              g = 10))          # numero de classes a considerar
# nao esquecer que se trata de um teste QQ, logo observar os valores esperados para averiguar se é necessario considerar g<10
hl$expected                     # valores esperados
# sem problemas
# nota: logitgof tem as versões adaptadas para modelos logisticos, multinomiais e ordinais

# exitem pacotes alternativos:
#library(ResourceSelection)
#hoslem.test(as.numeric(dados$caminhada)-1, fitted(fitF), g=10) # so para modelo logistico

# conclusão: o modelo ajusta-se aos dados


# ii) opção: Teste de Cessie e van Houwelingen
# H0: o modelo ajusta-se aos dados vs H1: o modelo não se ajusta aos dados
library(rms) 
fitF.1 <- lrm(caminhada ~ idade + sexo,
               data = dados,            # nome do conjunto de dados
               x = TRUE, y = TRUE)      # para podermos usar a instrucao resid()
# lrm: funcao alternativa para ajustar um modelo logístico ou um modelo ordinal
print(fitF.1)  # opcional: para visualizar o modelo ajustado. Neste caso não se usa a funçã summary
# Model Likelihood Ratio Test: devolve teste a significância geral do modelo
# Discrimination Indexes: R2 = Nagelkerke

# teste de Cessie
resid(fitF.1, 'gof')  # "gof" para devolver o teste de ajustamento de Cessie e van Houwelingen
# p-value = 0.104 -> o modelo ajusta-se aos dados 

# iii) opção: teste Deviance (SE NÃO só tivessemos variáveis categóricas)
resumo <- summary(fitF)           # modelo ajustado
(gl <- resumo$df.residual)        # graus de liberdade do qui-quadrado
(ET <- resumo$deviance)           # Deviance
(valorp <- 1 - pchisq(ET,gl))     # valor p

# v) opção: coeficientes pseudo-R2 de Nagelkerke e de McFadden
print(fitF.1)  # Discrimination Indexes: R2 = Nagelkerke

# outros pseudo-R2
library(DescTools)
DescTools::PseudoR2(fitF, which = c("McFadden", "CoxSnell", "Nagelkerke"))
#  McFadden   CoxSnell Nagelkerke 
# 0.1859167  0.2109407  0.2928212 
# R2 de McFadden= 0.1859 -> 18.6% de ganho de informacao do modelo fitF em relacao ao modelo nulo
# ou em alternativa
#library(pscl)
#pR2(fitF)


# iv) opção: coeficiente Brier score
#     ( 0 <= Brier <= 1, quanto mais proximo de 0 melhor)
#     = media(erros^2): mede a precisão das predições. Queremos que esteja próximo de zero
#     NÃO usar quando estamos perante eventos muitos raros, i.e, quando o número de sucessos ou insucessos é muito pequeno
print(fitF.1)
# Brier= 0.175 -> próximo de 0, o que é bom



### vii) -----------------------------------------------------------------------
###  Capacidade discriminativa -------------------------------------------------

# -- curva ROC
library(Epi)
ROC(form=caminhada ~idade+sexo,
    dat=dados,
    plot="sp")
# o ponto de corte é obtido onde se cruza a sensibilidade e especificidade

# versão com valor do ponto de corte
ROC(form = caminhada~sexo+idade,
    data=dados, 
    plot="ROC",
    PV=T, MX=T,
    AUC=T)
# AUC=0.796 (para ponto de corte 0.296) -> capacidade discriminativa razoável/boa
# devolve a P(caminhar|caminha) > P(caminhar|não caminha), a um indivíduo escolhido ao acaso
# Para um ponto de corte optimo=0.296 tem-se uma sensibilidade de 79.3% e uma especificadade de 69.3%

# matriz de confusão
# 1o obter probabilidades preditas
probsPred <- predict(fitF, type="response")  # prob prevista
# 2o obter a classificacao com base no ponto de corte obtido na analise da curva ROC
pontoCorte <- 0.296
categoriaPred <- as.factor(ifelse(probsPred >= pontoCorte, "Sim", "Não"))
# atencao: categoriaPred tem que estar no mesmo formato da variavel Y

library(caret)
confusionMatrix(data = categoriaPred, dados$caminhada)
# problema: sensibilidade e especificidade trocadas relativamente a ROC
# isto acontece porque ROC considera a categoria de ref a última, e o caref considera a 1a
confusionMatrix(data = categoriaPred, dados$caminhada, 
                positive = "Sim")  # indicar explicitamente a categoria de referencia  

# se quisermos aumentar a especificidade, basta alterar o ponto de corte
pontoCorte <- 0.33
categoriaPred <- as.factor(ifelse(probsPred >= pontoCorte, "Sim", "Não"))
confusionMatrix(data = categoriaPred, dados$caminhada, 
                positive = "Sim")  # indicar explicitamente a categoria de referencia  
# claro que muda a sensibilidade, bem como as outras medidas...



## e) --------------------------------------------------------------------------

### i) -------------------------------------------------------------------------
### Equação do modelo ----------------------------------------------------------
fitF$coefficients

### ii) ------------------------------------------------------------------------
### Intervalos de confiança ----------------------------------------------------
confint(fitF)

### iii) -----------------------------------------------------------------------
### Estimar probabilidades -----------------------------------------------------

# dados com o perfil pretendido
novosF <- data.frame(idade=c(30, 40, 50), sexo="Feminino")
# probabilidade predita
predict(fitF, newdata = novosF, type = "response")

### iv) ------------------------------------------------------------------------
### Perfis de probabilidade ----------------------------------------------------

# dados com o perfil pretendido
novosFM <- data.frame(idade=rep(seq(15, 80, 1), 2), 
                      sexo=as.factor(c(rep("Feminino", 66), 
                                       rep("Masculino",66))))  # nao esquecer de definir como fator
# juntar aos dados anteriores os logit preditos
novosFM <- cbind(novosFM, 
                  predict(fitF, newdata = novosFM, type = "link", se = TRUE))
# juntar aos dados anteriores os limites dos IC a 90%
novosFM <- within(novosFM, {
  ProbPrev <- plogis(fit)
  LL <- plogis(fit - qnorm(0.95) * se.fit)
  UL <- plogis(fit + qnorm(0.95) * se.fit)
})
# grafico com evolucao do perfil de probabilidade dos homens e mulheres com idade entre 15 e 80
library(ggplot2)
ggplot(novosFM, aes(x = idade, y = ProbPrev)) + 
  geom_ribbon(aes(ymin = LL,ymax = UL, fill = sexo), alpha = 0.2) + 
  geom_line(aes(colour = sexo), size = 1) +
  xlab("Idade") +                                 # titulo do eixo x
  ylab("Probabilidade prevista de caminhar")      # titulo do eixo y