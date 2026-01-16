# ******************************************************************************
#                     Regressao e Classificacao 2024/25
#
# Exercicios: Regressão linear multipla
#
# ******************************************************************************



# ----------------------------- Ex. 3.2 ----------------------------------------

# Dados -----

# packages necessarios
library(datarium)  # contem o conjunto de dados marketing

# detalhes do conjunto de dados
str(marketing)
summary(marketing)
# valores em milhares de dolares


# avaliar relacao entre y e cada uma das covariaveis x
plot(marketing)


# a) -----
modelo <- lm(sales ~ youtube + facebook + newspaper, data=marketing)
modelo  # so mostra os coeficientes
summary(modelo) # mais detalhes sobre o modelo
#sales^= 3.5267+0.0458*youtube + 0.1885*facebook -0.001*newspaper


# b) -----
# H0: beta1=beta2=beta3=0 vs. H1: existe pelo menos um betaj diferente de 0, j=1,2,3
# teste F
summary(modelo)
# fobs= 570.3; p-value<0.001 -> rejeitar H0

# igual a ajustar teste F parcial: H0: modelo nulo (so com intercepto) vs. H1: modelo
# 1o ajustar modelo nulo
modelo0 <- lm(sales ~ 1, data=marketing)
# 2o teste F
anova(modelo0, modelo)
# p-value<0.001 -> rejeitar H0


# c) ----
# H0: betaj=0 vs. H1: betaj!=0, para cada j=0,1,2,3
summary(modelo)
# H0: beta0=0 vs. H1: beta0 diferente 0; tobs=9.422, p-value<0.001 -> rejeitar H0 -> manter no modelo
# H0: beta1=0 vs. H1: beta1 diferente 0; tobs=32.809, p-value<0.001 -> rejeitar H0 -> manter no modelo
# H0: beta2=0 vs. H1: beta2 diferente 0; tobs=21.893, p-value<0.001 -> rejeitar H0 -> manter no modelo
# H0: beta3=0 vs. H1: beta3 diferente 0; tobs=-0.177, p-value=0.86 -> nao rejeitar H0 -> remover do modelo


# d) -----
modelo1 <- lm(sales ~ youtube + facebook, data=marketing)
#ou 
modelo1 <- update(modelo, ~.-newspaper)
summary(modelo1)

# observacao: se fizerem teste F parcial entre modelo e modelo1 <=> teste T para beta3=0
anova(modelo, modelo1)


# e) -----

# estrategia backward com base no teste F: partir do modelo completo
drop1(modelo, test="F")  # -> remover newspaper (nao significativa)
# ajustar modelo sem newspaper
modelo1.back <- update(modelo, ~.-newspaper)
drop1(modelo1.back, test="F") # todas sign. -> STOP
#Modelo obtido (igual a modelo1): sales ~ facebook + youtube


# estrategia forward com base no teste F: partir do modelo nulo e adicionar variaveis uma a uma
(modelo0 <- lm(sales ~ 1, data=marketing))  # modelo nulo, i.e., so com intercepto (=b0)
add1(modelo0, scope=~youtube + facebook + newspaper, test="F") # -> adicionar youtube (a mais significativa)
# ajustar modelo com youtube
modelo1a <- update(modelo0, ~.+youtube)
summary(modelo1a)
# investigar qual a proxima variavel a incluir no modelo
add1(modelo1a, scope=~.+facebook + newspaper, test="F") # -> adicionar facebook (a mais significativa)
# alternativa a add1: 
modelo2a <- update(modelo1a, ~.+facebook)
summary(modelo2a)
anova(modelo2a, modelo1a, test="F")
modelo2b <- update(modelo1a, ~.+newspaper)
summary(modelo2b)
anova(modelo2b, modelo1, test="F")
# fim de alernativa a add1
# investigar qual a proxima variavel a incluir no modelo
add1(modelo2a, scope=~.+ newspaper, test="F") # -> newspaper nao significativa. STOP. 
#Modelo obtido (igual a modelo1): sales ~ facebook + youtube

# estrategia stepwise com base no teste F: neste caso temos poucas variaveis para ilustrar
# 1. modelo nulo
# 2. incluir as 2 primeiras variaveis pelo metodo forward
# 3. usar metodo backward para avaliar a exclusao (e excluir) de alguma variavel
# 4. usar metodo forward para avaliar (e incluir) nova variavel
# 5. repetir passos 3 e 4 até não incluir e excluir variaveis.


# estrategia backward com base no AIC:
step(modelo)
modeloF <- step(modelo, direction="backward")
#Modelo obtido: sales ~ facebook + youtube

# estrategia forward com base no AIC:
modeloF <- step(modelo0, scope=~youtube + facebook + newspaper, direction="forward")
#Modelo obtido: sales ~ facebook + youtube

# estrategia stepwise com base no AIC:
modeloF <- step(modelo0, scope=~youtube + facebook + newspaper, direction="both")
#Modelo obtido: sales ~ facebook + youtube

# conclusao: neste exemplo todas as estrategias resultaram no mesmo modelo



# f) i) -----
confint(modelo1)



# f) ii) -----
(modelo1.estandartizado <- lm(data.frame(scale(modelo1$model))))
confint(modelo1.estandartizado)
# preditor youtube

# alternativa, por ex.
library(parameters)
standardize_parameters(modelo1)


# f) iii) -----
# como a variavel newspaper nao entra no modelo, nao e preciso defini-la para a previsao
novosDados <- data.frame(youtube=100, facebook=50)
predict(modelo1, newdata=novosDados)



# f) iv) -----

# modelo final
modeloF <- lm(sales ~ youtube + facebook, data=marketing)
  
# pressupostos e problemas se violados:
# 1. independencia: se violada -> p-values e ICs incorretos
# 2. residuos normais: se violado -> p-values e ICs incorretos. problema com n pequeno, nao tanto com n grande (pelo menos 10 obs por preditor) 
# 3. linearidade: se violada -> estimativas enviesadas
# 4. homogeneidade dos residuos: se violado -> p-values e ICs incorretos mesmo com n grande

ri <- rstandard(modeloF)  # residuos standard
yi <- fitted(modeloF)     # valores preditos

# definir porque irao ser uteis ais à frente
n <- dim(marketing)[1]               # dimensao da amostra
p <- length(modeloF$coefficients) -1 # numero de preditores do modelo (exclui intercepto)

library(car)
vif(modeloF)  # sem problemas de multicolinearidade

# graficos para avaliar pressupostos
par(mfrow=c(2,2))
plot(modelo1)  
# grafico 1: 
#    parece formar um U -> falha linearidade 
#    nuvem em torno de y=0 -> residuos com media nula: OK
# grafico 2: parece haver problemas nas caudas -> normalidade em causa
    # instrucao alternativa: com bandas de confianca
    par(mfrow=c(1,1))
    library("car")
    qqPlot(ri)   # qqplot com banda de confianca: problemas com normalidade
    hist(ri)     # o histograma tambem é muito útil
# grafico 3: homocedasticidade parece estar ok
# grafico 4: Di < 1 :-)
#            ha pontos influentes (i.e., com hii>0.03 (= 2*(p+1)/n = 2*(2+1)/200))? Nao ha pontos influentes
#             (Nota: ha quem prefira regra menos conservadora: é influente se hii>3*(p+1)/n)

    
# indepedencia
plot(ri)
# o grafico nao apresenta qualquer padrao, curvatura ou alternancia de sinal :-). Verifica pressuposto de independencia´

 

# TESTES FORMAIS para avaliar os pressupostos (para amostras grandes pode nao se o aconselhado)
# 1. avaliar possibilidade de heterocedasticidade
# teste Breusch-Pagan (versao robusta que usa os residuos studentizados): H0: homocedasticidade vs. H1: heterocedasticidade
library(lmtest)
lmtest::bptest(modeloF)  # p=0.0903 -> nao rejeitar homocedasticidade

# 2. avaliar normalidade
# teste Shapiro-Wilk: H0: residuos normais vs. H1: residuos nao normais
shapiro.test(ri)  # p<0.001 -> rej. normalidade

# 3. avaliar independencia dos residuos
# teste Durbin-Watson: H0: residuos indepedentes vs. H1: residuos nao independente
library(car)
durbinWatsonTest(modeloF)  # p=0.536   -> nao rej. residuos independentes



# analise DISTANCIAS DE COOK
plot(modeloF, which=4)  # todas menores que 0.5 :-)



# analise dos LEVERAGE
plot(modeloF, which = 5) # leverage vs residuos + distancia de Cook

(LEVERAGE <- hatvalues(modeloF))    # valores leverage
plot(LEVERAGE, type='h')
maximo <- 2*(p+1)/n
abline(h=maximo, col="red")

# contar casos com hii>maximo
table(LEVERAGE>maximo)  # 1 valor com problemas -> investigar



# analise dos DFBETAS
(DFBETAS <- dfbetas(modeloF))  # Tem tantas colunas quantos os betas no modelo

par(mfrow=c(3,1))
plot(abs(DFBETAS[,1]), type="h", main="intercept")
maximo <- 2/sqrt(n)
abline(h=c(-maximo, maximo), col="green")

plot(abs(DFBETAS[,2]), type='h', main="youtube")
abline(h=c(-maximo, maximo), col="green")

plot(abs(DFBETAS[,3]), type='h', main="facebook")
abline(h=c(-maximo, maximo), col="green")

# contar casos com DFBETASi>maximo
table(abs(DFBETAS[,1]) > maximo)  # sem problemas no beta0
table(abs(DFBETAS[,2]) > maximo)  # sem problemas no beta1
table(abs(DFBETAS[,3]) > maximo)  # sem problemas no beta2



# analise dos DFFITS
(DFFITS <- dffits(modeloF))
plot(abs(DFFITS), type="h")
maximo <- 2*sqrt((p+1)/n)  
abline(h=maximo, col="green")

# contar casos com DFFITSi>maximo
table(abs(DFFITS)>maximo)  # 29 valores com problemas -> investigar



# ALTERNATIVAS PARA OBTER ALGUMAS DAS MEDIDAS ACIMA MAIS FACILMENTE
library(car)
# Graficos com Distancia de Cook, Residuos studentizados, leverage e teste Bonferroni para avaliar se há outliers
influenceIndexPlot(modeloF)  # identifica 131 como outlier

library(stats)
influentes <- influence.measures(modeloF, infl = influence(modeloF)) # The 'infl' argument is not needed, but avoids recomputation
influentes  # mostra as medidas para todos os "individuos" da nossa base de dados
# quais "individuos" sao possiveis observacoes influentes (se destacam em pelo menos 1 medida)?
which(apply(influentes$is.inf, 1, any))   
# 6  36  127 131 179 # posicao dos possiveis influentes 
# para vermos os resultados das medidas apenas nos possiveis "casos" influentes
summary(influentes)  # casos identificados com problemas apenas no dffit



# g) ----

# i) transformar Y e/ou X

# Transformar y?
# podemos experimentar a transformacao logaritimica, que normalmente e util
modelo1a <- lm(log(sales) ~ facebook+youtube, data=marketing) # transformar youtube
summary(modelo1a)  # R2=.7995, R2a=0.7974
par(mfrow=c(2,2))
plot(modelo1a)     # nao normal

# solucao mais geral: transformacao de Box-Cox
# usualmente arredonda-se o resultado gerado para multiplo de 0.5
library(MASS)
lambda <- boxcox(modeloF)                 # sugere lambda prox. 1 -> nao transformar y
lambda$x[which(lambda$y==max(lambda$y))]  # [1] 0.9090909 -> aprox. = 1 -> nao transfomar y



# Transformar x?
# xi vs residuos
plot(marketing$facebook, ri)  # sem padrao
plot(marketing$youtube, ri)   # tem padrao -> transformar youtube. 
# como faz curva voltada para baixo, talvez uma raiz quadrada, para cortar efeito nos extremos...
# alternativa: estes dois graficos tambem podem ser obtidos (e também (y, ri))
car::residualPlots(modelo1, tests=F, quadratic=F, layout=c(1,3))

par(mfrow=c(2,2))

# vamos experimentar transformar X=youtube
modelo2a <- lm(sales ~ facebook+I(sqrt(youtube)), data=marketing) 
summary(modelo2a)  # R2=.929, R2a=0.9282
plot(modelo2a)     # normalidade ok, mas nao linear
# possiveis problemas com 131


# ii) INCLUIR TERMOS/interacoes -----

# incluir interacao
modelo3a <- lm(sales ~ facebook * youtube, data=marketing)
summary(modelo3a)  # R2=0.9678, R2a=0.9673
plot(modelo3a)     # falha normalidade, linearidade, homogeneidade
# se excluirmos 131 e 156, talvez resolva problemas com linearidade e homogeneidade


# por exemplo incluir termo polinomial e tranformar y
modelo3b <- lm(sales ~ facebook + poly(youtube, 2), data=marketing)
summary(modelo3b)  # R2=.917, R2a=0.9154
plot(modelo3b)     # normalidade ok, mas nao linear. 
# possiveis prolemas com 131 e talvez 6


AIC(modeloF, modelo2a, modelo3a, modelo3b) # melhor 3a (interacao), seguido de 1a (raiz quadrada)

# vamos experimentar juntar transformacao raizquadrada com interacao
modelo4 <- lm(sales ~facebook * I(sqrt(youtube)), data=marketing)
summary(modelo4)  # R2=0.9928, R2a=0.9926
plot(modelo4)     # sem 131 e 156, parecem estar ok os pressupostos :-)

# remover casos 131 e 156
modelo4b <- update(modelo4, data=marketing[-c(131,156),])
plot(modelo4b)
# o melhor que se consegui!

# MODELO ESCOLHIDO: modelo4b
modeloFinal <- modelo4b
summary(modeloFinal)



# h) ----
summary(modeloFinal)
#  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                5.4948018  0.1965237  27.960  < 2e-16 ***
#  facebook                  -0.0488459  0.0057478  -8.498 5.06e-15 ***
#  I(sqrt(youtube))           0.4686989  0.0149660  31.318  < 2e-16 ***
#  facebook:I(sqrt(youtube))  0.0195610  0.0004317  45.309  < 2e-16 ***

# modelo: sales^= 5.4948 -0.0488459*facebook + 0.4686989*sqrt(youtube) + 0.0195610*facebook*sqrt(youtube)

# como interpretar? Temos que calcular os EFEITOS MARGINAIS 
# (nota:i.e., as alteracoes de 1 unidade equivalem a derivar sales em ordem a variavel a alterar):


# FIXAR FACEBOOK
# se facebook=c -> sales^= 5.4948 -0.0488459*c + 0.4686989*sqrt(youtube) + 0.0195610*c*sqrt(youtube)
# Se aumentar 1 unidade em sqrt(youtube): sales^ altera 0.4686989 + 0.0195610*c (=derivada de sales em ordem a sqrt(youtube), i.e., efeito marginal de sqrt(youtube))
#   se c = 0 (i.e., nao ha investimento em facebook), entao por cada aumento de 1 milhar de dolares em publicidade sqrt(youtube) espera-se que as vendas aumentem 0.4686989 milhares de dolares
#   quanto maior for o investimento em facebook, maior será o impacto do investimento de sqrt(youtube) nas vendas. 

# graficamente
library(ggplot2)
summary(marketing$facebook)  # valores variam entre 0 e aprox. 60
summary(marketing$youtube)   # valores variam entre aprox. 0 e aprox. 350

# criar data.fame com valores para os preditores
valores_facebook <- c(0, 27.92, 60)
valores_youtube <- seq(0, 350, 10)  # criar sequencia de valores entre 0 e 350, espaçados em 10 unidades

# expand.grid: permite criar data.frame com a combinacao de todos os valores de valores_facebook com os de valores_youtube
novosdados <- expand.grid(youtube = valores_youtube, facebook = valores_facebook)

# obter vendas estimadas com base no modelo ajustado com os valores de facebbok e youtube definidos, e respetivos ICs
previsoes <- predict(modeloFinal, newdata = novos, interval = "confidence")
head(previsoes)

# adicionar previsoes ao data.frame novos
novosdados$sales_prev  <- previsoes[,"fit"]
novosdados$sales_ICinf <- previsoes[,"lwr"]
novosdados$sales_ICsup <- previsoes[,"upr"]
head(novosdados)

library(ggplot2)
ggplot(novosdados, aes(x=youtube, 
                       y=sales_prev, 
                       #color=facebook))+          # ERRO
                       color=factor(facebook)))+
  geom_line()+
  geom_ribbon(aes(ymin=sales_ICinf,
                  ymax=sales_ICsup,
                  fill=factor(facebook)),
              alpha=0.2)+
  labs(title="Valores em milhares de dolares",
       x = "Despesa com youtube",
       y = "Vendas previstas",
       color = "Despesa com facebook",
       fill = "Despesa com facebook")+
  theme_bw()


# FIXAR YOUTUBE: TPC
