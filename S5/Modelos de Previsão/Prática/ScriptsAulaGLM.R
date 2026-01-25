# Miguel Grilo
# ----------------------------------------------------
###### SCRIPTS DE AULA - GLM
# ----------------------------------------------------
# ScriptAirPassengers2
library(forecast)

# Monthly Airline Passenger Numbers 1949-1960
data(AirPassengers)
class(AirPassengers)

library(TSstudio)
ts_info(AirPassengers)
ts_plot(AirPassengers)
ts_plot(AirPassengers,
        title="Número de passageiros aéreos entre 1949 e 1960",
        Ytitle="Número mensal de passageiros",
        Xtitle="Fonte: RStudio",
        slider=TRUE)

ts_heatmap(AirPassengers)

ts_seasonal(AirPassengers, type="all")
# Cores escuras para os primeiros anos e claras para os últimos

ts_cor(AirPassengers, lag.max=40)

ts_lags(AirPassengers)
# Correlação dos lags

ts_lags(AirPassengers, lags=c(12,24,36))

ts_polar(AirPassengers)

### Modelos de Decomposição Clássica
## Usar quando queremos estudar apenas a tendência ou apenas a sazonalidade
# Decomposição aditiva
dec.air.a <- decompose(AirPassengers, type="additive")
plot(dec.air.a$seasonal)
# Versão dos dados ajustada sazonalmente (apenas a componente sazonal)
plot(dec.air.a)
# Mostra todas as componentes do modelo!
plot(dec.air.a$seasonal) # Mostra apenas uma das componentes
plot(dec.air.a$trend)

# Versão dos dados ajustada sazonalmente (versão dos dados apenas com tendência+erros)
seasadj.a<- AirPassengers - dec.air.a$seasonal
plot(seasadj.a) # Apenas o erro e a tendência

library(forecast)
checkresiduals(dec.air.a$random)
# Não são ruído branco obviamente
# Contudo, o checkresiduals não têm objetivo aqui
# Serve apenas para mostrar a separação das três componentes

plot(AirPassengers)
lines(seasadj.a, col="red")

# Decomposição Multiplicativa
# Porque o modelo multiplicativo combina melhor para a série em questão
dec.air.m<- decompose(AirPassengers, type = "multiplicative")
plot(dec.air.m)
checkresiduals(dec.air.m$random)
# Novamente, obviamente não são ruído branco
# E, novamente, o checkresiduals não têm objetivo aqui.

seasadj.m<- AirPassengers - dec.air.m$seasonal
plot(seasadj.m)

plot(AirPassengers - dec.air.m$trend)

# Com a library TSstudio
ts_decompose(AirPassengers, type = "both")

### Modelo de Decomposição Clássica Robusta
# Com a library stats (robust=T para usar o Loess)
library(dplyr)

# Decomposição aditiva
dec.stl.a <- stl(AirPassengers, t.window=13, s.window="periodic", robust=TRUE)
# t.window = 13 -> Período + 1
# Por sugestão geral o t.window (janela da tendência) deve ser o período+1
# s.window é a janela do período/sazonalidade
plot(dec.stl.a)

# Tirar as componentes:
plot(dec.stl.a$time.series[,2]) # Tendência
plot(dec.stl.a$time.series[,1]) # Sazonalidade
plot(dec.stl.a$time.series[,3]) # Erro

checkresiduals(dec.stl.a$time.series[,3])
# Obviamente não são ruído branco, de novo

# Criar um gráfico mais bonito:
AirPassengers %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()
AirPassengers %>%
  stl(t.window=13, s.window=5, robust=TRUE) %>%
  autoplot()

# Decomposição multiplicativa
dec.stl.m<-stl(log(AirPassengers), t.window=13, s.window="periodic", robust=TRUE)
plot(dec.stl.m)

# Transformar as componentes:
trend<- exp(dec.stl.m$time.series[, "trend"])
seasonal<- exp(dec.stl.m$time.series[, "seasonal"])
remainder<- exp(dec.stl.m$time.series[, "remainder"])

plot(trend)
plot(seasonal)
plot(remainder)

library(tsibble)
library(feasts) # Feature extraction and statistics for time series

# Com a library feasts

air.tsibble<- as_tsibble(AirPassengers)
class(air.tsibble)

dec.air.m.rob<-air.tsibble %>%
  model(STL(log(AirPassengers) ~ trend(window=13) + season(window="periodic"),
            robust=TRUE)) %>%
  components()

autoplot(dec.air.m.rob)

dec.air.m.rob2<- dec.air.m.rob %>%
  mutate(
    trend = exp(dec.air.m.rob$trend),
    season = exp(dec.air.m.rob$season_year),
    remainder = exp(dec.air.m.rob$remainder)
  )

autoplot(dec.air.m.rob2)

### Modelos do tipo TSLM: Modelos Lineares por Partes
### Permitem captar tendências não lineares dividindo os dados
### Em segmentos e ajustando um modelo de regressão linear separado
### Para cada segmento. Esta abordagem permite captar tendências médias.

## Treino e Teste
treino<-window(AirPassengers, end=c(1958,12))
teste<-window(AirPassengers, start=c(1959,1))

model.tslm <- tslm(treino ~ trend + season)
model.tslm

check_res(model.tslm)
# Não são ruído branco e não seguem uma distribuição aproximadamente normal
# Ou seja, podemos ajustar um modelo para os resíduos apenas
# Já que já extraimos a tendência e a sazonalidade
# Para juntar previsões, juntamos as três componentes

model.tslm.prev<-forecast(model.tslm, h = length(teste))
plot(model.tslm.prev)
accuracy(model.tslm.prev, teste)

plot(model.tslm.prev, ylim=c(80,610), main="TSLM")
lines(fitted(model.tslm), col="red")
lines(teste)

### Modelos do Tipo ETS - Error, Trend, Seasonality model
### Erro (E), Tendência (T) e Sazonalidade (S): que podem ser:
### Aditivos ou multiplicativos, resultando, assim, em inúmeros
### Modelos Possíveis.

model.ets <- ets(treino)

checkresiduals(model.ets$res)
t.test(model.ets$res)
# Os resíduos são ruído branco

model.ets.prev<-forecast(model.ets, h = length(teste))
plot(model.ets.prev)
accuracy(model.ets.prev, teste)

plot(model.ets.prev, ylim=c(80,610), main="ETS Automáticamente preenchido")
lines(teste, col="black")
legend("topleft", legend=c("Previsões", "Dados"), col= c("blue", "black"), lty=1, cex=0.8)
# Previsão péssima, mas com bandas de confiança altíssimas.

model.ets2 <- ets(treino, opt.crit = "sigma")

checkresiduals(model.ets2$res)
t.test(model.ets2$res)
# Os resíduos são ruído branco

model.ets.prev2<-forecast(model.ets2, h = length(teste))
plot(model.ets.prev)
accuracy(model.ets.prev2, teste)

plot(model.ets.prev2, ylim=c(80,610), main="ETS Automáticamente preenchido otimizado")
lines(teste, col="black")
legend("topleft", legend=c("Previsões", "Dados"), col= c("blue", "black"), lty=1, cex=0.8)
# Mesmo com a otimização a previsão continua horrível


### Modelos STL com Previsão (ETS ou ARIMA)

plot(stlf(treino, lambda=BoxCox.lambda(treino)))
plot(stlf(treino, method="arima", lambda=BoxCox.lambda(treino)))
model.stlf1 <- stlf(treino, lambda=BoxCox.lambda(treino))
model.stlf2 <- stlf(treino, method="arima", lambda=BoxCox.lambda(treino))

checkresiduals(model.stlf1)
checkresiduals(model.stlf2)

plot(stlf(treino, lambda=BoxCox.lambda(treino)))
lines(forecast(model.stlf2, h=24)$mean, col="red")
lines(teste)
### Melhor modelo SARIMA encontrado (ScriptAirPassengers.R)
fit.treino5<-Arima(treino, order=c(0,1,1),
                   seas=list(order=c(0,1,1),period=12),
                   lambda=BoxCox.lambda(treino, lower=-2, upper=2))

plot(stlf(treino, lambda=BoxCox.lambda(treino)))
lines(forecast(model.stlf2, h=24)$mean, col="red")
lines(forecast(fit.treino5, h=24)$mean, col="blue")
lines(teste)





# ScriptHmedias2
library(stats)
library(nortest)
library(forecast)
library(imputeTS)
library(randtests)
library(TSstudio)
library(dplyr)
library(tsibble)
library(feasts)

dados <- read.csv("C://UNI-L58387-IACD//S5 - MP//Prática//BasesDados//Hmedias.csv", header = T, dec=",", sep=";")
summary(dados)
str(dados)
class(dados)
dados.ts <- ts(dados, start=c(1984,11), frequency=12)
# Já sabemos, por estudo prévio (ScriptHmedias) que existem NA's
# Preencher os NA's
dados.ts <- na_interpolation(dados.ts, option = "spline")
ggplot_na_distribution(dados.ts, title="NA")
class(dados.ts)
# Novamente, pelo estudo prévio (ScriptHmedias) sabemos que existem valores anômalos
# Substituir os valores anômalos:
# Substituir os valores problemáticos: Jan-85 (3), Fev-86 (16) & Nov-87 (37)
p = 12
dados.ts[3] = mean(dados.ts[3+p], dados.ts[3+2*p], dados.ts[3+3*p])
dados.ts[16] = mean(dados.ts[16-p], dados.ts[16+p], dados.ts[16+2*p])
dados.ts[37] = mean(dados.ts[37-3*p], dados.ts[37-2*p], dados.ts[37-p], dados.ts[37+p])
dados.ts[3]
dados.ts[16]
dados.ts[37]

# Verificar a estacionariedade
tsdisplay(dados.ts)
# Parece haver sazonalidade mas não tendência
ndiffs(dados.ts) # 0
nsdiffs(dados.ts) # 1
# Existe sazonalidade mas não tendência

# Divisão treino-teste
treino<-window(dados.ts, end=c(1987,11))
teste<-window(dados.ts, start=c(1987,12))

# Garantir que tendência e sazonalidade não alteram
ndiffs(treino)
nsdiffs(treino)
# Não alteram

##########################################################################
## Modelo automático (testar algo novo que a professora mostrou)
## Não usamos tanto porque o modelo costuma ser péssimo
modelteste <- auto.arima(treino, stationary = "FALSE") # library(forecast)
modelteste # Faz apenas uma diferenciação sazonal
# Este é, portanto, um dos casos em que auto.arima não faz nada
# Contudo, existem casos em que auto.arima ajuda
plot(forecast(modelteste, h=12))
lines(fitted(modelteste), col="red")
lines(teste)
#########################################################################

# Como não existe tendência, comecemos por um modelo TSLM
# Não tentarei (por ora) decomposição clássica porque já temos apenas sazonalidade
# Então dividir apenas a componente sazonal e o erro não parece fazer tanto sentido
model.tslm <- tslm(treino ~ season)
model.tslm

checkresiduals(model.tslm$residuals) # Valor p = 0.7642
t.test(model.tslm$residuals) # Valor p = 1
# Os resíduos são ruído branco

model.tslm.prev<-forecast(model.tslm, h = length(teste))
plot(model.tslm.prev)
accuracy(model.tslm.prev, teste)
# Gráfico da modelagem
plot(model.tslm.prev, ylim=c(0.5, 4.5), main="TSLM")
lines(fitted(model.tslm), col="red")
lines(teste)
# Segue até que bem, embora preveja mal aquele pico no inicio de 1988

# Tentemos agora um modelo ETS
model.ets <- ets(treino)
checkresiduals(model.ets$res) # Valor p = 0.8774
t.test(model.ets$res) # Valor p = 0.8749
# Portanto os resíduos são ruído branco

model.ets.prev<-forecast(model.ets, h = length(teste))
plot(model.ets.prev)
accuracy(model.ets.prev, teste)
plot(model.ets.prev, ylim=c(0.5,4.5), main="ETS Automáticamente preenchido")
lines(teste, col="black")
legend("topleft", legend=c("Previsões", "Dados"), col= c("blue", "black"), lty=1, cex=0.8)
# Novamente, previsão boa salvo no início de 1988

model.ets2 <- ets(treino, opt.crit = "sigma")
checkresiduals(model.ets2$res) # Valor p = 0.8711
t.test(model.ets2$res) # Valor p = 0.5066
# Os resíduos são ruído branco

model.ets.prev2<-forecast(model.ets2, h = length(teste))
plot(model.ets.prev)
accuracy(model.ets.prev2, teste)
plot(model.ets.prev2, ylim=c(0.5,4.5), main="ETS Automáticamente preenchido otimizado")
lines(teste, col="black")
legend("topleft", legend=c("Previsões", "Dados"), col= c("blue", "black"), lty=1, cex=0.8)
# A otimização não ajudou.

# Procuremos, então, ajustar um modelo STL com Previsão (ETS ou ARIMA)
plot(stlf(treino, lambda=BoxCox.lambda(treino))) # Intervalos de confiança negativos
# O ETS não é possível
plot(stlf(treino, method="arima"))
model.stlf2 <- stlf(treino, method="arima", h=12)

checkresiduals(model.stlf2) # Valor p = 0.7912
t.test(model.stlf2$res) # Valor p = 1
# Os resíduos são ruído branco nos dois modelos

plot(forecast(model.stlf2))
lines(teste)
accuracy(model.stlf2, teste)





# ScriptPremios
premios<-read.table("C://UNI-L58387-IACD//S5 - MP//Prática//BasesDados//Premios.csv", header=T, sep=";", dec=".")

summary(premios)
str(premios)
# Variável resposta: num_awards
# math - variável preditora contínua
# prog - variável preditora categórica com 3 categorias (Academic, General, Vocational)


table(premios$prog)
premios$prog <- as.factor(premios$prog) # Tornar prog em factor, pois está em character
# Categoria de referência = Académico

(mu<-mean(premios$num_awards))
(var<-var(premios$num_awards))

var/mu
# Diferença demasiado alta entre mean e var, não parece que Poisson faça sentido
# Uma vez que a diferença entre a média e a variância é de 1.76
# Superior ao intervalo da regra de polegar entre 0.5 e 1.5

plot(seq(0,6) - 0.05, prop.table(table(premios$num_awards)), type="h",
     lwd=2, ylab="relative frequency", ylim=c(0,1),
     xlab="number of awards",
     main="Observados VS Poisson")
lines(seq(0,6) + 0.05, dpois(seq(0, 6), mu), col="red", type="h", lwd=2)
legend("topright", c("Observed", "Poisson"), col=c("black", "red"), lty=1, lwd=2)


premios$prog <- relevel(premios$prog, ref="General")

# Faremos a Poisson à mesma por contexto de aula
modelo1 <- glm(num_awards ~ prog + math, family=poisson, data=premios)
summary(modelo1)
# A única variável com valor p superior a alfa = 0.05 é uma categoria de prog
# Então não removemos, porque a outra categoria do fator é significativa
# Residual deviance próxima dos graus de liberdade
# Temos um indício que talvez estejamos próximos de um bom modelo

beta<-coefficients((modelo1))
exp(beta)
# Interpretar o modelo APENAS QUANDO os resíduos forem analisados
# Se o modelo estiver correto então interpretamos

# Por questões académicas, a interpretação será feita agora:
# Quem frequenta programa académico tem cerca de 3x mais chances
# De conseguir troféus do que quem frequenta programa geral
# Quem frequenta programa vocacional tem cerca de 1.4x mais chances
# De conseguir troféus do que quem frequenta programa geral
# Por cada ponto a mais na nota de matemática o aluno aumenta em cerca de 1.07x
# As chances de vir a conseguir troféus

# Math para 10 pontos:
exp(beta[4]*10)
# Quando comparamos uma variável as outras estão fixas

# Variação percentual do programa académico em comparação com o programa geral
(acad.percent.change<-((exp(beta[1] + beta[2]))/exp(beta[1]))*100-100)

library(DHARMa)

# Simula resíduos com base no modelo
resid<-simulateResiduals(modelo1)
plot(resid)
# Valor p normalidade = 0.76761 > 0.05, normal
# Dispersion p = 0.24 > 0.05
# Outlier test p = 1 > 0.05
# Nada a vermelho no residual vs predicted, tudo bem

# Avaliar a sobredisposição
library(AER)
dispersiontest(modelo1)
# Valor p = 0.2973 > 0.05
# Então o modelo Poisson está adequado





# ScriptAffairs
library(AER)
data(Affairs)
# co-variáveis: género, idade, nº de anos de casamento,
# filhos (sim ou não), religiosidade (varia de 1, anti-religiosidade, a 5, muito),
# escolaridade, ocupação (código da ocupação),
# e avaliação (é uma autoavaliaçao do casamento,
# varia entre 1 (muito infeliz) a 5 (muito feliz)).

### Poisson de inicio (média == var)
### Quasipoisson quando existem problemas de sobredispersão (admite sobredispersão)
### Binomial Negativa quando tudo falha (mais robusta)

### Indícios de um modelo bem ajustado -> Residual deviance próxima dos degrees of freedom

str(Affairs)

View(Affairs)

# Recodificar as variáveis para que faça mais sentido:
# age: <20, 20-24, 25-29,...

Affairs$religiousness <- as.factor(Affairs$religiousness)
Affairs$rating <- as.factor(Affairs$rating)

(mean<-mean(Affairs$affairs))
(var<-var(Affairs$affairs))
var/mean
# Obviamente não pode ser usada a Poisson
# A variância está altíssima, outliers podem provocar essa variância enorme
# Devemos, portanto, analisar a existência de outliers depois do ajuste

# Opções de modelos quando a Poisson falha:
# quasipoisson
# Binomial Negativa

# Ponto para selecionar todas as variáveis, -occupation para tirar occupation
modelo1 <- glm(affairs ~ . -occupation, data=Affairs, family="poisson")
summary(modelo1)
# Variáveis não significativas:
# children
# education
# rating2 (uma categoria entre outras que são significativas)

# Começamos por remover a variável de valor p mais alto
modelo2 <- glm(affairs ~ . -occupation -education, data=Affairs, family="poisson")
summary(modelo2)

# Agora as únicas variáveis não significativas são fatores
# Dito isso ainda existe uma enorme discrepância entre a deviance e os DoF
# Portanto, temos indícios que haverão problemas com os resíduos

library(DHARMa)
# Análise de resíduos
resid<-simulateResiduals(modelo2)
plot(resid)
# Problemas severos:
# Normalidade obviamente rejeitada (p = 0), dispersão também (p = 0), 
# existem outliers (p = 0)
# Curvas do residual vs predicted significativas

# Avaliar a sobredisposição
# library(AER) <- já temos a biblioteca
dispersiontest(modelo2)
# Valor p < 0.001, existem problemas com a dispersão
# Obviamente o modelo Poisson não é adequado
# Existem outliers, devemos verificar quais são para os limpar

boxplot(modelo2$residuals)
# Temos vários outliers, sobretudo um altamente problemático

# Modelo Quasipoisson
modelo3 <- glm(affairs ~ . - occupation, data = Affairs, family="quasipoisson")
summary(modelo3)

# Remover education
modelo4 <- glm(affairs ~ . -occupation - education, data=Affairs, family="quasipoisson")
summary(modelo4)

# Remover age
modelo5 <- glm(affairs ~ . -occupation -education -age,
               data = Affairs, family="quasipoisson")
summary(modelo5)
# Como o age estava próximo de 5% estudemos os modelos 4 e 5 (com e sem age)

# Validar os modelos

par(mfrow = c(2,3))
plot(modelo5, which = 1:6)

dev.off()

# Distância de Cook: Valores que influenciam
(n<-nrow(Affairs))
(threshold<-4/n)

val.cook<-cooks.distance(modelo5)

# Conta quantos ultrapassam o limite
sum(val.cook > threshold) # 42 valores com distância de cook acima do threshold

# Outra opção
threshold.mean<-3*mean(val.cook)
sum(val.cook > threshold.mean) # 55 valores com distância de cook acima do threshold

### Ou seja, qualquer uma das opções dá problemas enormes

# Remover todos os outliers
Affairs2 <- Affairs[ - which.max(round(cooks.distance(modelo5))), ]

modelo6 <- glm(affairs ~ gender + yearsmarried + children + religiousness + rating,
               data = Affairs2, family = "quasipoisson")
summary(modelo6)
par(mfrow=c(2,3))
plot(modelo6, which=1:6)
dev.off()

val.cook2<-cooks.distance(modelo6)
threshold.mean2<-3*mean(val.cook2)
sum(val.cook2 > threshold.mean2)

modelo7 <- glm(affairs ~ gender + yearsmarried + children + religiousness + rating,
               data = Affairs2, family = "quasipoisson", maxit=100)
summary(modelo7)
par(mfrow=c(2,3))
plot(modelo7, which=1:6)
dev.off()

val.cook3<-cooks.distance(modelo7)
threshold.mean3<-3*mean(val.cook3)
sum(val.cook3 > threshold.mean3)

modelo8 <- glm(affairs ~ gender + yearsmarried + children + religiousness + rating,
               data = Affairs2, family = "quasipoisson", maxit=20)
summary(modelo8)
par(mfrow=c(2,3))
plot(modelo8, which=1:6)
dev.off()
# Problemas severos independentemente do que façamos

# Cálculo da deviance explicada pelo modelo == coef de determinação
(dev.expl<-(modelo7$null.deviance - modelo7$deviance)/modelo7$null.deviance)
# Apenas 20% da deviance é explicada pelo modelo, o modelo é péssimo


### Binomial Negativa´
library(MASS)
modelo9 <- glm.nb(affairs ~ gender + age + yearsmarried + children + 
                    religiousness + education + rating, data = Affairs)
summary(modelo9)

# Removemos age
modelo10 <- glm.nb(affairs ~ gender + yearsmarried + children + 
                     religiousness + education + rating, data = Affairs)
summary(modelo10)

# Removemos education
modelo11 <- glm.nb(affairs ~ gender + yearsmarried + children + 
                     religiousness + rating, data = Affairs)
summary(modelo11)
# E não removemos mais nada
# Validar resíduos
par(mfrow=c(2,3))
plot(modelo11, which=1:6)
dev.off()

# Se o modelo estivesse bom fariamos e interpretavamos
(est<-cbind(Estimate = coef(modelo11), confint(modelo11)))
exp(est)


# Zero-Inflated Poisson Regression is used to model count data that has an excess of zero counts.
# Esta base de dados é uma base de dados que precisaria disto
# Contudo, isto não é algo da cadeira. É importante saber que existe, contudo
# E estudar em casa para compreender sobre





# ScriptParasitoides
library(dplyr)
url("https://www.dropbox.com/scl/fi/2ekglfs55ccow0u9pekcu/HawaiiHymentopteraSites.rds?rlkey=149gl3gqxahf235dvk9cv8qbt&dl=1") %>%
  readRDS() -> hh.sites

names(hh.sites)

# Vegetação natural e pastagem (LandUse)
# Site_number: O local onde as espécies foram amostradas
# Predominant_land_use: o uso da terra no local amostrado
# ForestCover: a percentagem de cobertura florestal no local amostrado
# Species_richness: a riqueza de espécies registada no local

hh.sites2<- hh.sites %>% mutate(LandUse = recode(Predominant_land_use,
                                                 'Primary vegetation' = 'Primary',
                                                 'Pasture' = 'Pasture')) %>%
  select(LandUse, ForestCover, Species_richness, Site_number)

str(hh.sites2)


# Modelo Poisson

mean(hh.sites2$Species_richness)
var(hh.sites2$Species_richness)

var(hh.sites2$Species_richness)/mean(hh.sites2$Species_richness)
# Poisson obriga que a média seja (teoricamente) igual à variância
# Fica de nosso critério se a razão é próxima o suficiente de 1 para assumir igualdade
# Regra do polegar - Entre 0.5 e 1.5 podemos avançar com a Poisson
# Ajustamos o modelo e se os resíduos tiverem problemas ai sim é que ajustamos outro modelo
# Portanto, por ora avançamos com o modelo

modelo1 <- glm(Species_richness ~ LandUse + ForestCover + Site_number, family=poisson, data=hh.sites2)
summary(modelo1)
# Considerando alfa = 5%, retiramos todos os que estão acima de 5%
# Portanto, removemos apenas o site_number
# Vale-se ressaltar que esta regra não é fixa, e fica uma espécie de regra do polegar
# Por exemplo, se a variável a ser removida for significativa o suficiente ou importante

modelo2 <- glm(Species_richness ~ LandUse + ForestCover, family=poisson,
               data=hh.sites2) 
summary(modelo2)
# Agora nenhuma das variáveis está acima de 5%
# Uma forma de ver se o modelo está mais ou menos bom é
# Se a deviance dos resíduos for próxima dos degrees of freedom
# Não é regra específica, é apenas empírico, mas ainda assim dá suspeita

beta <- exp(modelo2$coefficients)
beta

coef.const <- beta[1]
coef.past <- beta[2]
coef.forest <- beta[3]

rich.pasture <- coef.const * coef.past
rich.pasture
# Para interpretação do modelo

# Variação percentual da riqueza de espécies nas pastagens comparada com vegetação primária
pasture.percent.change <- ((rich.pasture/coef.const)*100)-100
pasture.percent.change
# A riqueza das espécies diminui 78% na pastagem relativamente à vegetação primária


# Deviance nula e residual
(dev.null <- modelo2$null.deviance)
(dev.resid <- modelo2$deviance)

# Cálculo da deviance explicada pelo modelo == ao coef de determinação
(dev.expl <- (dev.null - dev.resid)/dev.null)
# O nosso modelo explica 28.77% da variabilidade da variável resposta

# Validação do modelo
library(DHARMa)

# Simula resíduos com base no modelo
resid <- simulateResiduals(modelo2)
testDispersion(resid) 

# Compara os resíduos do modelo com os esperados
plot(resid)
# Valor p do KS test = 0.16847 > 0.05, os resíduos são normais
# Valor p do teste de dispersão = 0.88 > 0.05, não há dispersão nos resíduos
# Valor p dos outliers = 0.36 > 0.05, não existem outliers
# DHARMa test: Parece estar tudo bem também

# Avaliar a sobredispersão
library(AER)
# Avaliar se a variância é diferente da média. Se falhar a Poisson não serve
# E temos que usar uma binomial negativa
dispersiontest(modelo2)
# Valor p = 0.13 > 0.05, não se rejeita H0
# Portanto, o modelo Poisson está bem adequado

# Previsões
prevs <- predict(object = modelo2, se.fit = TRUE)
head(prevs)

prevs$fit <- exp(prevs$fit)

prevs$LI <- exp(prevs$fit - 1.96*prevs$se.fit)
prevs$LS <- exp(prevs$fit + 1.96*prevs$se.fit)

View(prevs)
# Falta deixar as previsões em gráfico