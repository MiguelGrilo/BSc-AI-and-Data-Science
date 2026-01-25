# Miguel Grilo
# ----------------------------------------------------
###### SCRIPTS DE AULA - SÉRIES TEMPORAIS
# ----------------------------------------------------
# ScriptExe1

### Ler a Base de Dados

dados <- read.csv("C:\\UNI-L58387-IACD\\S5 - MP\\Prática\\BasesDados\\Exe1.csv",
                  fileEncoding = "utf-8", 
                  stringsAsFactors = T, 
                  na.strings = "")
summary(dados)
head(dados)
class(dados)

dados2<-dados+25
summary(dados2)

# Caso venhamos a trabalhar com a dimensão dos dados
n<-dim(dados)[1]
n

### Passo Nº1
plot.ts(dados)

# Transformação do formato dos dados em série temporal
dados.ts <- ts(dados, start=c(1900,1), frequency=12)
# Formato: ts(dados, start=c(ano,mês), frequency=como é que os dados são recolhidos)
# 12 - mensalmente
dados.ts2 <- ts(dados2, start=c(1900,1), frequency=12)
class(dados.ts)
# ts - temporal series

length(dados.ts)
start(dados.ts)
end(dados.ts)
# start e end - início e fim da série temporal

# Com o plot normal e dados no formato ts, o plot normal já serve para representação
plot(dados.ts, main="ST", ylab="Altura das ondas")

### Estatística Descritiva
library(fBasics)
basicStats(dados.ts)

(med<-mean(dados.ts))

# Ou
library(forecast)

tsdisplay(dados.ts)
# A olho não parece haver missings, parece ser estacionária do tipo AR(1)

library(tseries)
adf.test(dados.ts)
# O adf test verifica se a série é estacionária ou não
# Devemos verificar a hipótese alternativa porque o H0 e H1 podem estar trocados
# Neste caso, como p value = 0.01 e H1 = estacionária, com alfa = 0.05
# Rejeitamos H0, pelo que a série é estacionária
# Deve-se ter cuidado com este teste porque este teste só faz a estacionaridade
# Baseado em uma autocorrelação de ordem 1
# Se tivemos autocorrelações mais fortes o teste pode não as apanhar
# Ou seja, o teste é muito limitado

# Outro teste possível, na biblioteca forecast:
ndiffs(dados.ts, method="adf")
# Se dizer 0, o número de diffs é 0, logo a série é estacionária
# Se dizer diferente de 0, a série não é estacionária

# Verificar se tem dados omissos ou não
library(imputeTS)
ggplot_na_distribution(dados.ts, title="AA")
# Se houvesse algum missing o gráfico mostraria o valor em um ponto de cor diferente
statsNA(dados.ts)
# Não existem dados omissos

# Mudar a série ao adicionar 25 a todos os valores não muda nada na leitura
# Uma vez que os valores continuam constantes
tsdisplay(dados.ts2)

# Testar a normalidade dos dados
library(nortest)
lillie.test(dados.ts)
shapiro.test(dados.ts)
# valor p = 0.2961 > 0.05
# Valor p = 0.6673 > 0.05, logo não rejeitamos H0, portanto os dados são normais

hist(dados.ts)
# Formato de uma distribuição normal
boxplot(dados.ts)
# 3 outliers, ou seja 3 valores não entram dentro da Normal.

### Estimar os Parâmetros
# library(forecast)
# Já importado antes

fit1 <- Arima(dados.ts, order=c(1,0,0), include.mean=TRUE, method="ML")
# ML - Máxima-Verossimilhança (usado por defeito)
# CSS - Mínimos Quadrados (conditional sum of squares)
# method = c("CSS-ML", "ML", "CSS")
# order - (p, d, q), order 1,0,0 = AR(1)
fit1
confint(fit1)
# Obter a constante do modelo
const<-mean(dados.ts)*(1-coef(fit1)[1])
const

fit2 <- Arima(dados.ts2, order=c(1,0,0), include.mean=TRUE, method="ML")
fit2
confint(fit2)
coef(fit2)
const<-mean(dados.ts2)*(1-coef(fit2)[1])
const

### Significância dos parâmetros

# Como não temos estatística de teste, calculamos os intervalos de confiança
# Se o intervalo de confiança contiver o 0, o coeficiente pode ser 0
# Se não contiver o 0, rejeitamos H0 pelo que o coeficiente é diferente de 0
confint(fit1)
# Rejeita-se H0 para ar1, portanto temos modelo
# Apesar de dizer intercept, refere-se na realidade à média (por algum motivo)
# Dito isso, a média pode ser 0, e a média só é 0 quando a constante é 0
# Porque mean = constante / (1 - ar), portanto a constante é 0
# Então não se rejeita H0 para a constante, e retiramo-la do modelo
fit2 <- Arima(dados.ts, order=c(1,0,0), include.constant=FALSE, method="ML")
fit2

# Sempre que estimamos o novo parâmetro devemos ver a significância de novo
confint(fit2)
# Rejeita-se H0 para ar1, então acaba aqui.

### Análise dos Resíduos

residuals(fit2) # Para ver todo o modelo

tsdisplay(fit2$residuals) # Para ver apenas os residuos em gráfico
# Pelo gráfico, podemos ver que é ruído branco porque a série é estacionária
# E porque para a FAC e para a FACP os valores estão todos dentro das bandas
# Ou seja, estão dentro dos intervalos de confiança centrados em 0
# Se um valor muito avançado no gráfico estiver de fora, continuamos com ruído branco
# O problema é se estiver fora das bandas de confiança em lag 1 ou lag 2

checkresiduals(fit2)
# Os resíduos seguem uma distribuição aproximadamente normal
# Valor p = 0.6331 > 0.05, não rejeitamos H0
# Logo podemos admitir que os resíduos são não-correlacionados
t.test(fit2$residuals)
# Valor p = 0.5225 > 0.05, não rejeitamos H0
# Logo podemos admitir que os resíduos têm média nula
# Então os resíduos são ruído branco





# ------------------------------------------------------------------------------
# ScriptGold
library(forecast)
data(gold)

plot(gold)
class(gold)

library(imputeTS)
ggplot_na_distribution(gold, title="Distribuição de Missings")
ggplot_na_distribution2(gold, title="Distribuição de Missings")
# Verificamos a existência de missings graficamente.
statsNA(gold)
summary(is.na(gold))
# Contamos portanto 34 NAs.

gold2 <- na_kalman(gold, model = "auto.arima", smooth = TRUE)
ggplot_na_distribution(gold2, title="AA")
# Portanto, não temos mais missings.
# Outro método possível:

gold3 <- na_interpolation(gold, option = "linear")
ggplot_na_distribution(gold3, title="AA")
# Novamente, não temos mais missings.

gold4 <- na_interpolation(gold, option = "spline")
ggplot_na_distribution(gold4, title="AA")
# Novamente, não temos mais missings.

statsNA(gold4)
summary(is.na(gold4))
# Confirmamos portanto que já não existem NAs.
# Avançamos com a data corrigida a partir da na_interpolation com opção spline.
class(gold4)

plot(gold4, col="red")
lines(gold, col="blue")

# Identificar outliers
tsoutliers(gold4)
# Verificamos a existência de um outlier na posição 770
# Recomenda trocar o seu valor por 494.9

gold5<- tsclean(gold4)
plot(gold4)
lines(gold5, col="red")





# ------------------------------------------------------------------------------
# ScriptMortesUK

dados <- read.csv("C:\\UNI-L58387-IACD\\S5 - MP\\Prática\\BasesDados\\MortesUK.csv", 
                  header = F, dec=",", sep=";")
summary(dados)
str(dados)
# Mortes por acidentes rodoviários nos UK entre Jan 1973 & Dez 1981

dados.ts <- ts(dados, start=c(1973,1), frequency=12)
end(dados.ts)

plot(dados.ts, main="mortes por acidentes rodoviários no UK", xlab="Tempo", ylab="freq")

# Verificar a existência de missings:
library(imputeTS)
ggplot_na_distribution(dados.ts, title="AA")
# Pela observação do gráfico a série não aparenta ter missings 

# Verificar a estacionaridade da série
library(forecast)
tsdisplay(dados.ts) # teste KPSS (por defeito) OU PP
# A FAC demora para convergir para 0, então a série não parece ser estacionária
# Verificamos no gráfico superior que os dados tem tendência

ndiffs(dados.ts)
# diferente de 0, logo temos confirmação que a série não é estacionária

# Verificar a sazonalidade da série
nsdiffs(dados.ts)
# Igual a 0, logo a série não é sazonal
# Portanto a nossa série só tem tendência
# Estamos, por isso, perante um modelo ARIMA

# Passo 1: Transformação de Box-Cox
lambda.est<-BoxCox.lambda(dados.ts, lower = -2, upper = 2)
lambda.est

dadosBoxCox <- BoxCox(dados.ts, lambda.est)

# Diferenciação simples
diff(dadosBoxCox, differences=1)
# ou diff(dadosBoxCox), porque por defeito faz sempre uma

diff(dados.ts, differences=1)

tsdisplay(diff(dados.ts, differences=1)) # Sem transformação de BoxCox
tsdisplay(diff(dadosBoxCox, differences=1)) # Com transformação de BoxCox
# Comprovar a estacionariedade das séries
ndiffs(diff(dados.ts, differences=1))
ndiffs(diff(dadosBoxCox, differences=1))
# Deu 0 em ambos os casos, então os dois são estacionários

# Parece que com a transformação de BoxCox teremos mais parâmetros
# Portanto, experimentamos o modelo sem a transformação de BoxCox

boxplot(diff(dados.ts, differences=1))
# 8 outliers, ou seja 8 valores não entram dentro da Normal.

# Estimar os parâmetros
fit1 <- Arima(dados.ts, order=c(1,1,0), include.mean=TRUE)
### Significância dos parâmetros
confint(fit1)
# Todos os parâmetros são significativos porque o 0 está fora dos intervalos

# Validar o modelo: Análise dos resíduos
tsdisplay(fit1$residuals)
checkresiduals(fit1)
# Valor p = 0.562 > 0.05, não rejeitamos H0
# Portanto, podemos admitir que até lag 21 todas as correlações são nulas
# Ou seja, os resíduos são não-correlacionados

# Verificar a normalidade
library(nortest)
shapiro.test(fit1$residuals) # Valor p < 0.001 < 0.05
lillie.test(fit1$residuals) # Valor p < 0.001 < 0.05
# Rejeitamos o H0 nos dois testes, então não podemos admitir a normalidade

# Como vemos os dados não são normais
# Mas temos uma amostra suficientemente grande para podermos usar o teste t
# O teste t requer normalidade OU amostra suficientemente grande
t.test(fit1$res)
# Valor p = 0.9543 > 0.05, não rejeitamos H0
# Portanto admitimos a média nula
# Então os resíduos são ruído branco por terem correlação e média nulas

### Testar a Aleatoriedade

library(randtests)
difference.sign.test(fit1$residuals) # Valor p = 0.06048 > 0.05
# H1: Não aleatoriedade. Não rejeitamos a aleatoriedade dos resíduos
turning.point.test(fit1$residuals) # Valor p = 0.8745 > 0.05
# H1: Não aleatoriedade. Não rejeitamos a aleatoriedade dos resíduos
rank.test(fit1$residuals) # Valor p = 0.5519 > 0.05
# H1: Têm tendência. Podemos dizer que os resíduos não têm tendência (padrão)


# Avaliar o ajustamento do modelo
plot(dados.ts)
lines(fitted(fit1), col="red")
# O modelo parece acompanhar bem os dados

# Previsão





# ------------------------------------------------------------------------------
# ScriptHmedias

library(stats)
library(nortest)
library(forecast)
library(imputeTS)
library(randtests)

dados <- read.csv("C:\\UNI-L58387-IACD\\S5 - MP\\Prática\\BasesDados\\Hmedias.csv", 
                  header = T, dec=",", sep=";")
summary(dados)
str(dados)

attach(dados)

# Tornar em série temporal:
dados.ts <- ts(Hmed, start=c(1984,11), frequency=12)
str(dados.ts)
# Confirmar a mudança para série temporal:
summary(dados.ts)
# Vemos que existem 2 valores NA
plot(dados.ts, main="Altura média das ondas", xlab="Tempo", ylab="freq")
# Mostra ter missings. Vamos, ainda assim, confirmar com o plot na_distribution
ggplot_na_distribution(dados.ts, title="AA")
# Logo, pelo gráfico, admitimos a existência de missings
statsNA(dados.ts)

# Existem duas lacunas: Março 85 e Setembro 85
# Antes de avançar para o habitual, devemos preencher as lacunas de missings

dados.ts2 <- na_kalman(dados.ts, model = "auto.arima", smooth = TRUE)
ggplot_na_distribution(dados.ts2, title="AA")
# Portanto, não temos mais missings.
# Outro método possível:

dados.ts3 <- na_interpolation(dados.ts, option = "spline")
ggplot_na_distribution(dados.ts3, title="AA")
# Novamente, não temos mais missings

# Verificar a estacionaridade da série
tsdisplay(dados.ts3)
# A Fac converge depressa para dentro das bandas de confiança
ndiffs(dados.ts3)
# A série não tem tendência
nsdiffs(dados.ts3)
# A série tem sazonalidade

plot(dados.ts3, type="o", pch=16)
text(dados.ts3, labels=Data, cex=0.6, pos=4, col="red")

# Substituir os valores problemáticos: Jan-85 (3), Fev-86 (16) & Nov-87 (37)
p = 12
dados.ts3[3] = mean(dados.ts3[3+p], dados.ts3[3+2*p], dados.ts3[3+3*p])
dados.ts3[16] = mean(dados.ts3[16-p], dados.ts3[16+p], dados.ts3[16+2*p])
dados.ts3[37] = mean(dados.ts3[37-3*p], dados.ts3[37-2*p], dados.ts3[37-p], dados.ts3[37+p])
dados.ts3[3]
dados.ts3[16]
dados.ts3[37]
plot(dados.ts3, type="o", pch=16)
text(dados.ts3, labels=Data, cex=0.6, pos=4, col="red")
# Com os valores substituídos, podemos seguir:

# Verificar a estacionaridade da série de novo, pois agora sim está pronta para trabalho
tsdisplay(dados.ts3)
# A Fac converge depressa para dentro das bandas de confiança
ndiffs(dados.ts3)
# A série não tem tendência
nsdiffs(dados.ts3)
# A série tem sazonalidade

### Dividir os dados entre treino e teste
treino<-window(dados.ts3, end=c(1987,10))
teste<-window(dados.ts3, start=c(1987,11))

tsdisplay(treino)
ndiffs(treino)
nsdiffs(treino)
# Os testes ainda nos permitem admitir sazonalidade.

# Diferenciação sazonal (nsdiffs 1) sem box-cox
tsdisplay(diff(dados.ts3, lag=12))
# Parecemos ter perdido a sazonalidade
# Todos os valores estão dentro das bandas de confiança
# Ou seja, temos ruído branco (todos os lags estão dentro das bandas)

# Transformação de boxcox:
(lambda.est<-BoxCox.lambda(treino, lower=-2, upper=2))
# Praticamente 2 de tão próximo (1.99994)
# Transformação de BoxCox
pass.trans <- BoxCox(treino, lambda=lambda.est)
tsdisplay(diff(pass.trans, lag=12))
# Já está dentro das bandas de confiança a partir de ordem 1
# Novamente, temos apenas ruído branco. A mesma coisa que sem box-cox

# Tentemos, então, usar outra transformação de BoxCox
# Por exemplo, com lambda = 0 (logaritmo)
pass.trans2 <- BoxCox(treino, lambda=0)
tsdisplay(diff(pass.trans2, lag=12))
# Novamente, ruído branco.

# Vamos, portanto, voltar a trabalhar com a série pré-transformações
tsdisplay(treino)
# Ignorando a sazonalidade, parece ser um AR6.
fit1 <- Arima(treino, order=c(6,0,0), include.mean=TRUE)
confint(fit1)
fit2 <- Arima(treino, order=c(6,0,0), include.mean=TRUE,
              transform.pars=F,
              fixed=c(NA,NA,NA,0,NA,NA,NA))
confint(fit2)
fit3 <- Arima(treino, order=c(6,0,0), include.mean=TRUE,
              transform.pars=F,
              fixed=c(NA,NA,0,0,NA,NA,NA))
confint(fit3)
fit4 <- Arima(treino, order=c(6,0,0), include.mean=TRUE,
              transform.pars=F,
              fixed=c(NA,0,0,0,NA,NA,NA))
confint(fit4)
fit5 <- Arima(treino, order=c(6,0,0), include.mean=TRUE,
              transform.pars=F,
              fixed=c(0,0,0,0,NA,NA,NA))
confint(fit5)
fit5
checkresiduals(fit5)
# O modelo não é bom porque o teste de ljung-box falha obviamente
# Valor p = 0.001383 < 0.05

# Outra alternativa: Colocar a sazonalidade no modelo
fit6<-Arima(treino, order=c(6,0,0),
            seas=list(order=c(0,0,1),period=12),
            include.mean=TRUE, transform.pars=FALSE,
            fixed=c(0,0,0,0,0,NA,NA,NA))
confint(fit6)
checkresiduals(fit6$residuals)
# Valor p = 0.4761 > 0.05
t.test(fit6$residuals)
# Valor p = 0.3798 > 0.05

plot(treino)
lines(fitted(fit6), col="red")

# Experimentar a sazonalidade no AR
fit7<-Arima(treino, order=c(6,0,0),
            seas=list(order=c(1,0,0),period=12),
            include.mean=TRUE, transform.pars=FALSE)
confint(fit7)
fit8<-Arima(treino, order=c(6,0,0),
            seas=list(order=c(1,0,0),period=12),
            include.mean=TRUE, transform.pars=FALSE,
            fixed=c(0,0,0,0,NA,NA,NA,NA))
confint(fit8)
checkresiduals(fit8$residuals)
# Valor p = 0.9692 > 0.05
t.test(fit8$residuals)
# Valor p = 0.2512 > 0.05

plot(treino)
lines(fitted(fit8), col="red")
lines(fitted(fit6), col="green")
accuracy(fit6)
accuracy(fit8)
fit6
fit8





# ------------------------------------------------------------------------------
# Script_ts2

### Ler a Base de Dados

dados <- read.csv("C:\\UNI-L58387-IACD\\S5 - MP\\Prática\\BasesDados\\ts2.csv", 
                  fileEncoding = "utf-8", 
                  stringsAsFactors = T, 
                  na.strings = "")
summary(dados)
class(dados)
str(dados)
# X - Índice da amostra
# Time - Tempo
# Val - Série
library(dplyr)
glimpse(dados)

#### Transformação do formato dos dados em série temporal
dados.ts <- ts(dados$val, start=c(1980,1), frequency=12)
end(dados.ts)
# Ou, se a variável não tivesse nome
series <- dados[,3]
series
# E usávamos ts para criar a série temporal
###########################################
# Verificar a existência de missings:
library(imputeTS)
ggplot_na_distribution(dados.ts, title="AA")
# Portanto, não temos missings na nossa série temporal

# Verificar a estacionaridade da série
library(forecast)
tsdisplay(dados.ts)
# Parece ser estacionária do tipo AR(4), mas por contexto da aula vamos fazer um AR(8)
library(tseries)
adf.test(dados.ts)
# Valor p = 0.01 < 0.05, rejeitamos H0
# Pelo que admitimos a estacionaridade da nossa série temporal
ndiffs(dados.ts)
# 0, logo a série é estacionária (ARIMA(p,0,q) = ARMA(p,q))

hist(dados.ts)
# Formato de uma distribuição normal
boxplot(dados.ts)
# 2 outliers, ou seja 2 valores não entram dentro da Normal.

# Estimar os parâmetros
fit1 <- Arima(dados.ts, order=c(8,0,0), include.mean=TRUE, method="ML")
### Significância dos parâmetros
confint(fit1)
# A maioria dos parâmetros não são significantes e devem ser removidos
# Começando por baixo (8 fica, 7 sai por não ser significante)
# Os valores devem ser removidos um a um
fit2 <- Arima(dados.ts, order=c(8,0,0), include.mean=TRUE,
              transform.pars=F,
              fixed=c(NA,NA,NA,NA,NA,NA,0,NA,NA))
confint(fit2)
# Remover 5 agora
fit3 <- Arima(dados.ts, order=c(8,0,0), include.mean=TRUE,
              transform.pars=F,
              fixed=c(NA,NA,NA,NA,0,NA,0,NA,NA))
confint(fit3)
# Remover 3 agora
fit4 <- Arima(dados.ts, order=c(8,0,0), include.mean=TRUE,
              transform.pars=F,
              fixed=c(NA,NA,0,NA,0,NA,0,NA,NA))
confint(fit4)
# Remover 2 agora
fit5 <- Arima(dados.ts, order=c(8,0,0), include.mean=TRUE,
              transform.pars=F,
              fixed=c(NA,0,0,NA,0,NA,0,NA,NA))
# Existe erro por termos sido demasiado ambiciosos com AR(8)
# Ajustamos um AR(4) como o PACF diz ao invés disso (como sabia)
fit6 <- Arima(dados.ts, order=c(4,0,0), include.mean=TRUE)
confint(fit6)
# Removemos 3
fit7 <- Arima(dados.ts, order=c(4,0,0), include.mean=TRUE,
              transform.pars=F,
              fixed=c(NA,NA,0,NA,NA))
confint(fit7)
# Removemos 2
fit8 <- Arima(dados.ts, order=c(4,0,0), include.mean=TRUE,
              transform.pars=F,
              fixed=c(NA,0,0,NA,NA))
confint(fit8)
# E assim não temos que remover mais nenhum
# Análise dos resíduos
tsdisplay(fit8$residuals)
checkresiduals(fit8)
# Valor p = 0.0375 < 0.05, rejeitamos H0
# Portanto, não podemos admitir que as correlações são todas nulas
# Então os resíduos não são ruído branco
# Por exemplo, sem tirar 2 e 3 do modelo:
fit9 <- Arima(dados.ts, order=c(4,0,0), include.mean=TRUE)
confint(fit9)
tsdisplay(fit9$residuals)
checkresiduals(fit9)
# Melhorou, mas ainda não é bom o suficiente
checkresiduals(fit9, lag=15)
# Com lag superior o teste melhora e podemos admitir que são ruído branco
# Contudo, o valor p ainda é muito pequeno, o que torna isto desconfiável
# Verificar se a média dos resíduos é 0
t.test(fit9$res)
# Valor p = 0.9588 > 0.05, não rejeitamos H0, a média é nula

tsdisplay(dados.ts)

# Tentativa de solução: Ajustar um ARMA ao invés de um AR
# Testar ARMA(2,2) e ir aumentando o MA até conseguir
fit10 <- Arima(dados.ts, order=c(2,0,4), include.mean=TRUE)
checkresiduals(fit10)
# ARMA(2,4) parece funcionar!
t.test(fit10$res)
# Valor p = 0.9469 > 0.05, não rejeitamos H0, a média é nula

library(nortest)
shapiro.test(fit10$residuals) # Valor p = 0.2591 > 0.05
lillie.test(fit10$residuals) # Valor p = 0.3121 > 0.05
# Pelos dois testes podemos admitir a normalidade

### Testar a Aleatoriedade

library(randtests)

difference.sign.test(fit10$residuals) # Valor p = 0.8164 > 0.05
# H1: Não aleatoriedade. Não rejeitamos a aleatoriedade dos resíduos
turning.point.test(fit10$residuals) # Valor p = 0.2023 > 0.05
# H1: Não aleatoriedade. Não rejeitamos a aleatoriedade dos resíduos
rank.test(fit10$residuals) # Valor p = 0.8403 > 0.05
# H1: Têm tendência. Podemos dizer que os resíduos não têm tendência (padrão)

difference.sign.test(fit9$residuals) # Valor p = 0.6988 > 0.05
# H1: Não aleatoriedade. Não rejeitamos a aleatoriedade dos resíduos
turning.point.test(fit9$residuals) # Valor p = 0.2023 > 0.05
# H1: Não aleatoriedade. Não rejeitamos a aleatoriedade dos resíduos
rank.test(fit9$residuals) # Valor p = 0.9624 > 0.05
# H1: Têm tendência. Podemos dizer que os resíduos não têm tendência (padrão)


plot(dados.ts)
lines(fitted(fit10), col="red")
lines(fitted(fit9), col="green")
# Distribuição dos dados e ajuste do modelo em comparação à série original
# Colocado também o modelo anterior, o AR(4), como a cor verde
# Os dois modelos mostram acompanhar bem a série original

# Comparar os modelos:
results_1 <- data.frame(Model = c("Modelo 9", "Modelo 10"),
                        AIC = c(fit9$aic, fit10$aic),
                        BIC = c(fit9$bic, fit10$bic),
                        AICc = c(fit9$aicc, fit10$aicc))
results_1

# O modelo 10 tem um AIC e um AICc menores mas um BIC maior que o modelo 9
# É adequado fazer a escolha sobretudo com base no AIC e no AICc
# Pelo que admitimos o modelo 10 como o melhor modelo entre os dois

accuracy(fit9)
accuracy(fit10)
# O modelo com os menores erros é o modelo mais apropriado
# Como o modelo 10 tem erros menores, mantemos o modelo 10


# Previsão
plot(forecast(fit10, h=24))
# Bandas escuras - intervalo de confiança a 80%
# Bandas claras - intervalo de confiança a 95%
plot(forecast(fit10, h=48))


plot(forecast(fit10, h=24))
lines(fitted(fit10), col="green")





# ------------------------------------------------------------------------------
# ScriptAirPassenger

library(forecast)
# Monthly Airline Passengers Numbers 1949-1960
data(AirPassengers)

summary(AirPassengers)
str(AirPassengers)
plot(AirPassengers, main="mortes por acidentes rodoviários no UK", xlab="Tempo", ylab="freq")
# Parece ser não-estacionária com sazonalidade

# Verificar a existência de missings:
library(imputeTS)
ggplot_na_distribution(AirPassengers, title="AA")
# Pela observação do gráfico a série não aparenta ter missings 

# Verificar a estacionaridade da série
library(forecast)
tsdisplay(AirPassengers)
# Portanto, como a FAC demora para convergir para dentro das bandas de confiança,
# Podemos admitir que a série é não estacionária
ndiffs(AirPassengers)
# E, como o ndiffs é diferente de 0, podemos, pelo teste, admitir a não-estacionaridade

# Verificar a sasonalidade da série
nsdiffs(AirPassengers)
# Diferente de 0, portanto podemos admitir que a série tem sazonalidade
# Algo que já assumimos por observação do gráfico.
# Ou seja, estamos perante um modelo SARIMA


### Dividir os dados entre treino e teste
treino<-window(AirPassengers, end=c(1958,12))
teste<-window(AirPassengers, start=c(1959,1))

tsdisplay(treino)
ndiffs(treino)
nsdiffs(treino)

# Série não estacionária -> estacionária
BoxCox.lambda(treino, lower = -2, upper = 2)
# Por ora não fazemos nada, mas como o valor é próximo de 0
# Se, adiante, as coisas derem problemas então usamos transformação logaritmica

# Quando a diferenciação é sazonal, é necessário dizer o período
# Lag -> Período
# Diferenciação simples depois da diferenciação sazonal
tsdisplay(diff(diff(AirPassengers, lag=12)))
# Com as duas transformações feitas parece ser estacionária
# Pois a FAC converge muito rapidamente para dentro das bandas de confiança

# Como perdemos a sazonalidade, vamos testar a transformação de BoxCox
(lambda.est<-BoxCox.lambda(treino, lower=-2, upper=2))
# Transformação de BoxCox
pass.trans2 <- BoxCox(treino, lambda=lambda.est)
tsdisplay(diff(diff(pass.trans2, lag=12)))
# Dentro do período (até 12) -> ARMA(1,1) para não ser demasiado ambicioso
# Entre o período (múltiplos de 12) -> 12 está de fora nos dois lados mas é o único (1,1 também)

# Portanto, vamos experimentar um SARIMA(1,1,1)x(1,1,1)
fit.treino<-Arima(treino, order=c(1,1,1),
                  seas=list(order=c(1,1,1),period=12),
                  include.drift=TRUE,
                  lambda=lambda.est)
fit.treino
confint(fit.treino)
# AR1 e SAR1 não são significativos
# Começamos por remover SAR1:
fit.treino2<-Arima(treino, order=c(1,1,1),
                   seas=list(order=c(0,1,1),period=12),
                   lambda=lambda.est)
confint(fit.treino2)
# AR1 ainda não é significativo, removemos também
fit.treino3<-Arima(treino, order=c(0,1,1),
                   seas=list(order=c(0,1,1),period=12),
                   lambda=lambda.est)
confint(fit.treino3)
# Todos os valores são significativos, não removemos nenhum
fit.treino3


# Validar o modelo: Análise dos resíduos
tsdisplay(fit.treino3$residuals)
checkresiduals(fit.treino3)
# Valor p = 0.3665 > 0.05, não rejeitamos H0
# Portanto, podemos admitir que até lag 24 todas as correlações são nulas
# Ou seja, os resíduos são não-correlacionados
# Como no gráfico os dados já parecem aproximadamente normais
# Não testamos a normalidade

t.test(fit.treino3$res)
# Valor p = 0.8047 > 0.05, não rejeitamos H0
# Podemos, portanto, admitir a média nula
# Então os resíduos são ruído branco por terem correlação e média nulas

### Testar a Aleatoriedade

library(randtests)
difference.sign.test(fit.treino3$residuals) # Valor p = 0.6367 > 0.05
# H1: Não aleatoriedade. Não rejeitamos a aleatoriedade dos resíduos
turning.point.test(fit.treino3$residuals) # Valor p = 0.5607 > 0.05
# H1: Não aleatoriedade. Não rejeitamos a aleatoriedade dos resíduos
rank.test(fit.treino3$residuals) # Valor p = 0.1219 > 0.05
# H1: Têm tendência. Podemos dizer que os resíduos não têm tendência (padrão)


# Avaliar o ajustamento do modelo
plot(treino)
lines(fitted(fit.treino3), col="red")
# O modelo parece acompanhar bem os dados
accuracy(fit.treino3)
# Portanto, cerca de 2.75% dos dados é que sofrem predição errada. O que é muito bom!

# Previsão
plot(forecast(fit.treino3))
accuracy(forecast(fit.treino3, h=24), teste)
# Portanto, a predição do modelo é boa porque apenas cerca de 6.46% dos dados são mal previstos

# Portanto, precisamos agora de pôr a linha, a vermelho, dos dados reais (teste):
lines(teste, col="red")
# E adicionamos também o modelo de treino no gráfico, a cor azul:
lines(fitted(fit.treino3), col="blue")





# ------------------------------------------------------------------------------
# ScriptL12

library(RSiteSearch) # Para procurar package
sos <- help.search('Box-Cox')
HTML(sos) # Mostra os resultados numa página web

library(stats)
library(nortest)
library(forecast)
library(imputeTS)
library(randtests)

dados <- read.table("C:\\UNI-L58387-IACD\\S5 - MP\\Prática\\BasesDados\\L12.txt")
# Venda de Passes L12 em Lisboa (Jan91-Dez98)
summary(dados)
str(dados)

# Formato data frame. Tornar em série temporal:

dados.ts <- ts(dados$V1, start=c(1991,1), frequency=12)
# Confirmar a mudança para série temporal:
summary(dados.ts)
str(dados.ts)
plot(dados.ts, main="Passes L12 vendidos em Lisboa", xlab="Tempo", ylab="freq")

# Verificar a existência de missings:
ggplot_na_distribution(dados.ts, title="AA")
# Portanto, pelo gráfico, admitimos que os dados não tem missings em falta.

# Verificar a estacionaridade da série
tsdisplay(dados.ts)
# A Fac desce depressa, mas só converge dentro das bandas de confiança
# Em ordem 15. Portanto, podemos rejeitar a estacionaridade
ndiffs(dados.ts)
# E, como o ndiffs é diferente de 0, podemos, pelo teste, admitir a não-estacionaridade

# Verificar a sazonalidade da série
nsdiffs(dados.ts)
# Como nsdiffs é diferente de 0, podemos, pelo teste, admitir que existe sazonalidade

### Dividir os dados entre treino e teste
treino<-window(dados.ts, end=c(1996,12))
teste<-window(dados.ts, start=c(1997,1))

tsdisplay(treino)
ndiffs(treino)
nsdiffs(treino)
# Os testes ainda nos permitem admitir não-estacionaridade e sazonalidade.

# Série não estacionária -> estacionária
BoxCox.lambda(treino, lower = -2, upper = 2)
# Valor de BoxCox = -0.2878298
# Próximo de 0, então a transformação logarítmica é uma opção

# Diferenciação simples depois da diferenciação sazonal (ndiffs 1 e nsdiffs 1)
tsdisplay(diff(diff(dados.ts, lag=12)))
# Já está dentro das bandas de confiança a partir de ordem 1!
# Parece, portanto, ser estacionária.
# Parecemos, também, ter a sazonalidade ao ver o gráfico!
# Valor de ordem 12 na FAC e FACP fora das bandas de confiança
# Portanto, não parece haver problemas com a sazonalidade
# Valores até ordem 12: Nenhum de fora (ARMA(0,0))
# Múltiplos de 12: Apenas ordem 12 está de fora nos dois casos (1,1)

# Transformação de boxcox:
(lambda.est<-BoxCox.lambda(treino, lower=-2, upper=2))
# Transformação de BoxCox
pass.trans2 <- BoxCox(treino, lambda=lambda.est)
tsdisplay(diff(diff(pass.trans2, lag=12)))
# Já está dentro das bandas de confiança a partir de ordem 1
# Parece ser estacionária, por isso.
# Valores de ordem 12 na FAC e FACP fora das bandas de confiança
# Valores até ordem 12: Nenhum de fora (ARMA(0,0))
# Múltiplos de 12: Apenas ordem 12 está de fora nos dois casos (1,1)


### Ajustar o modelo a usar a transformação de boxcox:
# SARIMA(0,1,0)x(1,1,1)
fit.treino<-Arima(treino, order=c(0,1,0),
                  seas=list(order=c(1,1,1),period=12),
                  include.drift=TRUE,
                  lambda=lambda.est)
fit.treino
confint(fit.treino)
# SAR1 pode ser removido porque o 0 pertence ao intervalo de confiança.
fit.treino2<-Arima(treino, order=c(0,1,0),
                   seas=list(order=c(0,1,1),period=12),
                   include.drift=TRUE,
                   lambda=lambda.est)
confint(fit.treino2)
# SAM1 não precisa ser removido, então acabamos por aqui.
fit.treino2

# Validar o modelo: Análise dos resíduos
tsdisplay(fit.treino2$residuals)
checkresiduals(fit.treino2)
# Valor p = 0.9572 > 0.05, não rejeitamos H0, podemos admitir não-correlação até ordem 14
# O gráfico dos dados parece irregular, então testamos a normalidade também.
shapiro.test(fit.treino2$residuals) # Valor p < 0.001 < 0.05
lillie.test(fit.treino2$residuals) # Valor p < 0.001 < 0.05
# Rejeitamos o H0 nos dois testes, então não podemos admitir a normalidade

# Mas temos uma amostra de 96 elementos, então podemos usar o teste t (acho)
t.test(fit.treino2$residuals)
# Valor p = 0.5398 > 0.05, não rejeitamos H0
# Então admitimos a média nula
# Por terem média nula e correlação nula, dizemos que os resíduos são ruído branco

# Testemos, agora, a aleatoriedade dos resíduos
difference.sign.test(fit.treino2$residuals) # Valor p < 0.001  0.05
# H1: Não aleatoriedade. Rejeitamos a aleatoriedade dos resíduos
turning.point.test(fit.treino2$residuals) # Valor p = 0.1087 > 0.05
# H1: Não aleatoriedade. Não rejeitamos a aleatoriedade dos resíduos
rank.test(fit.treino2$residuals) # Valor p = 0.5148 > 0.05
# H1: Têm tendência. Podemos dizer que os resíduos não têm tendência (padrão)


# Avaliar o ajustamento do modelo
plot(treino)
lines(fitted(fit.treino2), col="red")
# O modelo parece acompanhar bem os dados, contudo tem um problema enorme em 1992
accuracy(fit.treino2)
# 1.84% dos dados estão incorretamente ajustados pelo MAPE

# Previsão
plot(forecast(fit.treino2))
accuracy(forecast(fit.treino2, h=24), teste)
# Portanto, a predição do modelo é boa porque apenas cerca de 6.48% dos dados são mal previstos

# Portanto, precisamos agora de pôr a linha, a vermelho, dos dados reais (teste):
lines(teste, col="red")
# E adicionamos também o modelo de treino no gráfico, a cor azul:
lines(fitted(fit.treino2), col="blue")

### Ajustar o modelo a usar as diferenciações:
# SARIMA(0,1,0)x(1,1,1)
fit.treino3<-Arima(treino, order=c(0,1,0),
                   seas=list(order=c(1,1,1),period=12),
                   include.drift=TRUE)
fit.treino3
confint(fit.treino3)
# SAM1 precisa de ser removido.
fit.treino4<-Arima(treino, order=c(0,1,0),
                   seas=list(order=c(1,1,0),period=12),
                   include.drift=TRUE)
confint(fit.treino4)
# SAR1 não precisa ser removido.
fit.treino4

# Validar o modelo: Análise dos resíduos
tsdisplay(fit.treino4$residuals)
checkresiduals(fit.treino4)
# Valor p = 0.6376 > 0.05, não rejeitamos H0, podemos admitir não-correlação até ordem 14
# Testemos a normalidade também.
shapiro.test(fit.treino4$residuals) # Valor p = 0.02265 < 0.05
lillie.test(fit.treino4$residuals) # Valor p = 0.09455 > 0.05
# Rejeitamos o H0 no shapiro mas não no lillie, então podemos assumir a normalidade

t.test(fit.treino4$residuals)
# Valor p = 0.6786 > 0.05, não rejeitamos H0
# Então admitimos a média nula
# Por terem média nula e correlação nula, dizemos que os resíduos são ruído branco

# Testemos, agora, a aleatoriedade dos resíduos
difference.sign.test(fit.treino4$residuals) # Valor p = 0.06808 > 0.05
# H1: Não aleatoriedade. Não rejeitamos a aleatoriedade dos resíduos
turning.point.test(fit.treino4$residuals) # Valor p = 0.1087 > 0.05
# H1: Não aleatoriedade. Não rejeitamos a aleatoriedade dos resíduos
rank.test(fit.treino4$residuals) # Valor p = 0.7118 > 0.05
# H1: Têm tendência. Podemos dizer que os resíduos não têm tendência (padrão)

# Avaliar o ajustamento do modelo
plot(treino)
lines(fitted(fit.treino4), col="red")
# O modelo parece acompanhar bem os dados, contudo tem um problema enorme em 1992
accuracy(fit.treino4)
# 1.35% dos dados estão incorretamente ajustados pelo MAPE

# Previsão
plot(forecast(fit.treino4))
accuracy(forecast(fit.treino4, h=24), teste)
# Portanto, a predição do modelo é boa porque apenas cerca de 3% dos dados são mal previstos

# Portanto, precisamos agora de pôr a linha, a vermelho, dos dados reais (teste):
lines(teste, col="red")
# E adicionamos também o modelo de treino no gráfico, a cor azul:
lines(fitted(fit.treino4), col="blue")

# Portanto, o modelo sem a transformação de Box-Cox parece melhor ajustado aos dados
# A previsão é, também, superior!
# Portanto, consideramos o modelo sem as transformações de Box-Cox como o nosso modelo

### Usar o modelo para previsão de novos valores:

fit.treino5<-Arima(dados.ts, order=c(0,1,0),
                   seas=list(order=c(1,1,0),period=12),
                   include.drift=TRUE)
fit.treino5
confint(fit.treino5)

# Validação do modelo para o conjunto de dados total:
tsdisplay(fit.treino5$residuals)
checkresiduals(fit.treino5)
# Valor p = 0.6082 > 0.05, não rejeitamos H0, podemos admitir não-correlação até ordem 14
# Testemos a normalidade também.
shapiro.test(fit.treino5$residuals) # Valor p = 0.02704 < 0.05
lillie.test(fit.treino5$residuals) # Valor p = 0.03115 > 0.05
# Rejeitamos o H0 nos dois testes, não podemos admitir a normalidade

# Mas os resíduos são simétricos o suficiente, e em quantidade o suficiente
# Para permitir a robustez do teste t.
t.test(fit.treino5$residuals)
# Valor p = 0.6567 > 0.05, não rejeitamos H0
# Então admitimos a média nula
# Por terem média nula e correlação nula, dizemos que os resíduos são ruído branco

# Testemos, agora, a aleatoriedade dos resíduos
difference.sign.test(fit.treino5$residuals) # Valor p = 0.02224 < 0.05
# H1: Não aleatoriedade. Rejeitamos a aleatoriedade dos resíduos
turning.point.test(fit.treino5$residuals) # Valor p = 0.5146 > 0.05
# H1: Não aleatoriedade. Não rejeitamos a aleatoriedade dos resíduos
rank.test(fit.treino5$residuals) # Valor p = 0.9093 > 0.05
# H1: Têm tendência. Podemos dizer que os resíduos não têm tendência (padrão)

# Previsão para os dois anos futuros
plot(forecast(fit.treino5))

#### lambda.est.l <- 0 # logaritmo





# ------------------------------------------------------------------------------
# ScriptAirPassengers2

data(AirPassengers)
class(AirPassengers)


library(TSstudio)
ts_info(AirPassengers)

# Ao invés de:
# plot(dados, main="TITULO", xlab="VARIÁVEL TEMPO", ylab="Y")
# plot.ts(dados)
ts_plot(AirPassengers)
ts_plot(AirPassengers,
        title = "Número de passageiros aéreos entre 1949 e 1960",
        Ytitle = "Número mensal de passageiros",
        Xtitle = "Fonte: RStudio",
        slider = T)
# Permite visualização interativa, zoom temporal e design mais claro.

# Complementa visualmente plot(dados.ts):
ts_heatmap(AirPassengers)
# Mostra de forma instantânea a sazonalidade e anos atípicos 
# (muito útil antes da diferenciação).

# Complementa o tsdisplay()
ts_seasonal(AirPassengers, type = "all")
# Ilustra imediatamente padrões sazonais mensais/trimestrais — mais intuitivo que o olhar apenas para FAC/PACF.

# Alternativa a tsdisplay() ou apoio à escolha de (p,q):
ts_cor(AirPassengers, lag.max = 40)
ts_lags(AirPassengers)
ts_lags(AirPassengers, lags = c(12, 24, 36, 48))
# Gráficos claros de autocorrelações e dispersões defasadas, ajudam a confirmar 
# ordens AR e MA antes de Arima().

# Opcional
ts_polar(AirPassengers)
# Destaca ciclos anuais (excelente para séries mensais com padrão recorrente, 
# como consumo, turismo, etc.).

# Decomposição aditiva
dec_air <- decompose(AirPassengers, type="additive")
plot(dec_air)