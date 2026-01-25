# Miguel Grilo
# ----------------------------------------------------
###### SCRIPT UNIVERSAL - PARTE 1 ATUALIZADO
###### SÉRIES TEMPORAIS: ARIMA, SARIMA -> ETS, TSLM, DECOMPOSIÇÃO

### Carregar bibliotecas
#   ...
library(stats)      # Funções base de séries temporais
library(forecast)   # tsdisplay(), Arima(), ndiffs(), forecast(), checkresiduals()
library(tseries)    # adf.test()
library(imputeTS)   # na_kalman(), ggplot_na_distribution(), statsNA()
library(randtests)  # difference.sign.test(), turning.point.test(), rank.test()
library(nortest)    # shapiro.test(), lillie.test()
# Opcional:
library(fBasics)   # basicStats()
library(dplyr)     # glimpse()
library(TSstudio)   # Visualização interativa e avançada
library(tsibble)    # Estruturas de dados temporais modernas
library(feasts)     # Feature extraction e estatísticas (STL robusto)
library(fable)      # Necessário para a função model() usada com feasts

#ts_info(dados.ts)  # Mostra informações básicas da série temporal: frequência, início, fim, número de observações e periodicidade
#ts_plot(dados.ts, 
#        title="Série temporal original", 
#        Ytitle="Valores", 
#        slider=TRUE)  # Cria um gráfico interativo da série, permitindo explorar visualmente tendência, sazonalidade e zoom temporal
#ts_seasonal(dados.ts, type="all")  # Analisa a sazonalidade, mostra padrões mensais/trimestrais recorrentes ao longo dos anos
#ts_heatmap(dados.ts)  # Exibe um heatmap que destaca visualmente períodos de maior e menor valor em cada ano/mês
#ts_cor(dados.ts, lag.max=40)  # Mostra a função de autocorrelação (ACF) de forma visual, ajudando a identificar dependências temporais e possíveis ordens MA.   
#ts_lags(dados.ts)  # Exibe diagramas de dispersão com diferentes defasagens (lags), úteis para identificar relações lineares e possíveis componentes AR.




### Ler a base de dados
##        Para .csv
dados <- read.csv("...\\FICHEIRO.csv",
                  fileEncoding = "utf-8", 
                  stringsAsFactors = T, 
                  na.strings = "",
                  dec=",", sep=";")
# Verificar se possui header = T/F
## ou
## Para tabelas .txt
dados <- read.table("...\\FICHEIRO.txt")
## ou
## Para conjuntos de dados de bibliotecas do R
library(forecast)
data(gold) # Por exemplo




### Formato dos Dados
##        Identificação do formato
attach(dados) # Opcional
# Torna as colunas do objeto acessíveis diretamente pelo nome
# Por exemplo, em vez de usar dados$x, posso usar apenas x

plot(dados, main="TITULO", xlab="VARIÁVEL TEMPO", ylab="Y")
plot.ts(dados)
# Podemos verificar graficamente a existência de tendência/estacionaridade
# Assim como a sazonalidade

summary(dados)
head(dados)
str(dados) 
class(dados)
library(dplyr)
glimpse(dados)
# Verificar o formato dos dados

# Opcional:   dados2<-dados+25  # Adiciona 25 aos valores da base de dados inicial (caso necessário)
# O que não afeta as propriedades da série temporal
# Opcional:   (n<-dim(dados)[1]) # Dimensão dos dados (caso necessário)

## Transformar o formato dos dados em série temporal
dados.ts <- ts(dados, start=c(ANO_INICIAL,MES_INICIAL), frequency=FREQ)
# Para FREQ= ...
# 12 (mensal), 4 (trimestral), 1 (anual)
class(dados.ts)
# Verificar se o formato dos dados foi alterado corretamente

# length(dados.ts)  # N de observações
# start(dados.ts)   # Ano e mês inicial da série temporal
# end(dados.ts)     # Ano e mês final da série temporal

plot(dados.ts, main="SÉRIE TEMPORAL ORIGINAL", ylab="VALORES", xlab="TEMPO")




### Identificação de missings e outliers
## Identificar missings
library(imputeTS)
ggplot_na_distribution(dados.ts, title="Distribuição de Missings")
ggplot_na_distribution2(dados.ts, title="Distribuição de Missings")
# Verificamos a existência de missings graficamente.
statsNA(dados.ts)
summary(is.na(dados.ts))
# Contagem de missings

## Corrigir os missings
dados.ts2 <- na_kalman(dados.ts, model = "auto.arima", smooth = TRUE)
ggplot_na_distribution(dados.ts2, title="AA")
# Verificar se ainda existem missings
# Outro método possível:
dados.ts3 <- na_interpolation(dados.ts, option = "linear")
ggplot_na_distribution(dados.ts3, title="AA")
# Verificar se ainda existem missings
dados.ts4 <- na_interpolation(gold, option = "spline") # GERALMENTE O MELHOR PARA SÉRIES TEMPORAIS
ggplot_na_distribution(dados.ts4, title="AA")
# Verificar se ainda existem missings

statsNA(dados.ts4)
summary(is.na(dados.ts4))
# Confirmamos a não existência de missings numericamente.
# Avançamos com a data corrigida a partir da na_interpolation com opção spline (Por exemplo).
class(dados.ts4)
# Verificamos se o formato dos dados se mantém

plot(dados.ts4, col="red")
lines(dados.ts4, col="blue")

## Identificar outliers
tsoutliers(dados.ts4)
# Verificamos a existência de um outlier na posição ...
# Recomenda trocar o seu valor por ...

## Aplicar a correção sugerida ao outlier
dados.ts5<- tsclean(dados.ts4)
plot(dados.ts4)
lines(dados.ts5, col="red")
# Verificamos então o modelo com a sugestão alterada

## TRATAMENTO MANUAL DE OUTLIERS (SUBSTITUIÇÃO PELA MÉDIA SAZONAL)
#        SEM TENDÊNCIA/ESTACIONARIEDADE E COM SAZONALIDADE
# Substituir os valores problemáticos: (3), (16) & (37)     (Por exemplo)
p = 12
dados.ts3[3] = mean(dados.ts3[3+p], dados.ts3[3+2*p], dados.ts3[3+3*p])
dados.ts3[16] = mean(dados.ts3[16-p], dados.ts3[16+p], dados.ts3[16+2*p])
dados.ts3[37] = mean(dados.ts3[37-3*p], dados.ts3[37-2*p], dados.ts3[37-p], dados.ts3[37+p])
dados.ts3[3]
dados.ts3[16]
dados.ts3[37]
plot(dados.ts4, type="o", pch=16)
text(dados.ts4, labels=Data, cex=0.6, pos=4, col="red")
# Com os valores substituídos, podemos seguir...




### Análise Exploratória Avançada (NOVO - TSstudio)
# Informação da estrutura
ts_info(dados.ts4)

# Gráfico Interativo (Zoom e Slider)
ts_plot(dados.ts4, 
        title="Série Temporal Interativa", 
        Ytitle="Valores", 
        slider=TRUE)

# Mapa de Calor (Sazonalidade vs Ano)
ts_heatmap(dados.ts4, title="Mapa de Calor da Série")

# Análise de Sazonalidade (Boxplots por ciclo)
ts_seasonal(dados.ts4, type="all")

# Correlação e Lags (Alternativa visual ao ACF)
ts_cor(dados.ts4, lag.max=40)
ts_lags(dados.ts4, lags=c(12, 24, 36)) # Ajustar lags à frequência
ts_polar(dados.ts4, title="Gráfico Polar (Ciclos)")




### Decomposição da Série (NOVO - Decompose e STL)

## Decomposição Clássica
# Escolher 'additive' (sazonalidade constante) 
# ou 'multiplicative' (sazonalidade cresce com tendência)
dec.classica <- decompose(dados.ts4, type="multiplicative") 
plot(dec.classica)
checkresiduals(dec.classica$random) # Verificar se a componente aleatória é ruído


## Decomposição Robusta (STL)
# Lida melhor com outliers e mudanças de padrão
stl_model <- stl(dados.ts4, s.window="periodic", robust=TRUE)
plot(stl_model, main="Decomposição STL Robusta")




### Verificar a estacionariedade

tsdisplay(dados.ts4) # teste KPSS (por defeito) OU PP
# No gráfico superior verificamos a existência de uma possível tendência
# Se existir tendência então não será estacionária
# Na FAC(ACF) verificamos a velocidade com que converge para a banda de confiança,
# Se for um decrescimento lento então não será estacionária.

library(tseries)
adf.test(dados.ts4)
# Devemos verificar a hipótese alternativa para tirar conclusões
# Se H1 = estacionária e p-value<0.05 então rejeitamos H0 e é, de facto, estacionária.

# Deve-se ter cuidado com este teste porque este teste só faz a estacionaridade
# baseado em uma autocorrelação de ordem 1, se tivemos autocorrelações mais fortes 
# o teste pode não as apanhar, ou seja, o teste é muito limitado

# Outro teste possível e mais aconselhado:
library(forecast)
# Verificar a estacionariedade da série
ndiffs(dados.ts4, method = "adf")
# Se = 0, o número de diffs é 0, logo a série é estacionária
# Se /= 0, a série não é estacionária

# Verificar a sazonalidade da série
nsdiffs(dados.ts4)
# Se = 0, a série não é sazonal
# Se /= 0, a série é sazonal

# Aplicar os diffs
#serie_estacionaria <- diff(TREINO, differences=1) # Diferenciação simples (d)
#serie_estacionaria <- diff(serie_estacionaria, lag=FREQUENCIA, differences=1) # Diferenciação sazonal (D)




### Dividir os dados para conjuntos de treino e teste
treino<-window(dados.ts4, end=c(ANO_FIM_TREINO, MES_FIM_TREINO))
teste<-window(dados.ts4, start=c(ANO_INICIO_TESTE, MES_INICIO_TESTE))
# A melhor proporção para o teste é 1 ou 2 vezes a frequência da série,
# ou seja, se a frequência for igual a 12 então o conjunto de teste terá
# os últimos 12/24 meses.
ndiffs(treino)
nsdiffs(treino)
# Voltar a testar ambos para verificar se se mantêm iguais.




### Transformações
## Série não estacionária -> estacionária
# Transformação de Box-Cox
(lambda.est<-BoxCox.lambda(treino, lower = -2, upper = 2))
# Se for próximo de 0 podemos futuramente usar a transformação logaritmo,
# ou seja, lambda.est = 0

dadosBoxCox <- BoxCox(treino, lambda.est)

# Diferenciação simples
#diff(dadosBoxCox, differences=1) # differences = 1 por defeito
#diff(treino, differences=1)

# Para não estacionária
tsdisplay(diff(treino, differences=1)) # Sem transformação de BoxCox
tsdisplay(diff(dadosBoxCox, differences=1)) # Com transformação de BoxCox
# Para não estacionária e sazonal
tsdisplay(diff(diff(treino, lag=12)))
# Se perdermos a sazonalidade após os diffs testamos a transformação BoxCox
tsdisplay(diff(diff(dadosBoxCox, lag=12)))

# Comprovar a estacionariedade das séries
ndiffs(diff(treino, differences=1))
ndiffs(diff(dadosBoxCox, differences=1))
# Se um dos dois for = 0, então é estacionário e usaremos essa transformações.
# Caso ambos forem = 0, então escolhemos o que pareça menos complexo, com menos parâmetros.

hist(diff(treino))
boxplot(diff(treino, differences=1))
# Verifica outliers

# Se após as transformações obtivermos ruído branco -> Voltar à série original




### Identificação do modelo
### Abordagens Alternativas Rápidas (NOVO)
# Usar para ter benchmarks antes de fazer o ARIMA manual


## Auto ARIMA (Automático)
fit.auto <- auto.arima(treino, seasonal=TRUE, stepwise=FALSE, approximation=FALSE)
summary(fit.auto)
checkresiduals(fit.auto)


## TSLM (Modelo Linear com Tendência e Sazonalidade)
# Útil se a tendência for determinística (linear simples)
fit.tslm <- tslm(treino ~ trend + season)
summary(fit.tslm)
checkresiduals(fit.tslm) # TSLM falha frequentemente nos resíduos, mas é explicativo


## ETS (Exponential Smoothing)
# Muito robusto. Otimização automática de Erro, Tendência e Sazonalidade
fit.ets <- ets(treino) # Padrão
fit.ets_opt <- ets(treino, opt.crit = "sigma") # Otimizado por Sigma
summary(fit.ets)
checkresiduals(fit.ets)


## STLF (Híbrido STL + Modelo)
# Decompõe a série e prevê a componente sazonal e não-sazonal separadamente
fit.stlf <- stlf(treino, method="arima", lambda=lambda.est) # Usa ARIMA na tendência
# plot(fit.stlf)




### Identificação do modelo MANUALMENTE
##  GUIA DE ANÁLISE DE SÉRIES TEMPORAIS COM ACF, PACF E ARIMA/SARIMA

# 1. Análise do gráfico da série temporal (dados.ts)
# - Verificar se há tendência → se sim, a série não é estacionária.
# - Verificar se há variação crescente/decrescente → pode indicar heterocedasticidade.
# - Verificar padrões repetitivos → indica sazonalidade.
# - Se necessário, aplicar transformação Box-Cox para estabilizar variância.
# - Aplicar diferenciação (diff) para remover tendência e/ou sazonalidade.

# 2. Análise do gráfico ACF (Autocorrelation Function)
# - Decaimento lento → série não estacionária.
# - Corte abrupto após lag k → sugere componente MA(q = k).
# - Picos em múltiplos de 12 (mensal) ou 4 (trimestral) → indica sazonalidade → considerar SARIMA.

# 3. Análise do gráfico PACF (Partial Autocorrelation Function)
# - Corte abrupto após lag k → sugere componente AR(p = k).
# - Picos em lags sazonais (ex: 12, 24) → componente sazonal AR(P).

# 4. Determinação dos parâmetros do modelo ARIMA/SARIMA
# 1 - p: número de lags significativos na PACF → componente AR.
# 0 - d: número de diferenciações aplicadas para tornar a série estacionária.
# 2 - q: número de lags significativos na ACF → componente MA.
# 3 - P: número de lags sazonais significativos na PACF → AR sazonal.
# 1 - D: número de diferenciações sazonais aplicadas → sazonalidade.
# 3 - Q: número de lags sazonais significativos na ACF → MA sazonal.
# 12 - s: periodicidade da série (ex: 12 para mensal, 4 para trimestral).

# Exemplo: Se a série tem tendência, sazonalidade mensal, PACF corta em lag 1 e 
# 12, ACF corta em lag 1 e 12
# → Modelo sugerido: SARIMA(1,1,1)(1,1,1)[12]

# 5. Validação do modelo
# - Ajustar o modelo com os parâmetros identificados.
# - Verificar os resíduos: devem parecer ruído branco (sem autocorrelação).
# - Usar ACF/PACF dos resíduos e teste de Ljung-Box.
# - Comparar modelos com AIC, BIC, RMSE para escolher o melhor.




### Estimar os parâmetros
## ARIMA (p, d, q)
fit1 <- Arima(treino, order=c(p,d,q), include.mean=TRUE, method="ML")
# ML - Máxima-Verossimilhança (usado por defeito)
# CSS - Mínimos Quadrados (conditional sum of squares)
# method = c("CSS-ML", "ML", "CSS")
# order - (p, d, q), order 1,0,0 = AR(1) por exemplo
fit1
confint(fit1)
# Obter a constante do modelo
# Calcular apenas para AR(1) por exemplo
#const<-mean(dados.ts)*(1-coef(fit1)[1])
#const

# CASO EM CONFINT OBTENHA PARÂMETROS NÃO SIGNIFICATIVOS
fit2 <- Arima(treino, order=c(p,d,q), include.mean=TRUE,
              transform.pars=F,
              fixed=c(NA,NA,NA,0,NA,NA,NA, ...))
# Remover 1 a 1 pela ordem mais alta/ordem superior os não significativos até 
# que sejam todos significativos, os restantes mudamos de NA para 0.

# Se der erro significa que temos um modelo muito ambicioso, ou seja, ordem
# demasiado alta.

# Após serem todos significativos
checkresiduals(fit2) # Verificar o teste de Ljung-Box
# Se o teste for significativo então NÃO é um bom modelo
# Caso não passe no teste podemos evitar remover algumas ordens inferiores e 
# correr o teste novamente ou aumentar o lag, 12 -> 15, por exemplo.
# Caso mesmo assim o teste obtenha valor p baixo passar de modelo AR -> ARMA.
# Aumentar o MA progressivamente até que passe em Ljung-Box.
# Caso passe no teste
# Portanto, podemos admitir que até lag ... todas as correlações são nulas,
# ou seja, os resíduos são não-correlacionados.

# Outra alternativa: Colocar a sazonalidade no modelo (Neste caso no Q)
## SARIMA (p, d, q)x(P, D, Q)[s]     s = period
fit3<-Arima(treino, order=c(p,d,q),
            seas=list(order=c(P,D,Q),period=12),
            include.mean=TRUE, transform.pars=FALSE,
            fixed=c(0,0,0,0,0,NA,NA,NA,...))
## Caso se pretenda usar a transformação de BoxCox no modelo
fit3<-Arima(treino, order=c(p,d,q),
            seas=list(order=c(P,D,Q),period=12),
            include.drift=TRUE,
            lambda=lambda.est) #<----

confint(fit3)
checkresiduals(fit3$residuals)
# Valor p > 0.05 -> Bom Modelo (Correlações Nulas)

t.test(fit3$residuals)
# Valor p > 0.05 -> Resíduos de Média Nula

# Caso passe em checkresiduals e t.test
# Então os resíduos são ruído branco por terem correlação e média nulas.

# CASO RESULTE
# Experimentar a sazonalidade no AR (No P)
fit4<-Arima(treino, order=c(p,d,q),
            seas=list(order=c(P,D,Q),period=12),
            include.mean=TRUE, transform.pars=FALSE,
            fixed=c(0,0,0,0,NA,NA,NA,NA))
confint(fit4)
checkresiduals(fit3$residuals)
# Valor p > 0.05 -> Bom Modelo (Correlações Nulas)
t.test(fit3$residuals)
# Valor p > 0.05 -> Resíduos de Média Nula
# Comparar valores p entre fit3 e fit4




### Validar o modelo
## Análise dos resíduos
# Testamos a Não-Correlação (Teste de Ruído Branco)
tsdisplay(fit4$residuals)
# FAC/FACP dos resíduos
checkresiduals(fit4)
# Teste de Ljung-Box (H0: Ruído Branco). Se p-value > 0.05, modelo adequado.
# Valor p > 0.05, não rejeitamos H0, podemos admitir não-correlação até ordem x...
# Se o gráfico dos dados parecer irregular, então testamos a normalidade também.
# Testamos a Normalidade
shapiro.test(fit4$residuals) # Valor p > 0.05
lillie.test(fit4$residuals) # Valor p > 0.05
# Se não rejeitarmos a normalidade em pelo menos 1 dos testes então admitimos a normalidade.
# Se rejeitarmos a normalidade nos dois testes, então não podemos admitir a normalidade.

# Teste da Média Nula
# O teste t (média nula) é considerado robusto se a amostra (n) for 
# suficientemente grande, mesmo que a normalidade seja rejeitada.
t.test(fit4$residuals)
# H0: média nula (µϵ = 0). Se p-value > 0.05, média é nula.
# Valor p > 0.05, não rejeitamos H0
# Então admitimos a média nula
# Por terem média nula e correlação nula, dizemos que os resíduos são ruído branco

# Testes de Aleatoriedade (i.i.d.)
# Realizar para todos os modelos candidatos
difference.sign.test(fit4$residuals) # Valor p > 0.05
# H1: Não aleatoriedade.
turning.point.test(fit4$residuals) # Valor p > 0.05
# H1: Não aleatoriedade.
rank.test(fit4$residuals) # Valor p > 0.05
# H1: Têm tendência/padrão.

# Avaliar o ajustamento do modelo
plot(treino)
lines(fitted(fit3), col="red")
lines(fitted(fit4), col="green")
# Verificamos se o modelo parece acompanhar bem os dados.

# Comparação de Modelos (Escolher o modelo com menores erros de ajustamento)
# Comparar agora ajustamento e métricas entre cada modelo para verificar com 
# qual prosseguir.
accuracy(fit3)
accuracy(fit4)
# x% dos dados estão incorretamente ajustados pelo MAPE.
# Portanto, cerca de x% dos dados que sofrem predição errada.

fit3
fit4
resultados <- data.frame(Model = c("Modelo 3", "Modelo 4"),
                         AIC = c(fit3$aic, fit4$aic),
                         BIC = c(fit3$bic, fit4$bic),
                         AICc = c(fit3$aicc, fit4$aicc))
resultados
# Comparar AIC, BIC, AICc entre modelos candidatos, sendo AIC e AICc preferível
# caso BIC influencie para uma conclusão diferente.
# Escolher o que obtiveram valores inferiores.




### Previsão
tamanho_prev <- length(teste) # Ou definir manualmente

plot(forecast(fit4, h=tamanho_prev))
# Bandas escuras -> intervalo de confiança a 80%
# Bandas claras -> intervalo de confiança a 95%
accuracy(forecast(fit4, h=tamanho_prev), teste)
# Verificamos se a predição do modelo é boa a partir da % de dados mal previstos

# Adicionamos a vermelho a linha dos dados reais/teste:
lines(teste, col="red")
# E adicionamos também o modelo de treino no gráfico, a cor azul:
lines(fitted(fit4), col="blue")










# ----------------------------------------------------
###### SCRIPT UNIVERSAL - PARTE 1


### Carregar bibliotecas
#   ...
library(stats)      # Funções base de séries temporais
library(forecast)   # tsdisplay(), Arima(), ndiffs(), forecast(), checkresiduals()
library(tseries)    # adf.test()
library(imputeTS)   # na_kalman(), ggplot_na_distribution(), statsNA()
library(randtests)  # difference.sign.test(), turning.point.test(), rank.test()
library(nortest)    # shapiro.test(), lillie.test()
# Opcional:
library(fBasics)   # basicStats()
library(dplyr)     # glimpse()
library(TSstudio)   # Visualização interativa e avançada
library(tsibble)    # Estruturas de dados temporais modernas
library(feasts)     # Feature extraction e estatísticas (STL robusto)
library(fable)      # Necessário para a função model() usada com feasts

#ts_info(dados.ts)  # Mostra informações básicas da série temporal: frequência, início, fim, número de observações e periodicidade
#ts_plot(dados.ts, 
#        title="Série temporal original", 
#        Ytitle="Valores", 
#        slider=TRUE)  # Cria um gráfico interativo da série, permitindo explorar visualmente tendência, sazonalidade e zoom temporal
#ts_seasonal(dados.ts, type="all")  # Analisa a sazonalidade, mostra padrões mensais/trimestrais recorrentes ao longo dos anos
#ts_heatmap(dados.ts)  # Exibe um heatmap que destaca visualmente períodos de maior e menor valor em cada ano/mês
#ts_cor(dados.ts, lag.max=40)  # Mostra a função de autocorrelação (ACF) de forma visual, ajudando a identificar dependências temporais e possíveis ordens MA.   
#ts_lags(dados.ts)  # Exibe diagramas de dispersão com diferentes defasagens (lags), úteis para identificar relações lineares e possíveis componentes AR.




### Ler a base de dados
##        Para .csv
dados <- read.csv("...\\FICHEIRO.csv",
                  fileEncoding = "utf-8", 
                  stringsAsFactors = T, 
                  na.strings = "",
                  dec=",", sep=";")
# Verificar se possui header = T/F
## ou
## Para tabelas .txt
dados <- read.table("...\\FICHEIRO.txt")
## ou
## Para conjuntos de dados de bibliotecas do R
library(forecast)
data(gold) # Por exemplo




### Formato dos Dados
##        Identificação do formato
attach(dados) # Opcional
# Torna as colunas do objeto acessíveis diretamente pelo nome
# Por exemplo, em vez de usar dados$x, posso usar apenas x

plot(dados, main="TITULO", xlab="VARIÁVEL TEMPO", ylab="Y")
plot.ts(dados)
# Podemos verificar graficamente a existência de tendência/estacionaridade
# Assim como a sazonalidade

summary(dados)
head(dados)
str(dados) 
class(dados)
library(dplyr)
glimpse(dados)
# Verificar o formato dos dados

# Opcional:   dados2<-dados+25  # Adiciona 25 aos valores da base de dados inicial (caso necessário)
# O que não afeta as propriedades da série temporal
# Opcional:   (n<-dim(dados)[1]) # Dimensão dos dados (caso necessário)

## Transformar o formato dos dados em série temporal
dados.ts <- ts(dados, start=c(ANO_INICIAL,MES_INICIAL), frequency=FREQ)
# Para FREQ= ...
# 12 (mensal), 4 (trimestral), 1 (anual)
class(dados.ts)
# Verificar se o formato dos dados foi alterado corretamente

# length(dados.ts)  # N de observações
# start(dados.ts)   # Ano e mês inicial da série temporal
# end(dados.ts)     # Ano e mês final da série temporal

plot(dados.ts, main="SÉRIE TEMPORAL ORIGINAL", ylab="VALORES", xlab="TEMPO")




### Identificação de missings e outliers
## Identificar missings
library(imputeTS)
ggplot_na_distribution(dados.ts, title="Distribuição de Missings")
ggplot_na_distribution2(dados.ts, title="Distribuição de Missings")
# Verificamos a existência de missings graficamente.
statsNA(dados.ts)
summary(is.na(dados.ts))
# Contagem de missings

## Corrigir os missings
dados.ts2 <- na_kalman(dados.ts, model = "auto.arima", smooth = TRUE)
ggplot_na_distribution(dados.ts2, title="AA")
# Verificar se ainda existem missings
# Outro método possível:
dados.ts3 <- na_interpolation(dados.ts, option = "linear")
ggplot_na_distribution(dados.ts3, title="AA")
# Verificar se ainda existem missings
dados.ts4 <- na_interpolation(gold, option = "spline") # GERALMENTE O MELHOR PARA SÉRIES TEMPORAIS
ggplot_na_distribution(dados.ts4, title="AA")
# Verificar se ainda existem missings

statsNA(dados.ts4)
summary(is.na(dados.ts4))
# Confirmamos a não existência de missings numericamente.
# Avançamos com a data corrigida a partir da na_interpolation com opção spline (Por exemplo).
class(dados.ts4)
# Verificamos se o formato dos dados se mantém

plot(dados.ts4, col="red")
lines(dados.ts4, col="blue")

## Identificar outliers
tsoutliers(dados.ts4)
# Verificamos a existência de um outlier na posição ...
# Recomenda trocar o seu valor por ...

## Aplicar a correção sugerida ao outlier
dados.ts5<- tsclean(dados.ts4)
plot(dados.ts4)
lines(dados.ts5, col="red")
# Verificamos então o modelo com a sugestão alterada

## TRATAMENTO MANUAL DE OUTLIERS (SUBSTITUIÇÃO PELA MÉDIA SAZONAL)
#        SEM TENDÊNCIA/ESTACIONARIEDADE E COM SAZONALIDADE
# Substituir os valores problemáticos: (3), (16) & (37)     (Por exemplo)
p = 12
dados.ts3[3] = mean(dados.ts3[3+p], dados.ts3[3+2*p], dados.ts3[3+3*p])
dados.ts3[16] = mean(dados.ts3[16-p], dados.ts3[16+p], dados.ts3[16+2*p])
dados.ts3[37] = mean(dados.ts3[37-3*p], dados.ts3[37-2*p], dados.ts3[37-p], dados.ts3[37+p])
dados.ts3[3]
dados.ts3[16]
dados.ts3[37]
plot(dados.ts4, type="o", pch=16)
text(dados.ts4, labels=Data, cex=0.6, pos=4, col="red")
# Com os valores substituídos, podemos seguir...




### Verificar a estacionariedade

tsdisplay(dados.ts4) # teste KPSS (por defeito) OU PP
# No gráfico superior verificamos a existência de uma possível tendência
# Se existir tendência então não será estacionária
# Na FAC(ACF) verificamos a velocidade com que converge para a banda de confiança,
# Se for um decrescimento lento então não será estacionária.

library(tseries)
adf.test(dados.ts4)
# Devemos verificar a hipótese alternativa para tirar conclusões
# Se H1 = estacionária e p-value<0.05 então rejeitamos H0 e é, de facto, estacionária.

# Deve-se ter cuidado com este teste porque este teste só faz a estacionaridade
# baseado em uma autocorrelação de ordem 1, se tivemos autocorrelações mais fortes 
# o teste pode não as apanhar, ou seja, o teste é muito limitado

# Outro teste possível e mais aconselhado:
library(forecast)
# Verificar a estacionariedade da série
ndiffs(dados.ts4, method = "adf")
# Se = 0, o número de diffs é 0, logo a série é estacionária
# Se /= 0, a série não é estacionária

# Verificar a sazonalidade da série
nsdiffs(dados.ts4)
# Se = 0, a série não é sazonal
# Se /= 0, a série é sazonal

# Aplicar os diffs
#serie_estacionaria <- diff(TREINO, differences=1) # Diferenciação simples (d)
#serie_estacionaria <- diff(serie_estacionaria, lag=FREQUENCIA, differences=1) # Diferenciação sazonal (D)




### Dividir os dados para conjuntos de treino e teste
treino<-window(dados.ts4, end=c(ANO_FIM_TREINO, MES_FIM_TREINO))
teste<-window(dados.ts4, start=c(ANO_INICIO_TESTE, MES_INICIO_TESTE))
# A melhor proporção para o teste é 1 ou 2 vezes a frequência da série,
# ou seja, se a frequência for igual a 12 então o conjunto de teste terá
# os últimos 12/24 meses.
ndiffs(treino)
nsdiffs(treino)
# Voltar a testar ambos para verificar se se mantêm iguais.




### Transformações
## Série não estacionária -> estacionária
# Transformação de Box-Cox
(lambda.est<-BoxCox.lambda(treino, lower = -2, upper = 2))
# Se for próximo de 0 podemos futuramente usar a transformação logaritmo,
# ou seja, lambda.est = 0

dadosBoxCox <- BoxCox(treino, lambda.est)

# Diferenciação simples
#diff(dadosBoxCox, differences=1) # differences = 1 por defeito
#diff(treino, differences=1)

# Para não estacionária
tsdisplay(diff(treino, differences=1)) # Sem transformação de BoxCox
tsdisplay(diff(dadosBoxCox, differences=1)) # Com transformação de BoxCox
# Para não estacionária e sazonal
tsdisplay(diff(diff(treino, lag=12)))
# Se perdermos a sazonalidade após os diffs testamos a transformação BoxCox
tsdisplay(diff(diff(dadosBoxCox, lag=12)))

# Comprovar a estacionariedade das séries
ndiffs(diff(treino, differences=1))
ndiffs(diff(dadosBoxCox, differences=1))
# Se um dos dois for = 0, então é estacionário e usaremos essa transformações.
# Caso ambos forem = 0, então escolhemos o que pareça menos complexo, com menos parâmetros.

hist(diff(treino))
boxplot(diff(treino, differences=1))
# Verifica outliers

# Se após as transformações obtivermos ruído branco -> Voltar à série original




### Identificação do modelo
##  GUIA DE ANÁLISE DE SÉRIES TEMPORAIS COM ACF, PACF E ARIMA/SARIMA

# 1. Análise do gráfico da série temporal (dados.ts)
# - Verificar se há tendência → se sim, a série não é estacionária.
# - Verificar se há variação crescente/decrescente → pode indicar heterocedasticidade.
# - Verificar padrões repetitivos → indica sazonalidade.
# - Se necessário, aplicar transformação Box-Cox para estabilizar variância.
# - Aplicar diferenciação (diff) para remover tendência e/ou sazonalidade.

# 2. Análise do gráfico ACF (Autocorrelation Function)
# - Decaimento lento → série não estacionária.
# - Corte abrupto após lag k → sugere componente MA(q = k).
# - Picos em múltiplos de 12 (mensal) ou 4 (trimestral) → indica sazonalidade → considerar SARIMA.

# 3. Análise do gráfico PACF (Partial Autocorrelation Function)
# - Corte abrupto após lag k → sugere componente AR(p = k).
# - Picos em lags sazonais (ex: 12, 24) → componente sazonal AR(P).

# 4. Determinação dos parâmetros do modelo ARIMA/SARIMA
# 1 - p: número de lags significativos na PACF → componente AR.
# 0 - d: número de diferenciações aplicadas para tornar a série estacionária.
# 2 - q: número de lags significativos na ACF → componente MA.
# 3 - P: número de lags sazonais significativos na PACF → AR sazonal.
# 1 - D: número de diferenciações sazonais aplicadas → sazonalidade.
# 3 - Q: número de lags sazonais significativos na ACF → MA sazonal.
# 12 - s: periodicidade da série (ex: 12 para mensal, 4 para trimestral).

# Exemplo: Se a série tem tendência, sazonalidade mensal, PACF corta em lag 1 e 
# 12, ACF corta em lag 1 e 12
# → Modelo sugerido: SARIMA(1,1,1)(1,1,1)[12]

# 5. Validação do modelo
# - Ajustar o modelo com os parâmetros identificados.
# - Verificar os resíduos: devem parecer ruído branco (sem autocorrelação).
# - Usar ACF/PACF dos resíduos e teste de Ljung-Box.
# - Comparar modelos com AIC, BIC, RMSE para escolher o melhor.




### Estimar os parâmetros
## ARIMA (p, d, q)
fit1 <- Arima(treino, order=c(p,d,q), include.mean=TRUE, method="ML")
# ML - Máxima-Verossimilhança (usado por defeito)
# CSS - Mínimos Quadrados (conditional sum of squares)
# method = c("CSS-ML", "ML", "CSS")
# order - (p, d, q), order 1,0,0 = AR(1) por exemplo
fit1
confint(fit1)
# Obter a constante do modelo
# Calcular apenas para AR(1) por exemplo
#const<-mean(dados.ts)*(1-coef(fit1)[1])
#const

# CASO EM CONFINT OBTENHA PARÂMETROS NÃO SIGNIFICATIVOS
fit2 <- Arima(treino, order=c(p,d,q), include.mean=TRUE,
              transform.pars=F,
              fixed=c(NA,NA,NA,0,NA,NA,NA, ...))
# Remover 1 a 1 pela ordem mais alta/ordem superior os não significativos até 
# que sejam todos significativos, os restantes mudamos de NA para 0.

# Se der erro significa que temos um modelo muito ambicioso, ou seja, ordem
# demasiado alta.

# Após serem todos significativos
checkresiduals(fit2) # Verificar o teste de Ljung-Box
# Se o teste for significativo então NÃO é um bom modelo
# Caso não passe no teste podemos evitar remover algumas ordens inferiores e 
# correr o teste novamente ou aumentar o lag, 12 -> 15, por exemplo.
# Caso mesmo assim o teste obtenha valor p baixo passar de modelo AR -> ARMA.
# Aumentar o MA progressivamente até que passe em Ljung-Box.
# Caso passe no teste
# Portanto, podemos admitir que até lag ... todas as correlações são nulas,
# ou seja, os resíduos são não-correlacionados.

# Outra alternativa: Colocar a sazonalidade no modelo (Neste caso no Q)
## SARIMA (p, d, q)x(P, D, Q)[s]     s = period
fit3<-Arima(treino, order=c(p,d,q),
            seas=list(order=c(P,D,Q),period=12),
            include.mean=TRUE, transform.pars=FALSE,
            fixed=c(0,0,0,0,0,NA,NA,NA,...))
## Caso se pretenda usar a transformação de BoxCox no modelo
fit3<-Arima(treino, order=c(p,d,q),
            seas=list(order=c(P,D,Q),period=12),
            include.drift=TRUE,
            lambda=lambda.est) #<----

confint(fit3)
checkresiduals(fit3$residuals)
# Valor p > 0.05 -> Bom Modelo (Correlações Nulas)

t.test(fit3$residuals)
# Valor p > 0.05 -> Resíduos de Média Nula

# Caso passe em checkresiduals e t.test
# Então os resíduos são ruído branco por terem correlação e média nulas.

# CASO RESULTE
# Experimentar a sazonalidade no AR (No P)
fit4<-Arima(treino, order=c(p,d,q),
            seas=list(order=c(P,D,Q),period=12),
            include.mean=TRUE, transform.pars=FALSE,
            fixed=c(0,0,0,0,NA,NA,NA,NA))
confint(fit4)
checkresiduals(fit3$residuals)
# Valor p > 0.05 -> Bom Modelo (Correlações Nulas)
t.test(fit3$residuals)
# Valor p > 0.05 -> Resíduos de Média Nula
# Comparar valores p entre fit3 e fit4




### Validar o modelo
## Análise dos resíduos
# Testamos a Não-Correlação (Teste de Ruído Branco)
tsdisplay(fit4$residuals)
# FAC/FACP dos resíduos
checkresiduals(fit4)
# Teste de Ljung-Box (H0: Ruído Branco). Se p-value > 0.05, modelo adequado.
# Valor p > 0.05, não rejeitamos H0, podemos admitir não-correlação até ordem x...
# Se o gráfico dos dados parecer irregular, então testamos a normalidade também.
# Testamos a Normalidade
shapiro.test(fit4$residuals) # Valor p > 0.05
lillie.test(fit4$residuals) # Valor p > 0.05
# Se não rejeitarmos a normalidade em pelo menos 1 dos testes então admitimos a normalidade.
# Se rejeitarmos a normalidade nos dois testes, então não podemos admitir a normalidade.

# Teste da Média Nula
# O teste t (média nula) é considerado robusto se a amostra (n) for 
# suficientemente grande, mesmo que a normalidade seja rejeitada.
t.test(fit4$residuals)
# H0: média nula (µϵ = 0). Se p-value > 0.05, média é nula.
# Valor p > 0.05, não rejeitamos H0
# Então admitimos a média nula
# Por terem média nula e correlação nula, dizemos que os resíduos são ruído branco

# Testes de Aleatoriedade (i.i.d.)
# Realizar para todos os modelos candidatos
difference.sign.test(fit4$residuals) # Valor p > 0.05
# H1: Não aleatoriedade.
turning.point.test(fit4$residuals) # Valor p > 0.05
# H1: Não aleatoriedade.
rank.test(fit4$residuals) # Valor p > 0.05
# H1: Têm tendência/padrão.

# Avaliar o ajustamento do modelo
plot(treino)
lines(fitted(fit3), col="red")
lines(fitted(fit4), col="green")
# Verificamos se o modelo parece acompanhar bem os dados.

# Comparação de Modelos (Escolher o modelo com menores erros de ajustamento)
# Comparar agora ajustamento e métricas entre cada modelo para verificar com 
# qual prosseguir.
accuracy(fit3)
accuracy(fit4)
# x% dos dados estão incorretamente ajustados pelo MAPE.
# Portanto, cerca de x% dos dados que sofrem predição errada.

fit3
fit4
resultados <- data.frame(Model = c("Modelo 3", "Modelo 4"),
                         AIC = c(fit3$aic, fit4$aic),
                         BIC = c(fit3$bic, fit4$bic),
                         AICc = c(fit3$aicc, fit4$aicc))
resultados
# Comparar AIC, BIC, AICc entre modelos candidatos, sendo AIC e AICc preferível
# caso BIC influencie para uma conclusão diferente.
# Escolher o que obtiveram valores inferiores.




### Previsão
tamanho_prev <- length(teste) # Ou definir manualmente

plot(forecast(fit4, h=tamanho_prev))
# Bandas escuras -> intervalo de confiança a 80%
# Bandas claras -> intervalo de confiança a 95%
accuracy(forecast(fit4, h=tamanho_prev), teste)
# Verificamos se a predição do modelo é boa a partir da % de dados mal previstos

# Adicionamos a vermelho a linha dos dados reais/teste:
lines(teste, col="red")
# E adicionamos também o modelo de treino no gráfico, a cor azul:
lines(fitted(fit4), col="blue")