# ----------------------------------------------------
# SCRIPT DO MINI TRABALHO 1
# ------------------------------------------------------------------------------
#### Miguel Grilo     58387
#### MiniTrab1

### Carregar bibliotecas
#   ...
library(stats)      # Funções base de séries temporais
library(forecast)   # tsdisplay(), Arima(), ndiffs(), forecast(), checkresiduals()
library(tseries)    # adf.test()
library(imputeTS)   # na_kalman(), ggplot_na_distribution(), statsNA()
library(randtests)  # difference.sign.test(), turning.point.test(), rank.test()
library(nortest)    # shapiro.test(), lillie.test()
# Opcional:
# library(fBasics)   # basicStats() -> Não utilizado
# library(dplyr)     # glimpse()    -> Não utilizado
# library(TSstudio)                 -> Não utilizado




### Ler a Base de Dados
dados <- read.table("C:\\UNI-L58387-IACD\\S5 - MP\\Prática\\BasesDados\\MiniTrab1.txt", 
                    header = T, sep = ";", dec = ".", na.strings = "NA")




### Formato dos Dados
attach(dados)
# Torna as colunas do objeto acessíveis diretamente pelo nome
# Por exemplo, em vez de usar dados$cvd, posso usar apenas cvd

class(dados)
# Após separada a informação do .txt corretamente, estará em formato "data.frame"
# (como verificado) em 'class(dados)' então tem de ser agora transformado em 
# série temporal.
summary(dados)
# Após a análise do comando 'summary(dados)' verifica-se que tem início no
# mês 1 de 1987 e termina em no mês 12 de 2000.

## Transformar o formato dos dados em série temporal
dados.ts <- ts(cvd, start=c(1987,1), frequency=12)
# 12 (mensal), 4 (trimestral), 1 (anual)
class(dados.ts)
# Verifica-se que o formato dos dados foi alterado corretamente

length(dados.ts)  # N de observações
# 168
start(dados.ts)   # Ano e mês inicial da série temporal
# 1987    1
end(dados.ts)     # Ano e mês final da série temporal
# 2000    12

# Verifica-se que os dados se encontram com o início e final esperado,
# progressimos para a sua análise.

plot(dados.ts, main="Plot dados.ts", xlab="Variável Tempo", ylab="Variável em Estudo (cvd)")
# Podemos verificar graficamente a inexistência de tendência, no entanto 
# aparenta ser estacionária e sazonal.





### Identificação de missings e outliers
## Identificar missings
ggplot_na_distribution(dados.ts, title="Distribuição de Missings")
ggplot_na_distribution2(dados.ts, title="Distribuição de Missings")
# Verificamos a existência de missings graficamente no início e final
# da série temporal em estudo.
statsNA(dados.ts)
summary(is.na(dados.ts))
# Contagem de missings
# Verificamos numericamente a existência de 2 missings que serão então corrigidos.

## Corrigir os missings
# Método 1 (Kalman)
dados.ts2 <- na_kalman(dados.ts, model = "auto.arima", smooth = TRUE)
ggplot_na_distribution(dados.ts2, title="AA")
# Verificamos que já não existem missings.

# Método 2 (Interpolação com opção 'linear')
dados.ts3 <- na_interpolation(dados.ts, option = "linear")
ggplot_na_distribution(dados.ts3, title="AA")
# Verificamos que já não existem missings.

# Método 3 (Interpolação com opção 'spline')
dados.ts4 <- na_interpolation(dados.ts, option = "spline") # GERALMENTE O MELHOR PARA SÉRIES TEMPORAIS
ggplot_na_distribution(dados.ts4, title="AA")
# Verificamos que já não existem missings.

statsNA(dados.ts4)
summary(is.na(dados.ts4))
# Confirmamos a não existência de missings numericamente.
# Avançamos com os missings corrigidos a partir da na_interpolation com opção 
# 'spline', sendo este método geralmente o melhor para séries temporais.
class(dados.ts4)
# Verificamos que o formato dos dados se mantém.

plot(dados.ts, col="red")
lines(dados.ts4, col="blue")
# Observamos então os dados corrigidos graficamente.

## Identificar outliers
tsoutliers(dados.ts4)
# Verificamos a existência de outliers na posição:
# 14, 25, 26, 49, 84, 97, 132, 133, 156
# Recomenda trocar os seus valores por:
# 1579.135, 1772.950, 1447.882, 1685.776, 1672.185, 1592.890, 1492.897, 1626.327, 1709.595
# Respetivamente.
# Embora fosse possível aplicar a correção dos outliers manualmente dada a 
# sazonalidade, vamos corrigir automaticamente pelas sugestões apresentadas.

## Aplicar a correção sugerida ao outlier
dados.ts5<- tsclean(dados.ts4)
plot(dados.ts4)
lines(dados.ts5, col="red")
# Verificamos então o modelo com a sugestão alterada.





### Verificar a estacionariedade

tsdisplay(dados.ts5) # teste KPSS (por defeito) OU PP
# No gráfico superior não verificamos a existência de uma possível tendência,
# então em princípio será estacionária.
# Na FAC(ACF) verificamos a velocidade com que converge para a banda de 
# confiança e leva a crer que será de facto estacionária, porém avançamos para
# os testes formais para tirar conclusões.

# adf.test(dados.ts5)
# Se H1 = estacionária e p-value<0.05 então rejeitamos H0 e é, de facto, estacionária.
# Deve-se ter cuidado com este teste porque este teste só faz a estacionaridade
# baseado em uma autocorrelação de ordem 1, se tivemos autocorrelações mais 
# fortes o teste pode não as apanhar, ou seja, o teste é muito limitado.
# Sendo este o nosso caso, com uma ordem superior a 1, avançamos então para
# o teste recomendado, não sendo tão limitado quanto adf.test().

# Verificar a estacionariedade da série
ndiffs(dados.ts5)
# 0
# Como é = 0, o número de diffs é 0, logo a série é estacionária.

# Verificar a sazonalidade da série
nsdiffs(dados.ts5)
# 1
# Como é /= 0, a série é sazonal.

# Aplicar o diff correspondente à sazonalidade
# serie_sazonal <- diff(dados.ts5, lag=12) # Diferenciação sazonal (D)
# tsdisplay(serie_sazonal)





### Dividir os dados para conjuntos de treino e teste
treino<-window(dados.ts5, end=c(1999, 12))
teste<-window(dados.ts5, start=c(2000, 1))
# A melhor proporção para o teste é 1, 2 ou até mesmo 3 vezes a frequência da 
# série, ou seja, como a frequência é igual a 12 então o conjunto de teste terá
# os últimos 12, 24 ou 36 meses, para esta série temporal, captando 13 anos,
# teria como objetivo usar para teste os últimos 24 meses, porém, neste cenário
# abdicaria da estacionariedade, então avançamos com um conjunto de teste
# relativo aos últimos 12 meses da série temporal apenas.

ndiffs(treino)
# 0
nsdiffs(treino)
# 1
# Voltamos então a testar ambos e verificamos que a estacionariedade e a
# sazonalidade se mantêm.





### Transformações
# Transformação de Box-Cox
(lambda.est<-BoxCox.lambda(treino, lower = -2, upper = 2))
# -0.4207947
# É relativamente próximo de 0, então podemos futuramente usar a transformação 
# logaritmo, ou seja, lambda.est = 0, caso necessário ??

dadosBoxCox <- BoxCox(treino, lambda.est)
# Diferenciação simples
diff(dadosBoxCox, lag=12)
diff(treino, lag=12)

hist(diff(treino, lag=12))
# Verificamos uma distribuição aproximadamente normal.
boxplot(diff(treino, lag=12))
# Verifica outliers.





### Identificação do modelo
tsdisplay(diff(treino, lag=12))

# Análise do gráfico FAC (ACF)
# Corte abrupto após lag 1 -> sugere componente MA(q = 1?).
# Picos em múltiplos de 12 (mensal) -> indica sazonalidade -> considerar SARIMA.

# Análise do gráfico FACP (PACF)
# Corte abrupto após lag 2 -> sugere componente AR(p = 2).
# - Picos em lags sazonais (12, 24) -> componente sazonal AR(P = 2).

# Determinação dos parâmetros do modelo SARIMA
# D: número de diferenciações sazonais aplicadas = 1
# Q: número de lags sazonais significativos na ACF = 2
# s: 12





### Estimar os parâmetros
fit1<-Arima(treino, order=c(2,0,1),
            seas=list(order=c(2,1,2),period=12),
            include.mean=TRUE, transform.pars=T)
confint(fit1)
# Removemos sma2 não significativo
fit2<-Arima(treino, order=c(2,0,1),
            seas=list(order=c(2,1,2),period=12),
            include.mean=TRUE, transform.pars=F,
            fixed=c(NA,NA,NA,NA,NA,NA,0))
confint(fit2)
# Removemos sar2 não significativo
fit3<-Arima(treino, order=c(2,0,1),
            seas=list(order=c(2,1,2),period=12),
            include.mean=TRUE, transform.pars=F,
            fixed=c(NA,NA,NA,NA,0,NA,0))
confint(fit3)
# Removemos sar1 não significativo
fit4<-Arima(treino, order=c(2,0,1),
            seas=list(order=c(2,1,2),period=12),
            include.mean=TRUE, transform.pars=F,
            fixed=c(NA,NA,NA,0,0,NA,0))
confint(fit4)
# Removemos ma1 não significativo
fit5<-Arima(treino, order=c(2,0,1),
            seas=list(order=c(2,1,2),period=12),
            include.mean=TRUE, transform.pars=F,
            fixed=c(NA,NA,0,0,0,NA,0))
confint(fit5)
# Removemos ar2 não significativo
fit6<-Arima(treino, order=c(2,0,1),
            seas=list(order=c(2,1,2),period=12),
            include.mean=TRUE, transform.pars=F,
            fixed=c(NA,0,0,0,0,NA,0))
confint(fit6)
# Obtemos todas significativas, então chegámos ao modelo final possivelmente.

# Modelo final sem transformação
fit_final<-Arima(treino, order=c(1,0,0),
                 seas=list(order=c(0,1,1),period=12),
                 include.mean=TRUE, transform.pars=T)
confint(fit_final)

# Modelo final com transformação BoxCox com lambda = 0, ou seja, transformação logaritmo
# lambda.est<-0
fit_final2<-Arima(treino, order=c(1,0,0),
                  seas=list(order=c(0,1,1),period=12),
                  lambda=lambda.est)



### Validar o modelo
## Análise dos resíduos
# Testamos a Não-Correlação (Teste de Ruído Branco)
tsdisplay(fit_final$residuals)
tsdisplay(fit_final2$residuals)
# No modelo fit_final2 verificamos que os resíduos são, de facto, ruído branco,
# enquanto no fit_final rejeitamos essa opção.
checkresiduals(fit_final2)
# Teste de Ljung-Box (H0: Ruído Branco). Se p-value > 0.05, modelo adequado.
# Valor p = 0.6332 > 0.05, não rejeitamos H0, podemos admitir não-correlação até ordem 22
# O gráfico dos dados parece irregular, então testamos a normalidade também.
# Testamos a Normalidade
shapiro.test(fit_final2$residuals) # Valor p = 0.02065 < 0.05 # Rejeitamos Normalidade
lillie.test(fit_final2$residuals) # Valor p = 0.07498 > 0.05 # Não rejeitamos Normalidade
# Como não rejeitamos a normalidade em pelo menos 1 dos testes então admitimos a normalidade.





# Teste da Média Nula
# O teste t (média nula) é considerado robusto se a amostra (n) for 
# suficientemente grande, mesmo que a normalidade seja rejeitada.
t.test(fit_final2$residuals)
# H0: média nula (µϵ = 0). Se p-value > 0.05, média é nula.
# Valor p = 0.1622 > 0.05, não rejeitamos H0
# Então admitimos a média nula
# Por terem média nula e correlação nula, dizemos que os resíduos são ruído branco

# Testes de Aleatoriedade (i.i.d.)
# Realizar para todos os modelos candidatos
difference.sign.test(fit_final2$residuals) # Valor p = 0.6784 > 0.05
# H1: Não aleatoriedade. Rejeitamos a não aleatoriedade.
turning.point.test(fit_final2$residuals) # Valor p = 0.7502 > 0.05
# H1: Não aleatoriedade. Rejeitamos a não aleatoriedade.
rank.test(fit_final2$residuals) # Valor p = 0.6746 > 0.05
# H1: Têm tendência/padrão. Rejeitamos a exitência de padrão/tendência.

# Avaliar o ajustamento do modelo
plot(fit_final2)
lines(fitted(fit_final2), col="red")
# Verificamos que o modelo parece acompanhar bem os dados.

accuracy(fit_final2)
# 3.76% dos dados estão incorretamente ajustados pelo MAPE.
fit_final2





### Previsão
tamanho_prev <- length(teste) # Ou definir manualmente

plot(forecast(fit_final2, h=tamanho_prev))
# Bandas escuras -> intervalo de confiança a 80%
# Bandas claras -> intervalo de confiança a 95%
accuracy(forecast(fit_final2, h=tamanho_prev), teste)
# Verificamos se a predição do modelo é boa a partir da % de dados mal previstos
# 3.85% dos dados estão incorretamente ajustados pelo MAPE no conjunto de teste.
# Podemos admitir um bom modelo.


# Adicionamos a vermelho a linha dos dados reais/teste:
lines(teste, col="red")
# E adicionamos também o modelo de treino no gráfico, a cor azul:
lines(fitted(fit_final2), col="blue")