# ----------------------------------------------------
###### SCRIPT UNIVERSAL - PARTE 2

### Carregar Bibliotecas
library(dplyr)      # Manipulação de dados (mutate, recode, select)
library(AER)        # Teste de dispersão (dispersiontest)
library(DHARMa)     # Diagnóstico de resíduos simulados
library(MASS)       # Modelo Binomial Negativa (glm.nb)
library(ggplot2)    # Opcional para gráficos mais avançados




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
## ou
## Para .rds (Link ou ficheiro local)
dados <- readRDS("...\\FICHEIRO.rds")
# ou
dados <- url("URL_DO_FICHEIRO") %>%
  readRDS()




### Análise Exploratória e Pré-Processamento

attach(dados) # Opcional
# Torna as colunas do objeto acessíveis diretamente pelo nome
# Por exemplo, em vez de usar dados$x, posso usar apenas x

summary(dados)
head(dados)
names(dados)
str(dados)
glimpse(dados)
# Breve introdução das variáveis dos dados, incluindo o tipo de variável

# Co-variável ou Variável de Controlo é uma variável "secundária". Não temos 
# necessariamente interesse nela, mas sabemos que pode influenciar o resultado, 
# por isso, é incluída no modelo para que não mascare o efeito da variável 
# preditora principal. Serve para "limpar" o erro.



## Seleção das Variáveis de Interesse (Variável Resposta incluída)
dados <- dados %>% 
  select(VariavelX, VariavelY, VariavelZ, ...)

summary(dados)
head(dados)
names(dados)
str(dados)
glimpse(dados)


## Tratamento de Variáveis Categóricas
# Se uma variável for categórica mas estiver como 'chr' ou 'int', converter 
# para factor
dados$VariavelCat <- as.factor(dados$VariavelCat)

# Definir Categoria de Referência (Relevel)
# Importante para a interpretação dos coeficientes (comparação contra a 
# referência)
table(dados$Programa) 
# Verificar qual poderá ser a melhor referência, geralmente a com maior contagem
dados$Programa <- relevel(dados$Programa, ref="Geral")

# Recodificar níveis de fatores (se necessário agrupar ou renomear)
dados <- dados %>% 
  mutate(Variavel = recode(VariavelOriginal, 'Antigo' = 'Novo'))


## Verificação Inicial de Dispersão (Regra de Polegar)
# A distribuição de Poisson assume, teoricamente, que a Média = Variância
# No entanto, fica a nosso critério decidir se a razão é próxima o suficiente 
# de 1 para assumir igualdade
(mu <- mean(dados$VariavelResposta))
(var <- var(dados$VariavelResposta))
(razao <- var/mu)
# CRITÉRIO DE DECISÃO:
# - Se razão aprox. 1 (entre 0.5 e 1.5): Tentar Poisson.
# - Se razão > 1.5 
#   -> Indício de Sobredispersão (Overdispersion) 
#     -> Quasipoisson ou Binomial Negativa.


## Comparação Gráfica: Observados vs. Teóricos (Poisson)
# Verifica visualmente se a distribuição dos dados segue uma Poisson
# Em aula foi feito 'seq(0, 6)...'
plot(seq(0, max(dados$VariavelResposta)) - 0.05, prop.table(table(dados$VariavelResposta)),
     type="h", lwd=2, 
     ylab="Frequência Relativa", ylim=c(0,1),
     xlab="Contagem", main="Observados VS Poisson")
lines(seq(0, max(dados$VariavelResposta)) + 0.05, dpois(seq(0, max(dados$VariavelResposta)), mu),col="red", type="h", lwd=2)
legend(4, 1, c("Observado", "Poisson"), col=c("black", "red"), lty=1, lwd=2)




### Ajuste do Modelo (Model Fitting)
## ESTRATÉGIA HIERÁRQUICA:
# 1 - Tentar Poisson.
# 2 - Testar Sobredispersão.
# 3 - Se falhar, tentar Quasipoisson ou Binomial Negativa.


## MODELO 1: POISSON (Base)
modelo <- glm(VariavelResposta ~ VariavelX + VariavelY + VariavelZ, family=poisson, data=dados)
summary(modelo)
# ou
# modelo <- glm(VariavelResposta ~ ., family=poisson, data=dados)
# summary(modelo)




### Diagnóstico e Validação de Poisson

# É realizado imediatamente após a primeira modelação para podermos trocar de
# modelo imediatamente caso seja necessário sem perdas de tempo.

## Teste de Sobredispersão (AER)
# H0: Equidispersão (Var = Média). 
# Se p-value < 0.05, rejeita H0 (existe sobredispersão).
dispersiontest(modelo)
# SE dispersiontest der p < 0.05 ou a razão Deviance/Graus de Liberdade for 
# muito alta -> Mudar para Quasipoisson ou Binomial Negativa.




### Caso o Teste de Sobredispersão (AER) FALHE
## OPÇÃO A: QUASIPOISSON (Trata a dispersão mas mantém coeficientes 
##                        existem problemas de sobredispersão)
modelo_quasi <- glm(VariavelResposta ~ VariavelX + VariavelY + VariavelZ, family=quasipoisson, data=dados)


## OPÇÃO B: BINOMIAL NEGATIVA (Mais robusta para alta variância
##                             quando tudo falha)
modelo_nb <- glm.nb(VariavelResposta ~ VariavelX + VariavelY + VariavelZ, data=dados)




### Continuar a modelação a partir do modelo indicado iterativamente

# Seleção de Variáveis (Backward Elimination)
# Remover variáveis não significativas (p-value > 0.05) uma a uma, começando 
# pela maior.
# Esta regra não é fixa, caso consideremos a variável a ser removida importante
# o suficiente, esta pode ser deixada no modelo.

# Nota: Se for uma categoria de um fator e outras categorias forem 
# significativas, NÃO remover o fator.

# Exemplo: Remover 'VariavelX'
modelo2 <- glm(VariavelResposta ~ VariavelY + VariavelZ, family=poisson, data=dados)
summary(modelo2)
# ou
# modelo2 <- glm(VariavelResposta ~ . - VariavelX, family=poisson, data=dados)
# summary(modelo2)

# Se a última remoção for feita de uma variável muito próxima de 0.05 podemos
# testar sem e com a respetiva variável.

# Se a deviance dos resíduos for próxima dos degrees of freedom pode significar
# que obtemos um modelo razoável, mas não é regra.




### Análise de Resíduos (DHARMa)
## Após modelação
resid_sim <- simulateResiduals(modelo)
plot(resid_sim)
# Teste de dispersão alternativo do DHARMa
testDispersion(resid_sim) 

## VERIFICAR
# 1. KS test 
# (H0: Normalidade): p > 0.05 (Ideal).
# Resíduos Normais

# 2. Dispersion test 
# (H0: Sem dispersão): p > 0.05 (Ideal).
# Não dispersão dos resíduos

# 3. Outlier test 
# (H0: Sem outliers): p > 0.05 (Ideal).
# Não existência de outliers

# 4. Linhas ou curvas vermelhas no gráfico da direita indicam mau ajustamento (desvios).
# Exemplo
# A análise visual de resíduos vs. preditos não revela padrões de sobredispersão 
# ou falta de linearidade significativa.


## Análise de Outliers e Influentes (Distância de Cook)
# Identificar pontos que distorcem o modelo
boxplot(modelo2$residuals) # Apenas para análise rápida, Poisson terá vários outliers naturalmente

par(mfrow=c(2,3))
plot(modelo, which=1:6) # Painel clássico de diagnóstico
dev.off()

# Cálculo numérico de influentes
n <- nrow(dados)
threshold <- 4 / n # Regra comum

influentes <- which(cooks.distance(modelo) > threshold)
length(influentes) # Quantidade de dados acima do limite

# ou
val.cook<-cooks.distance(modelo5)
sum(val.cook > threshold) # 42 valores com distância de cook acima do threshold
# Outra opção
threshold.mean<-3*mean(val.cook)
sum(val.cook > threshold.mean) # 55 valores com distância de cook acima do threshold

# Se necessário, remover outliers extremos e reajustar (Voltar à modelação)
dados_clean <- dados[-which.max(cooks.distance(modelo)), ]




### Qualidade do Ajuste e Interpretação
## Deviance Explicada (R² para GLM)
# Quanto da variabilidade dos dados o modelo explica
dev_null <- modelo$null.deviance
dev_resid <- modelo$deviance

(dev_expl <- (dev_null - dev_resid) / dev_null)
# Interpretação: "O modelo explica X% da variabilidade da variável resposta."


## Interpretação dos Coeficientes (Exp(Beta))
(est<-cbind(Estimate = coef(modelo), confint(modelo)))
exp(est)
# ou
# beta <- coef(modelo)
# exp(beta)


# INTERPRETAÇÃO PRÁTICA:
# - Variáveis Contínuas: "Por cada unidade extra em X, a contagem esperada 
# multiplica-se por exp(beta)."
# - Variáveis Categóricas: "A categoria Y tem exp(beta) vezes as 
# chances/contagem da categoria de Referência."


## Variação Percentual (Percent Change)
# Fórmula: (exp(beta) - 1) * 100
(percent_change <- (exp(est)[,1] - 1) * 100)

# Interpretação: "Aumenta/Diminui X% em relação à referência."

# Exemplo de cálculo manual para interação ou combinação:
(exp(est[1,1] + est[2,1]) / exp(est[1,1])) * 100 - 100





### ============================================================================
### INTERPRETAÇÃO DOS RESULTADOS
### ============================================================================

# 1. SIGNIFICÂNCIA ESTATÍSTICA:
# Olhar para o Summary: Variáveis com p-value < 0.05 são estatisticamente 
# relevantes para explicar a variável resposta.
# Olhar para os confint: Variáveis que não contenham o o valor 1 no intervalo 
# são estatisticamente relevantes para explicar a variável resposta.

# 2. SENTIDO DO EFEITO:
# exp(beta) > 1 -> Efeito Positivo (Aumenta a contagem)
# exp(beta) < 1 -> Efeito Negativo (Diminui a contagem)

# 3. TEMPLATE DE INTERPRETAÇÃO:

# Para Variável Contínua:
# Por cada unidade adicional de [Variável], a contagem esperada de [Resposta] 
# aumenta/diminui em [Percent_Change]% (IRR = [exp(beta)]; IC95% [LI; LS]), 
# mantendo as restantes variáveis constantes.

# Para Variável Categórica:
# O grupo [Categoria X] apresenta uma contagem esperada de [Resposta] 
# [exp(beta)] vezes a do grupo de referência [Categoria Ref] (IC95% [LI; LS]), 
# o que corresponde a uma variação de [Percent_Change]%.

# 4. CONCLUSÃO DE AJUSTAMENTO:
# O modelo final explica [dev_expl * 100]% da variabilidade dos dados (Deviance Explicada). 
# Os testes de diagnóstico (DHARMa) confirmam que os pressupostos de [distribuição/dispersão] 
# foram respeitados, validando as inferências realizadas.

### ADICIONAR?
# Com um IC95% = [; ]


##  Regressão de Poisson (Contagens)
# • Distribuição: Y ∼ Poisson(λ).
# • Função de Ligação: Logaritmica (ln(λ) = Xβ).
# • Interpretação dos Coeficientes (β):
#   – Calcular a razão das taxas de incidência: IRR = exp(β).
#   – Se exp(β) = 1.08: ”Um aumento de 1 unidade em X aumenta a contagem esperada
#   em 8%”.
#   – Se exp(β) = 0.95: ”Um aumento de 1 unidade em X diminui a contagem esperada
#   em 5%”.
# • Sobredispersão: Se Var(Y) > E[Y], o modelo Poisson padrão subestima os erros
# padrão. Usar Quasi-Poisson ou Binomial Negativa.

## 3.2 Regressão Binomial / Logistica (Binária)
# Usada quando a resposta ´r binária (0 ou 1, Sucesso ou Fracasso).
# • Distribuiçãao: Y ∼ Binomial(n, p).
# • Função de Ligação: Logit (ln(p/1−p) = Xβ).
# • Interpretação dos Coeficientes:
#   – exp(β) representa o Odds Ratio (OR).
#   – Se exp(β) > 1: A probabilidade de sucesso aumenta com X.
#   – Se exp(β) < 1: A probabilidade de sucesso diminui com X.




### Previsão
## Previsão com Intervalos de Confiança
# O predict devolve valores na escala do link (log). É preciso usar exp() para 
# voltar à contagem.

prevs <- predict(object = modelo2, se.fit = TRUE)
head(prevs)

# Transformar fit e calcular intervalos
#prevs$fit <- exp(prevs$fit)
#prevs$se.fit <- exp(prevs$se.fit)

prevs$Previsto <- exp(prevs$fit)
prevs$LI <- exp(prevs$fit - 1.96*prevs$se.fit) # Limite Inferior
prevs$LS <- exp(prevs$fit + 1.96*prevs$se.fit) # Limite Superior

head(prevs)
View(prevs)
View(data.frame(prevs))

(est<-cbind(Estimate = coef(modelo11), confint(modelo11)))
exp(est)

### Gráfico de Previsões (Exemplo visual)
# GRÁFICO NÃO APRESENTADO EM AULA (NÃO SUBMETER??)
# Criar um dataframe com os valores reais e as previsões
dados_plot <- data.frame(Real = dados$VariavelResposta, 
                         Previsto = prevs$Previsto)

ggplot(dados_plot, aes(x = Previsto, y = Real)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") + # Linha de perfeição
  theme_minimal() +
  labs(title = "Valores Reais vs Previstos", x = "Previsão", y = "Observado")










# ----------------------------------------------------
###### SCRIPT UNIVERSAL - PARTE 1 ATUALIZADO
###### SÉRIES TEMPORAIS: ARIMA, SARIMA, ETS, TSLM, DECOMPOSIÇÃO

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










# ----------------------------------------------------
# SCRIPTS DE AULA - PARTE 2   (ORDENADOS POR SCRIPT)
# ------------------------------------------------------------------------------
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










# ----------------------------------------------------
# SCRIPTS DE AULA - PARTE 1  (ORDENADOS POR SCRIPT)
# ------------------------------------------------------------------------------
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