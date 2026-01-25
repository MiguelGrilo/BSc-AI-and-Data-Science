#### Miguel Grilo     58387
#### MiniTrab2


#### EXERCÍCIO 1 ####

### Carregar Bibliotecas
library(dplyr)      # Manipulação de dados (mutate, recode, select)
library(AER)        # Teste de dispersão (dispersiontest)
library(DHARMa)     # Diagnóstico de resíduos simulados
library(MASS)       # Modelo Binomial Negativa (glm.nb)
library(ggplot2)    # Opcional para gráficos mais avançados




### Ler a base de dados
## Para conjuntos de dados de bibliotecas do R
library(AER)
data(DoctorVisits)




#### ALÍNEA A ####

### Análise Exploratória e Pré-Processamento

attach(DoctorVisits) # Opcional
# Torna as colunas do objeto acessíveis diretamente pelo nome
# Por exemplo, em vez de usar dados$x, posso usar apenas x

summary(DoctorVisits)
head(DoctorVisits)
names(DoctorVisits)
str(DoctorVisits)
glimpse(DoctorVisits)
# Verificamos que todas as variáveis categóricas já se encontram no tipo 'factor'
# então podemos prosseguir na análise.


## Seleção das Variáveis de Interesse (Variável Resposta incluída)
#dados <- dados %>% 
#  select()
## Tratamento de Variáveis Categóricas
# Se uma variável for categórica mas estiver como 'chr' ou 'int', converter 
# para factor


## Verificação Inicial de Dispersão (Regra de Polegar)
# A distribuição de Poisson assume, teoricamente, que a Média = Variância
# No entanto, fica a nosso critério decidir se a razão é próxima o suficiente 
# de 1 para assumir igualdade
(mu <- mean(visits))
# Média = 0.3017341
(var <- var(visits))
# Variância = 0.6370176
(razao <- var/mu)
# Razão = 2.111189

# CRITÉRIO DE DECISÃO:
# - Se razão aprox. 1 (entre 0.5 e 1.5): Tentar Poisson.
# - Se razão > 1.5 
#   -> Indício de Sobredispersão (Overdispersion) 
#     -> Quasipoisson ou Binomial Negativa.

# A razão é significativamente superior a 1.5, ou seja, a média é diferente
# da variância, logo existe evidência de sobredispersão dos dados e não podemos 
# avançar com Poisson, visto que este obriga que Média = Variância teoricamente.
# Certamente teremos de avançar para os métodos alternativos (Quasipoisson ou 
# Binomial Negativa) de modo a que a sobredispersão não seja um problema.

# Ainda assim, começamos por verificar o modelo de Poisson e realizar o teste 
# adequado, embora tenhamos de avançar para para os métodos alternativos 
# garantidamente.




### Ajuste do Modelo (Model Fitting)
## ESTRATÉGIA HIERÁRQUICA:
# 1 - Tentar Poisson.
# 2 - Testar Sobredispersão.
# 3 - Se falhar, tentar Quasipoisson ou Binomial Negativa.


## MODELO 1: POISSON (Base)
modelo <- glm(visits ~ ., family=poisson, data=DoctorVisits)
summary(modelo)




### Diagnóstico e Validação de Poisson

# É realizado imediatamente após a primeira modelação para podermos trocar de
# modelo imediatamente caso seja necessário sem perdas de tempo.

## Teste de Sobredispersão (AER)
# H0: Equidispersão (Var = Média). 
# Se p-value < 0.05, rejeita H0 (existe sobredispersão).
dispersiontest(modelo)
# p-value = 3.105e-11 < 0.05, logo rejeita H0 (existe sobredispersão).
# Já tinhamos verificado que seria necessário seguir um caminho alternativo, 
# mas ainda assim foi verificado estatisticamente.
# Assim, mudamos para Quasipoisson ou Binomial Negativa.




#### ALÍNEA B ####

### Caso o Teste de Sobredispersão (AER) FALHE
## OPÇÃO A: QUASIPOISSON (Trata a dispersão mas mantém coeficientes 
##                        existem problemas de sobredispersão)
modelo_quasi <- glm(visits ~ ., family=quasipoisson, data=DoctorVisits)


## OPÇÃO B: BINOMIAL NEGATIVA (Mais robusta para alta variância
##                             quando tudo falha)
modelo_nb <- glm.nb(visits ~ ., data=DoctorVisits)




### Continuar a modelação a partir do modelo indicado iterativamente
### Começando pela OPÇÃO A: QUASIPOISSON

# Seleção de Variáveis (Backward Elimination)
# Remover variáveis não significativas (p-value > 0.05) uma a uma, começando 
# pela maior.
# Esta regra não é fixa, caso consideremos a variável a ser removida importante
# o suficiente, esta pode ser deixada no modelo.

# Nota: Se for uma categoria de um fator e outras categorias forem 
# significativas, NÃO removemos o fator.

summary(modelo_quasi)
# As variáveis age, income, privateyes, freerepatyes, nchronicyes e lchronicyes
# não são significativas, porém privateyes, freerepatyes, nchronicyes e 
# lchronicyes são categorias de fatores, então não podem ser removidas,
# começamos então por remover age, que possui o maior valor p entre age e income.

# Remover age
modelo_quasi2 <- glm(visits ~ . -age, family=quasipoisson, data=DoctorVisits)
summary(modelo_quasi2)
# Após a remoção de age a variável income tornou-se significativa então chegámos
# ao modelo ajustado, tendo em conta que as variáveis não significativas são
# categorias de um factor, então não devem ser removidas.




### Realizando agora pela OPÇÃO B: BINOMIAL NEGATIVA

# Seleção de Variáveis (Backward Elimination)
# Remover variáveis não significativas (p-value > 0.05) uma a uma, começando 
# pela maior.
# Esta regra não é fixa, caso consideremos a variável a ser removida importante
# o suficiente, esta pode ser deixada no modelo.

# Nota: Se for uma categoria de um fator e outras categorias forem 
# significativas, NÃO removemos o fator.

summary(modelo_nb)
# As variáveis income, age, privateyes, freerepatyes, nchronicyes e lchronicyes
# não são significativas, porém privateyes, freerepatyes, nchronicyes e 
# lchronicyes são categorias de fatores, então não podem ser removidas,
# começamos então por remover income, que possui o maior valor p entre age e 
# income, realizando assim uma remoção diferente do método anterior.

# Remover income
modelo_nb2 <- glm(visits ~ . -income,data=DoctorVisits)
summary(modelo_nb2)
# Após a remoção de income a variável age tornou-se significativa então chegámos
# ao modelo ajustado, tendo em conta que as variáveis não significativas são
# categorias de um factor, então não devem ser removidas.




#### ALÍNEA C ####

### Começando pela OPÇÃO A: QUASIPOISSON

# Validar os modelos
par(mfrow = c(2,3))
plot(modelo_quasi2, which = 1:6)

dev.off()
# Por análise gráfica verificamos a existência de inúmeros outliers logo teremos 
# de prosseguir numa análise aprofundada aos mesmos.
# Além disso, verificamos claramente a não normalidade.

# Distância de Cook: Valores que influenciam
(n<-nrow(DoctorVisits))
(threshold<-4/n)
# threshold = 0.0007707129

val.cook<-cooks.distance(modelo_quasi2)

# Conta quantos ultrapassam o limite
sum(val.cook > threshold) # 307 valores com distância de cook acima do threshold

# Outra opção
threshold.mean<-3*mean(val.cook)
sum(val.cook > threshold.mean) # 284 valores com distância de cook acima do threshold

# Ou seja, qualquer uma das opções dá problemas enormes, por isso têm de ser 
# removidos.

# Remover todos os outliers
DoctorVisits2 <- DoctorVisits[ -which.max(round(cooks.distance(modelo_quasi2))), ]

modelo_quasi3 <- glm(visits ~ . -age,
               data = DoctorVisits2, family = "quasipoisson")
summary(modelo_quasi3)
# Obtemos então o modelo ajustado novamente e voltamos a analisar os resíduos.

par(mfrow=c(2,3))
plot(modelo_quasi4, which=1:6)
dev.off()

# A remoção automática não remove todos os outliers visto que seria perigoso 
# remover uma quantidade absurda de observações de uma vez, logo não podemos 
# continuar a partir daqui dado o tempo de trabalho necessário para a remoção
# dos outliers manualmente, assim, realizamos a validação para a binomial 
# negativa embora seja provável que obtenhamos o mesmo problema.




### Realizando agora pela OPÇÃO B: BINOMIAL NEGATIVA
### Análise de Resíduos (DHARMa)
## Após modelação
resid_sim_nb <- simulateResiduals(modelo_nb2)
plot(resid_sim_nb)
# Teste de dispersão alternativo do DHARMa
testDispersion(resid_sim_nb) 
# Verificamos que aparenta ter uma boa dispersão, no entanto, falha nos 
# restantes testes.

## VERIFICAR
# 1. KS test 
# (H0: Normalidade): p = 0
# Resíduos Não Normais

# 2. Dispersion test 
# (H0: Sem dispersão): p > 0.05 (Ideal).
# Não dispersão dos resíduos

# 3. Outlier test 
# (H0: Sem outliers): p = 0
# Existência de outliers

# 4. Linhas ou curvas vermelhas no gráfico da direita indicam mau ajustamento (desvios).
# A análise visual de resíduos vs. preditos revela padrões de sobredispersão 
# ou falta de linearidade significativa.


## Análise de Outliers e Influentes (Distância de Cook)
# Identificar pontos que distorcem o modelo
boxplot(modelo_nb2$residuals) # Apenas para análise rápida
# Verificamos que de facto tem inúmeros outliers

par(mfrow=c(2,3))
plot(modelo, which=1:6) # Painel clássico de diagnóstico
dev.off()
# Verificamos novamente por análise gráfica verificamos a existência de inúmeros 
# outliers logo teremos de prosseguir numa análise aprofundada aos mesmos.
# Além disso, verificamos claramente a não normalidade.

# Cálculo numérico de influentes
n <- nrow(DoctorVisits)
threshold <- 4 / n # Regra comum

val.cook<-cooks.distance(modelo_nb2)
sum(val.cook > threshold) # 294 valores com distância de cook acima do threshold
# Outra opção
threshold.mean<-3*mean(val.cook)
sum(val.cook > threshold.mean) # 233 valores com distância de cook acima do threshold

# Se necessário, remover outliers extremos e reajustar (Voltar à modelação)
DoctorVisits3 <- DoctorVisits[-which.max(cooks.distance(modelo_nb2)), ]

# Novamente, a remoção automática não remove todos os outliers visto que seria 
# perigoso remover uma quantidade absurda de observações de uma vez, logo não 
# podemos continuar a partir daqui dado o tempo de trabalho necessário para a 
# remoção dos outliers manualmente, assim, realizamos a validação para a 
# binomial negativa embora seja provável que obtenhamos o mesmo problema.

View(DoctorVisits)
# Após breve análise da base de dados verificamos uma enorme quantidade de 
# valores zero nas variáveis 'reduced' e 'health', o que leva a uma grande 
# quantidade de tempo para modelar correta e eficazmente, deste modo, seria
# necessário trabalhar nestes dados futuramente, de modo a reduzir 
# significativamente a quantidade de outliers manualmente, voltar a tentar
# validar os dados e posteriormente continuaria a ser necessário mais ajustes
# muito provavelmente, mas o principal método de corrigir todos estes
# problemas seria usar o modelo Zero-Inflation Poisson. 




#### ALÍNEA D ####

### Visto que nenhum dos modelos podem ser validados, prosseguimos com o modelo
### QUASIPOISSON de modo a permitir a interpretação dos coeficientes do modelo
### embora estes estejam incorretos e não possam ser tidos em consideração 
### para uma análise estatística real.

### Qualidade do Ajuste e Interpretação
## Deviance Explicada (R² para GLM)
# Quanto da variabilidade dos dados o modelo explica
dev_null <- modelo_quasi2$null.deviance
dev_resid <- modelo_quasi2$deviance

(dev_expl <- (dev_null - dev_resid) / dev_null)
# O modelo explica 22.2% da variabilidade da variável resposta, o que é
# péssimo, este modelo não poderia ser usado como objeto de estudo ou análise
# estatistica.


## Interpretação dos Coeficientes (Exp(Beta))
(est<-cbind(Estimate = coef(modelo_quasi2), confint(modelo_quasi2)))
exp(est)
# genderfemale, income, illness, reduced, health, privateyes e freepooryes não
# contêm 0 (link) ou 1 (response), logo não são significativas e não serão 
# interpretadas.

# Variáveis contínuas: income, illness, reduced, health
# Variáveis categóricas: gender, private, freepoor

# INTERPRETAÇÃO PRÁTICA:

# - Por cada unidade adicional de income, a contagem esperada de visits diminui 
# em 19% (IRR = [0.81]; IC95% [0.67; 0.98]), mantendo as restantes variáveis 
# constantes.

# - Por cada unidade adicional em illness, a contagem esperada de visits aumenta 
# em 21% (IRR = [1.21]; IC95% [1.16; 1.26]), mantendo as restantes variáveis 
# constantes.

# - Por cada unidade adicional de reduced, a contagem esperada de visits aumenta 
# em 14% (IRR = [1.14]; IC95% [1.12; 1.15]), mantendo as restantes variáveis 
# constantes.

# - Por cada unidade adicional de health, a contagem esperada de visits aumenta 
# em 3% (IRR = [1.03]; IC95% [1.01; 1.05]), mantendo as restantes variáveis 
# constantes.


(percent_change <- (exp(est)[,1] - 1) * 100)
# - A variável gender apresenta uma contagem esperada de female 1.176 vezes a da 
# categoria de referência male (IC95% [1.04; 1.34]), 
# o que corresponde a uma variação de 17.6%.

# - A variável private apresenta uma contagem esperada de yes 1.177 vezes a da
# categoria de referência no (IC95% [1.01; 1.37]), 
# o que corresponde a uma variação de 17.7%.

# - A variável freepoor apresenta uma contagem esperada de yes 0.64 vezes a da 
# categoria de referência no (IC95% [0.41; 0.94]), 
# o que corresponde a uma variação de 36.3%.




### Previsão
## Previsão com Intervalos de Confiança
# O predict devolve valores na escala do link (log). É preciso usar exp() para 
# voltar à contagem.
prevs <- predict(object = modelo_quasi2, se.fit = TRUE)
head(prevs)

# Transformar fit e calcular intervalos
#prevs$fit <- exp(prevs$fit)
#prevs$se.fit <- exp(prevs$se.fit)

prevs$Previsto <- exp(prevs$fit)
prevs$LI <- exp(prevs$fit - 1.96*prevs$se.fit) # Limite Inferior
prevs$LS <- exp(prevs$fit + 1.96*prevs$se.fit) # Limite Superior

View(prevs)
View(data.frame(prevs))




#### ALÍNEA E ####

# Devemos ponderar usar o Zero-Inflation Poisson para modelar dados que possuem
# um valor excessivo de contagens zero. Esta base de dados poderia usufruir 
# deste método para obter uma boa análise estatística, visto que tal como 
# foi verificado anteriormente, existe uma enorme quantidade de valores zero nas 
# variáveis 'reduced' e 'health', o que leva a uma grande quantidade de tempo 
# para modelar correta e eficazmente, deste modo, seria necessário trabalhar 
# nestes dados futuramente, de modo a reduzir significativamente a quantidade 
# de outliers manualmente, voltar a tentar validar os dados e posteriormente 
# continuaria a ser necessário mais ajustes muito provavelmente, mas o 
# principal método de corrigir todos estes problemas seria realmente usar o 
# modelo Zero-Inflation Poisson. 