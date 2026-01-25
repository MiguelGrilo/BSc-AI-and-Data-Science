# Miguel Grilo
# ----------------------------------------------------
###### SCRIPT UNIVERSAL - GLM

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