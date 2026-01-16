#### TPC3 ####
#### Miguel Grilo   58387
#### Jorge Couto    58656

#### EXERCÍCIO 1

library(irr)

# 3 Pessoas avaliam 5 entrevistas de Emprego. #
# Verificamos a medida de concordância entre os três #
datac<-data.frame(
  entrevistas=c("A", "B", "C", "D", "E"),
  pessoax=c(1, 2, 3, 4, 5),
  pessoay=c(2, 1, 3, 5, 4),
  pessoaz=c(1, 2, 4, 5, 4)
)
# 3 pessoas avaliam as entrevistas A, B, C, D e E, ordenando-as de 1 a 5.
datac
kendall(datac[,2:4], correct=TRUE) # Coeficiente de concordância Kendall W 

# Temos um alto nível de concordância (Wt = 0.887)
# Valor p = 0.0309 < 0.05, então podemos rejeitar H0
# Logo, podemos admitir que existe concordância entre os três entrevistadores


### EXERCÍCIO 2

library(tidyverse)
library(aod)
library(gtsummary)
library(ResourceSelection)
library(pROC)
library(ggplot2)
library(mfp)
library(broom)
library(Epi)
library(sjPlot)
library(performance)

# Carregar base de dados
IFEC <- read.csv2("IFEC.csv", stringsAsFactors = TRUE, na.strings = c("NULL", "", "NA"),
                  dec = ",", sep=",", header = TRUE, encoding = "UTF-8")

# Carregamos os dados do ficheiro IFEC
glimpse(IFEC) # Visualização inicial dos dados e dos tipos
# Temos Existencia_de_irmãos, Experiência_conjugal, Sexo,
# Horas_Trabalho_Semana, Nivel_de_Escolaridade e resp
# como variáveis categóricas, sendo resp a variável resposta

# Id, IdadeIndiv e Tamanho_do_Agregado são lidas como variáveis inteiras
summary(IFEC)
# Temos vários dados omissos na nossa variável Horas_Trabalho_Semana
# E a variável Tamanho_do_Agregado, que deveria ser categórica, é lida como contínua

# Modelo nulo (apenas intercepto)
mod0 <- glm(resp ~ 1, family = binomial(link = "logit"), data = IFEC)
summary(mod0)

### Passo 1 - Ajustar modelos simples e comparar com o modelo nulo
# Como temos poucas variáveis (menos de 20), usamos alfa = 0.20 para a comparação.

# Sexo
mod1 <- glm(resp ~ Sexo, family = binomial(link = "logit"), data = IFEC)
summary(mod1)
# Estimativa = 0.02243
anova(mod0, mod1, test = "Chisq")
# Valor p = 0.811 > 0.20, não se rejeita H0.
# Podemos ignorar a variável por agora.

# Existencia_de_irmãos
mod2 <- glm(resp ~ Existencia_de_irmãos, family=binomial(link = "logit"), data= IFEC)
summary(mod2)
# Estimativa = -0.2746
anova(mod0, mod2, test="Chisq")
# valor p = 0.04504 < 0.20, rejeita-se H0
# A variável impacta o modelo
exp(coef(mod2))
1/0.76
# Invertemos pela estimativa ser negativa
# Uma pessoa sem irmãos tem 1.3x mais chances de pensar
# vir a ter filhos do que alguém com irmãos

# Experiência_conjugal
mod3 <- glm(resp ~ Experiência_conjugal, family = binomial(link = "logit"), data=IFEC)
summary(mod3)
# Estimativa = 3.80
anova(mod0, mod3, test="Chisq")
# valor p < 0.001, rejeita-se H0
# A variável impacta o modelo
exp(coef(mod3))
# Uma pessoa sem coabitação tem 1.4x mais chances de pensar
# vir a ter filhos do que alguém com coabitação 

# IdadeIndiv
mod4 <- glm(resp ~ IdadeIndiv, family=binomial(link = "logit"), data=IFEC)
summary(mod4)
# Estimativa = -0.156
anova(mod0, mod4, test="Chisq")
# valor p < 0.001, rejeita-se H0
# A variável impacta o modelo
exp(coef(mod4))
1/0.86
# Por cada ano de idade a mais a chance de pensar vir a ter
# filhos do que alguém um ano mais novo é 1.16x superior

# Horas_Trabalho_Semana
# Criamos um subset sem NA's para trabalharmos nele:
IFEC1 <- subset(IFEC, !is.na(IFEC$Horas_Trabalho_Semana))
mod5 <- glm(resp ~ Horas_Trabalho_Semana, family=binomial(link = "logit"), data=IFEC1)
summary(mod5)
# Estimativa = 0.8335 (1ª Categoria)
# Somente a 1ª categoria (>=36) é significativa, podemos ignorar a outra
# Reconstruimos o modelo zero para o subset sem NA's:
mod01 <- glm(resp ~ 1, family = binomial(link = "logit"), data=IFEC1)
anova(mod01, mod5, test="Chisq")
# valor p = 0.002 < 0.20, rejeita-se H0
# A variável impacta o modelo
exp(coef(mod5))
# Uma pessoa com 36 ou mais horas de trabalho por semana têm
# 2.3x mais chances de pensar vir a ter filhos do que alguém
# com menos do que 16 horas de trabalho por semana

# Tamanho_do_Agregado
mod6 <- glm(resp ~ Tamanho_do_Agregado, family=binomial(link = "logit"), data=IFEC)
summary(mod6)
# Estimativa = -0.4938
anova(mod0, mod6, test="Chisq")
# Valor p = 0.2238 > 0.20, não se rejeita H0
# A variável não afeta o modelo nulo

# Nivel_de_Escolaridade
mod7 <- glm(resp ~ Nivel_de_Escolaridade, family=binomial(link = "logit"), data=IFEC)
summary(mod7)
# Estimativa = 0.9508 (1ª Categoria)
# Estimativa = 0.6188 (2ª Categoria)
# Ambos os níveis são significativos, podemos interpretar ambos
anova(mod0, mod7, test="Chisq")
# Valor p < 0.001, rejeita-se H0
# A variável (os seus dois níveis) afeta o modelo
exp(coef(mod7))
# Alguém com o secundário e pós-secundário têm 2.6x mais chances de pensar
# vir a ter filhos do que alguém com menor nivel de escolaridade. Por outro lado,
# Alguém com ensino superior têm 1.9x mais chances de pensar vir a ter
# filhos do que alguém com nivel de escolaridade inferior ao secundário

### Passo 2 - Ajustar modelo com todas as variáveis significativas no passo anterior

# Modelo com múltiplas variáveis
mod8 <- glm(resp ~ Existencia_de_irmãos + Experiência_conjugal + IdadeIndiv + 
              Horas_Trabalho_Semana + Nivel_de_Escolaridade, 
            family = binomial(link = "logit"), data = IFEC)
summary(mod8)
# Notamos que Existência_de_irmãos não é significativo:
# Então passemos para o passo 3.

### Passo 3 - Remover, por ordem decrescente, variáveis não significativas do passo 2

# Podemos e devemos retirar Existência de irmãos
mod9 <- glm(resp ~ Experiência_conjugal + IdadeIndiv + Horas_Trabalho_Semana + 
              Nivel_de_Escolaridade, family = binomial(link = "logit"), data = IFEC)
summary(mod9)

# Usar AF1 para comparar mod7 com mod8, para saber se existem diferenças
# Entre um modelo com a variável Existência de irmãos e sem ela
anova(mod8, mod9, test="Chisq")
# Valor p = 0.2811 > 0.05, logo não podemos rejeitar H0
# Então não existe diferença entre ter e não ter a variável no modelo

### Passo 4 - Incluir, por ordem crescente do valor p, as variáveis que
### ficaram de fora no passo 2. Ou seja: Sexo e Tamanho_do_Agregado.
### Seguindo por ordem crescente de valor p, começamos por Tamanho_do_Agregado
  
# Sexo
mod10 <- glm(resp ~ Experiência_conjugal + IdadeIndiv + Horas_Trabalho_Semana +
               Nivel_de_Escolaridade + Tamanho_do_Agregado, family = binomial(link = "logit"),
             data = IFEC)
summary(mod10)
# Tamanho_do_Agregado não é uma variável significativa neste modelo.
anova(mod9, mod10, test="Chisq")
# Valor p = 0.356 > 0.05, não se rejeita H0
# A variável tamanho do agregado não deve ser mantida no modelo completo.

# Sexo
mod11 <- glm(resp ~ Experiência_conjugal + IdadeIndiv + Horas_Trabalho_Semana +
               Nivel_de_Escolaridade + Sexo, 
             family = binomial(link = "logit"), data = IFEC)
summary(mod11)
# Sexo é uma variável significativa no modelo
anova(mod9, mod11, test="Chisq")
# Valor p < 0.001, logo podemos rejeitar H0
# A variável sexo é significativa no nosso modelo.
# Portanto, devemos considerar o modelo com ela.
# Assim, mod10 é o nosso modelo final.