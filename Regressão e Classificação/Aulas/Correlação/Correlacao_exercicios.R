# ******************************************************************************
#                     Regressao e Classificacao 2024/25
#
# Exercicios: correlacao
#
# ******************************************************************************

# optativo: definir a diretoria atual como diretoria de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# ----------------------------- Ex. 1.1 ----------------------------------------

# ler dados
dados <- read.csv2("tempos.csv")
str(dados)
head(dados)

## a) ----

# versao mais simples
plot(dados$y ~ dados$x, 
     xlab="dias após o encaminhamento",
     ylab="dias após a consulta de referência", 
     main="Tempos de espera")

# igual a 
with(dados, plot(y ~ x, 
                 xlab="dias após o encaminhamento",
                 ylab="dias após a consulta de referência", 
                 main="Tempos de espera"))

# versao ggplot
library(ggplot2)
ggplot(dados, aes(x=x, y=y))+
  geom_point()+
  labs(x="\ndias após o encaminhamento", 
       y="dias após a consulta de referência\n", title="Tempos de espera")+
  theme_classic()

# versao ggplot com dplyr
library(dplyr)
# 1o converter data.frame em tibble
dados2 <- tibble(dados)
dados2 %>% ggplot(aes(x=x, y=y))+
  geom_point()+
  labs(x="\ndias após o encaminhamento", 
       y="dias após a consulta de referência\n", title="Tempos de espera")+
  theme_classic()



## b) ----

# versao simples
cor(dados$x, dados$y)
cor.test(dados$x, dados$y) # versao inferencial

# versao com dplyr: util para obter matriz de correlacao (util quando > 2 variaveis)
cor(dados2 %>% select(where(is.numeric)))



# ----------------------------- Ex. 1.2 ----------------------------------------

# ler dados
dados <- read.csv2("ML.csv")
head(dados)


## a) ----

# versao simples
with(dados, plot(Tempo ~ NumParametros, 
                 xlab="Num. de parametros",
                 ylab="Tempo de execução (s)"))

# versao ggplot
ggplot(dados, aes(x=NumParametros, y=Tempo))+
  geom_point()+
  labs(x="\nNum. de parametros", 
       y="Tempo de execução (s)")+
  theme_classic()


## b) ----

with(dados, cov(Tempo, NumParametros))


## c) ----

with(dados, cor(Tempo, NumParametros))


## d) ----

# criar variavel tempo em minutos
dados$TempoMin <- dados$Tempo/60

# covariancia
with(dados, cov(TempoMin, NumParametros))

# correlacao
with(dados, cor(TempoMin, NumParametros))



# ----------------------------- Ex. 1.2 ----------------------------------------

# ler dados
library(readxl)
dados <- read_excel("RDH2324.xlsx")
head(dados)


## a) ----

cor(dados %>% select(where(is.numeric)))

## b) ----

cor(dados %>% select(where(is.numeric)), method="spearman")

# grafico para ajudar a perceber as diferencas
plot(dados)

# ou via ggplot
library(GGally)
ggpairs(dados %>% select(where(is.numeric)))

