# leitura da base de dados
dados <- read.csv2("cripto.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                     sep=";", dec=",", header=T, encoding = "UTF-8")
summary(dados) # Síntese dos dados
names(dados) # designações das variáveis
# Calcular as médias e desvios padrão do retorno diário por criptomoeda #
with (dados, tapply(RD,  Moeda,  mean))
with (dados, tapply (RD,  Moeda,  sd))
# Obter as amostras a testar
Bitcoin<-subset(dados, Moeda=="BTC")
Dogecoin<-subset(dados, Moeda=="DOGE")
#### Amostras Independentes #####
# Normalidade
shapiro.test(Bitcoin$RD) # teste de Shapiro-Wilk aos valores da amostra do retorno diário da Bitcoin
shapiro.test(Dogecoin$RD) # teste de Shapiro-Wilk aos valores da amostra do retorno diário da Dogecoin
# Em caso de não rejeição...
# Igualdade de Variâncias
var.test(Bitcoin$RD, Dogecoin$RD) # teste F de igualdade de variâncias
t.test(Bitcoin$RD, Dogecoin$RD, conf.level = 0.90) # teste de Welch de comparação de médias (bilateral) e intervalo de confiança para a diferença de rendimentos médios
t.test(Bitcoin$RD, Dogecoin$RD, var.equal=TRUE) # teste t (variâncias iguais)
t.test(Bitcoin$RD, Dogecoin$RD, mu=0, alternative="greater") # H0:μ1-μ2≤m vs H1:μ1-μ2>m
# Em caso de rejeição da normalidade...
# Wilcoxon-Mann_Whitney
wilcox.test(Bitcoin$RD, Dogecoin$RD, conf.int = TRUE, conf.level = 0.95)
wilcox.test(Bitcoin$RD, Dogecoin$RD, mu = 0, alternative = "greater")