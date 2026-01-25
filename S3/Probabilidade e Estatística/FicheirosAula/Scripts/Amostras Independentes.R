# leitura da base de dados
dados <- read.csv2("AF.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                     sep=";", dec=",", header=T, encoding = "UTF-8")
summary(dados) # Síntese dos dados
names(dados) # designações das variáveis

# Calcular as médias, quartis e desvios padrão do retorno diário por criptomoeda #
with (dados, tapply(RD,  Moeda,  mean))
with (dados, tapply (RD,  Moeda,  sd))
with (dados, tapply(RD,  Moeda,  quantile , p=(c(0.25, 0.5, 0.75)), na.rm=TRUE))

# Obter as amostras a testar
Caminhantes<-subset(dados, praticacaminhadas=="Sim")
Homens<-subset(Caminhantes, sexo=="Masculino")
Mulheres<-subset(Caminhantes, sexo=="Feminino")
Bitcoin<-subset(dados, Moeda=="BTC")
Dogecoin<-subset(dados, Moeda=="DOGE")


#### Amostras Independentes #####

# Normalidade
shapiro.test(Bitcoin$RD) # teste de Shapiro-Wilk aos valores da amostra do retorno diário da Bitcoin
shapiro.test(Dogecoin$RD) # teste de Shapiro-Wilk aos valores da amostra do retorno diário da Dogecoin
shapiro.test(Mulheres$idade)
shapiro.test(Homens$idade)
# Em caso de não rejeição...
# Igualdade de Variâncias
var.test(Bitcoin$RD, Dogecoin$RD) # teste F de igualdade de variâncias
var.test(Homens$idade, Mulheres$idade)

t.test(Homens$idade, Mulheres$idade, var.equal=TRUE, conf.level=0.95)

t.test(Bitcoin$RD, Dogecoin$RD, conf.level = 0.90) # teste de Welch de comparação de médias (bilateral) e intervalo de confiança para a diferença de rendimentos médios
t.test(Bitcoin$RD, Dogecoin$RD, var.equal=TRUE) # teste t (variâncias iguais)
t.test(Bitcoin$RD, Dogecoin$RD, mu=1, alternative="greater") # H0:μ1-μ2≤m vs H1:μ1-μ2>m

# Em caso de rejeição da normalidade...
# Wilcoxon-Mann_Whitney
wilcox.test(Bitcoin$RD, Dogecoin$RD, conf.int = TRUE, conf.level = 0.95)
wilcox.test(Bitcoin$RD, Dogecoin$RD, mu = 1, alternative = "greater")



