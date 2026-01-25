# leitura da base de dados
dados <- read.csv2("cripto.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                   sep=";", dec=",", header=T, encoding = "UTF-8")
summary(dados) # Síntese dos dados
names(dados) # designações das variáveis
View(dados)

# Obter a amostra a testar
Ethereum<-subset(dados, Moeda=="ETH")

# Calcular a média, quartis e desvio padrão do retorno diário para a Ethereum #
mean(Ethereum$RD, na.rm=TRUE)
sd(Ethereum$RD, na.rm=TRUE)
quantile (Ethereum$RD , p=c(0.25, 0.5, 0.75), na.rm=TRUE)
##IMPORTANTE COLOCAR ", na.rm=TRUE"
##REMOVE OS "NA"

#### Uma amostra ####
# Normalidade
shapiro.test(Ethereum$RD) # teste de Shapiro-Wilk aos valores da amostra do retorno diário da Ethereum

# Em caso de não rejeição...
t.test(Ethereum$RD, conf.level=0.90) # Intervalo de confiança a 90% para o rendimento médio da Ethereum
t.test(Ethereum$RD, mu=0, alternative="greater") # Teste unilateral direito do rendimento médio ser positivo 

# Em caso de rejeição da normalidade...
library(moments)
agostino.test(Ethereum$RD) # Teste à simetria
# Em caso de não rejeição...
wilcox.test (Ethereum$RD) # Teste de Wilcoxon
# Em caso de rejeição
library(BSDA)
SIGN.test(Ethereum$RD, conf.level=0.90) # Intervalo de confiança a 90% para o rendimento mediano da Ethereum
SIGN.test(Ethereum$RD, md=0, alternative="greater") # Teste unilateral direito do rendimento mediano ser positivo 

