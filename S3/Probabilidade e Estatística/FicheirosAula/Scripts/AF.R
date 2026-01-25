# leitura da base de dados
dados <- read.csv2("AF.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                   sep=";", dec=",", header=T, encoding = "UTF-8")
summary(dados) # Síntese dos dados
names(dados) # designações das variáveis
View(dados)

# Obter a amostra a testar
Caminhantes<-subset(dados, praticacaminhadas == "Sim")

#### Uma amostra ####
# Normalidade
shapiro.test(Caminhantes$idade) # teste de Shapiro-Wilk aos valores da amostra do retorno diário da Caminhantes

# Em caso de não rejeição...
t.test(Caminhantes$idade, mu=50, alternative="greater") # Teste unilateral direito do rendimento médio ser positivo 

# Em caso de rejeição da normalidade...
library(moments)
agostino.test(Caminhantes$idade) # Teste à simetria
# Em caso de não rejeição...
wilcox.test (Caminhantes$idade) # Teste de Wilcoxon
# Em caso de rejeição
library(BSDA)
SIGN.test(Caminhantes$idade, conf.level=0.90) # Intervalo de confiança a 90% para o rendimento mediano da Caminhantes