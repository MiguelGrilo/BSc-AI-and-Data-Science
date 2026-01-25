# criar a base de dados
cart<-read.csv2("Cartões_Amarelos.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                sep=";", dec=",", header=T, encoding = "UTF-8")
cart
summary(cart)
# criar amostra das diferenças
cart$dif<-cart$Parte_II-cart$Parte_I
# Normalidade
shapiro.test(cart$dif) # teste de Shapiro-Wilk aos valores das diferenças
# Passando na normalidade...
# H0:μ1-μ2≤0 vs H1:μ1-μ2>0
t.test(cart$Parte_II, cart$Parte_I, paired=TRUE, conf.level = 0.90) # H0:μ1-μ2≤0 vs H1:μ1-μ2>0
t.test(cart$Parte_II, cart$Parte_I, paired=TRUE, mu=3, alternative="greater") # H0:μ1-μ2≤0 vs H1:μ1-μ2>0
# Em caso de rejeição...
library(moments)
agostino.test(cart$dif) # teste de Agostino às diferenças
# Rejeitando a normalidade e sendo simétrica...
# Teste de Wilcoxon
wilcox.test(cart$Parte_II, cart$Parte_I, paired=TRUE, conf.int = TRUE, conf.level=0.95) # Intervalo de confinça
wilcox.test(cart$Parte_II, cart$Parte_I, mu=3, paired=TRUE, alternative="greater") # Teste de Hipóteses
# Rejeitando a normalidade e sendo assimétrica
library(BSDA)
# Teste do Sinal
SIGN.test(cart$Parte_II, cart$Parte_I, conf.int = TRUE, conf.level=0.95) # Intervalo de confiança
SIGN.test(cart$Parte_II, cart$Parte_I, md=3, alternative="greater") # Teste de Hipóteses