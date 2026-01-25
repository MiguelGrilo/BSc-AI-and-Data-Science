# criar a base de dados
cart<-data.frame(Arbitro=c("A", "B", "C", "D", "E", "F", "G", "H"),
                 ParteI=c(10, 1, 20, 9, 28, 13, 11, 5),
                 ParteII=c(28, 7, 37, 7, 35, 24, 39, 5))
cart
summary(cart)

# criar amostra das diferenças
cart$dif<-cart$ParteII-cart$ParteI

# Normalidade
shapiro.test(cart$dif) # teste de Shapiro-Wilk aos valores das diferenças

# Passando na normalidade...
t.test(cart$ParteII, cart$ParteI, mu=0, paired=TRUE, alterdative="greater") # H0:μ1-μ2≤0 vs H1:μ1-μ2>0

# Rejeitando a normalidade
library(moments)
agostino.test(cart$dif) # teste de Simetria

# sendo simétrica...
# Teste de Wilcoxon
library(exactRankTests)
#library(coin)
wilcox.exact(cart$ParteII, cart$ParteI, mu=0, paired=TRUE, alternative="greater") # H0:h1-h2≤0 vs H1:h1-h2>0

# Rejeitando a normalidade e sendo assimétrica
library(BSDA)
# Teste do Sinal
SIGN.test(cart$ParteII, cart$ParteI, md=0, alternative="greater")

