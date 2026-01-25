# criar a base de dados
Antes=c(10.5, 12.0, 11.5, 13.0, 12.5, 10.0, 10.8, 11.0, 11.8, 12.5, 13.0, 11.5)
Depois=c(9.5, 10.0, 10.8, 11.5, 11.0, 9.0, 9.8, 10.0, 10.5, 11.0, 11.5, 10.5)

# criar amostra das diferenças
dif<-Depois-Antes

# Normalidade
shapiro.test(dif) # teste de Shapiro-Wilk aos valores das diferenças

# Passando na normalidade...

t.test(Depois, Antes, mu=-1, paired=TRUE, alternative="less") # H0:μ1-μ2≤0 vs H1:μ1-μ2>0
t.test(Depois, Antes, paired=TRUE, conf.level = 0.95)

# Em caso de rejeição...
library(moments)
agostino.test(dif) # teste de Agostino às diferenças

# Rejeitando a normalidade e sendo simétrica...
# Teste de Wilcoxon
wilcox.test(Depois, Antes, conf.int = TRUE, conf.level=0.95) # Intervalo de confinça
wilcox.test(Depois, Antes, mu=-1, paired=TRUE, alternative="less") # Teste de Hipóteses

# Rejeitando a normalidade e sendo assimétrica
library(BSDA)
# Teste do Sinal
SIGN.test(Depois, Antes, conf.int = TRUE, conf.level=0.95) # Intervalo de confiança
SIGN.test(Depois, Antes, md=0, alternative="less") # Teste de Hipóteses
