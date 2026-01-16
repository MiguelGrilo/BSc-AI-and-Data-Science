rm(list=ls(all=TRUE))
#+Carregar pacotes necessários#
library (car)
library(moments)
library(PMCMRplus)
library(moments)

dados <- data.frame(
  Regiao = factor(rep(c("Norte", "Sul", "Leste", "Oeste"), each = 4)),
  Trimestre = factor(rep(c("T1", "T2", "T3", "T4"), times = 4)),
  Estrategia = factor(c("A", "B", "C", "D", "B", "C", "D", "A", "C", "D", "A", "B", "D", "A", "B", "C")),
  Vendas = c(10.2, 12.5, 14.8, 9.3, 11.7, 15.9, 13.2, 10.5, 16.8, 14.1, 14.0, 11.2, 13.4, 14.6, 15.3, 17.0)
)
dados
+
# Ajustando o modelo ANOVA para o Quadrado Latino
mod <- aov(Vendas ~ Estrategia + Regiao + Trimestre, data = dados)
rs<-rstandard(mod) # Resíduos

#------Normalidade------#

qqPlot(rs) 
shapiro.test(rs)

#---Homocedasticidade----#

plot(mod$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")
abline(h=0, lty=3)
leveneTest(Vendas ~ Estrategia, data = dados)
# com suspeição de heterocedasticidade com os outros fatores
leveneTest(Vendas ~ Regiao, data = dados)
leveneTest(Vendas ~ Trimestre, data = dados)

# Interação Linha*Coluna 

ukey.add.test(y = dados$Vendas, A = dados$Trimestre, B = dados$Regiao)
#Interação Linha*Tratamento 
tukey.add.test(y = dados$Vendas, A = dados$Estrategia, B = dados$Trimestre)
#Interação Coluna*Tratamento 
tukey.add.test(y = dados$Vendas, A = dados$Estrategia, B = dados$Regiao)

# Resultados da ANOVA #
summary(mod)

#------------------------------#
### Comparações múltiplas ###
#------------------------------#

tukql <- TukeyHSD(mod, "Estrategia")  
tukql
plot(tukql)

