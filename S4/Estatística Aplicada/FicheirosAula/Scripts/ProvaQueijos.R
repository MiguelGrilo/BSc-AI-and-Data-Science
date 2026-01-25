library (car)
library(nortest)
library(moments)
library(PMCMRplus)
prova <- read.csv2("Queijosalt.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                    dec=",", header=T, encoding = "UTF-8")
summary(prova)
# médias e desvios padrão por Tipo e por factor(Provador) #
with(prova, tapply(Aval,  Tipo,  mean, na.rm = TRUE))
with(prova, tapply(Aval,  Tipo,  sd, na.rm = TRUE))
with(prova, tapply(Aval,  Tipo,  quantile , p=(c(0.25, 0.5, 0.75)), na.rm=TRUE))
#---------------------------------------------#
### ANOVA - 1 factor em factor(Provador) Casualizados ###
#---------------------------------------------#
### Modelo ANOVA ###
mod<-aov(Aval~factor(Provador)+Tipo, prova) # efeito dos factor(Provador): fixo #
summary (mod)
# Há diferença entre os tipos
# Resíduos #
rs<-rstandard (mod)
#------Normalidade------#
# Gráfico qplot #
qqPlot(rs)
## Testes de normalidade ##
# Shapiro-Wilk #
shapiro.test(rs)
# Admitimos a normalidade
#---Homocedasticidade----#
plot(mod$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")
leveneTest(Aval ~ Tipo, data = prova)
# Admitimos a homocedasticidade
#----Não aditividade-----#
library(ggplot2)
# Gráfico de interação: factor(Provador) x Tipo
ggplot(prova, aes(x = factor(Provador), y = Aval, color = Tipo, group = Tipo)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Interação entre factor(Provador) e Tipo",
       x = "factor(Provador)",
       y = "Número de Semanas",
       color = "Tipo") +
  theme_minimal() +
  theme(text = element_text(size = 14), legend.position = "bottom")

## Teste  de  não  aditividade (apenas caso de factor(Provador) fixos) ##
library(asbio)
tukey.add.test(y=prova$Aval, A = prova$Tipo, B = factor(prova$Provador))

## Teste de comparações múltiplas de Tukey para os Tratamentos ##
modi<-aov(Aval~Tipo+factor(Provador), prova)
tuk<-TukeyHSD(modi, "Tipo")  
tuk
plot(tuk)
