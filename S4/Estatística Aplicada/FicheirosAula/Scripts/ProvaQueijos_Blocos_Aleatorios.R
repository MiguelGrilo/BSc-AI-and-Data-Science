library (car)
library(nortest)
library(moments)
library(PMCMRplus)
library(asbio)

queijos <- read.csv2 ("Queijosalt.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                      dec=",", header=T, sep=";", encoding = "UTF-8") 
queijos
summary(queijos)


# médias e desvios padrão por Tipo e por Provador #
tapply(queijos$Aval,  queijos$Tipo,  mean, na.rm = TRUE)
tapply(queijos$Aval, queijos$Provador,  mean, na.rm = TRUE)
tapply(queijos$Aval,  queijos$Tipo,  sd, na.rm = TRUE)
tapply(queijos$Aval, queijos$Provador,  sd, na.rm = TRUE)

Provador<-factor(queijos$Provador)
#---------------------------------------------#
### ANOVA - 1 factor em Blocos Casualizados ###
#---------------------------------------------#

### Modelo ANOVA ###
mod<-aov(Aval~Tipo+Provador, queijos) # efeito dos Provador: fixo #
summary (mod)

modba<-aov(Aval~Error(Provador)+Tipo, queijos) # efeito dos Provador: aleatório #
summary(modba)

# Resíduos #
rs<-residuals (mod)

#------Normalidade------#

# Gráfico qplot #
qqPlot(rs)

## Testes de normalidade ##
# Shapiro-Wilk #
shapiro.test(rs)
# D'Agostino #
agostino.test(rs) 
# Anscombe-Glynn #
anscombe.test (rs)

#---Homocedasticidade----#

## Resíduos vs valores ajustados ##
plot(modba$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")


#----Não aditividade-----#
with(queijos, interaction.plot (Provador, Tipo, Aval))
with(queijos, interaction.plot (Tipo, Provador, Aval))
plot(mod$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")

## Teste  de  não  aditividade  ##
tukey.add.test(y =queijos$Aval, A = queijos$Tipo, B = queijos$Provador)


#------------------------------#
### Comparações múltiplas ###
#------------------------------#

## Teste de comparações múltiplas de Tukey para os Tipo ##
tuk<-TukeyHSD(mod, "Tipo")  
tuk
plot(tuk)

#-----------------------------------------------------------------#
###################################################################
################## Abordagem não paramétrica ######################
#-----------------------------------------------------------------#
## OBS: Neste caso não era necessário, pois com a transformação podemos admitir o pressuposto da normalidade ##

### Teste de Friedmann ###
friedman.test(Aval ~ Tipo|Provador, queijos)

### Comparações múltiplas ###


# Nemenyi #
y <- matrix(c(queijos$Aval),nrow=10, ncol=3,dimnames=list(1:10,c("A","B","C")))
print(y)
friedmanTest(y)
frdAllPairsNemenyiTest(y)

#-----------------------------------------------------------------#
###################################################################
############# Blocos com efeitos aleatórios  ######################
#-----------------------------------------------------------------#
library(lme4)
library(car)       
library(emmeans)  
library(multcomp)  
library(ggplot2)   
library(PMCMRplus) 

### Modelo ANOVA com Blocos como Efeito Aleatório ###
mod_misto <- lmer(Aval ~ Tipo + (1 | Provador), data = queijos)
Anova(mod_misto) 

# Resíduos #
rs <- residuals(mod_misto)

#------Normalidade------#
qqPlot(rs)  # Gráfico Q-Q
shapiro.test(rs)  # Teste de Shapiro-Wilk

#---Homocedasticidade----#
plot(fitted(mod_misto), rs, xlab="Valores Ajustados", ylab="Resíduos")

#----Normalidade do efeito dos blocos-------#
efeitos_blocos <- ranef(mod_misto)$Tipo[[1]]
qqPlot(efeitos_blocos, main = "QQ-Plot dos Efeitos Aleatórios dos Blocos")

shapiro.test(efeitos_blocos) 

### Comparações Múltiplas com Blocos Aleatórios ###
comp_mult <- emmeans(mod_misto, pairwise ~ Tipo, adjust = "tukey")
summary(comp_mult)

#-----------------------------------------------------------------#
################## Abordagem Não Paramétrica ######################
#-----------------------------------------------------------------#

### Teste de Friedmann ###
friedman.test(Aval ~ Tipo | Provador, data = queijos)

### Comparações Múltiplas ###
frdAllPairsNemenyiTest(Aval ~ Tipo | Provador, data = queijos)





