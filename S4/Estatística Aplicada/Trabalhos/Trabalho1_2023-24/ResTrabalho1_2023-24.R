## TRABALHO 1 ANO PASSADO ##

# 1

# 2

# 3
# Fator 1 - Tipoqueijos
# Fator 2 - Sal
# Sal (0g(P / M) / 20g(P / M) / 40g(P / M) / 60g(P / M))

library (car)
library(emmeans)
library(ggplot2)
queijos <- read.csv2 ("queijos.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                     dec=",", header=T, sep=";", encoding = "UTF-8") 
queijos
names(queijos)
summary(queijos)
with(queijos, tapply(Gomosidade,  list(Sal, Tipo.de.queijo),  mean, na.rm=TRUE))
(mediasSalforo  <-  tapply(queijos$Gomosidade,  queijos$Sal,  mean, na.rm=TRUE))
(mediasTipo.de.queijoassia  <-  tapply(queijos$Gomosidade,  queijos$Tipo.de.queijo,  mean, na.rm=TRUE))
with(queijos, tapply(Gomosidade,  list(Sal, Tipo.de.queijo),  sd, na.rm=TRUE))
(mediasSalforo  <-  tapply(queijos$Gomosidade,  queijos$Sal,  sd, na.rm=TRUE))
(mediasTipo.de.queijoassia  <-  tapply(queijos$Gomosidade,  queijos$Tipo.de.queijo,  sd, na.rm=TRUE))
# Gráfico de interação: Sal x Tipo de queijo
ggplot(queijos, aes(x = Sal, y = Gomosidade, group = Tipo.de.queijo, color = Tipo.de.queijo)) +
  stat_summary(fun = mean, geom = "point", size = 3) +  # Médias dos grupos
  stat_summary(fun = mean, geom = "line", aes(group = Tipo.de.queijo), linewidth = 1) +  # Linhas de interação
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Erros padrão
  labs(title = "Interação entre Sal e Tipo de queijo",
       x = "Sal",
       y = "Gomosidade",
       color = "Tipo de queijo") +
  theme_minimal()
# Gráfico de interação: Tipo de queijo x Sal
ggplot(queijos, aes(x = Tipo.de.queijo, y = Gomosidade, group = Sal, color = Sal)) +
  stat_summary(fun = mean, geom = "point", size = 3) +  # Médias dos grupos
  stat_summary(fun = mean, geom = "line", aes(group = Sal), linewidth = 1) +  # Linhas de interação
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Erros padrão
  labs(title = "Interação entre Sal e Tipo de queijo",
       x = "Tipo de queijo",
       y = "Gomosidade",
       color = "Sal") +
  theme_minimal()
#-------------------------#
### ANOVA - 2 fatores  ###
#------------------------_#
### Modelo ANOVA ###
mod<-aov(log(Gomosidade)~Sal*Tipo.de.queijo, queijos)
rs<-rstandard (mod)
#------Normalidade------#
qqPlot(rs)
#---Homocedasticidade----#
plot(mod$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")

model.tables(mod, "means") # médias dos tratamentos
model.tables(mod, "effects") # efeitos dos tratamentos
#------Normalidade------#
#-----------------------#
# Gráfico qplot #
qqPlot(rs)
## Testes de normalidade ##
# Shapiro-Wilk #
shapiro.test(rs)
#---Homocedasticidade----#
#------------------------#
## Resíduos vs valores ajustados ##
plot(mod$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")
abline(h=0, lty=3)


#### Resultados da ANOVA ####
#####....................####
summary(mod)
#-------------------------------------------------------------#
### Comparações múltiplas quando a interação significativa ###
#-------------------------------------------------------------#
# todas as comparações 
tuk  <-  TukeyHSD(mod, "Sal:Tipo.de.queijo")
tuk
plot(tuk, cex.axis=0.5)

# Comparar os níveis de um factor dentro de cada nível do outro #

# Tukey para Sal #
mf <- emmeans(mod, ~ Sal | Tipo.de.queijo)
(comp1<-pairs(mf, adjust="Tukey"))
plot(comp1)

# Tukey para Tipo.de.queijoássio #
mp <- emmeans(mod, ~ Tipo.de.queijo | Sal)
(comp2<-pairs(mp, adjust="Tukey"))
plot(comp2)

# representações dos valores preditos
emmip(mod, Sal~Tipo.de.queijo, type = "response", CIs = TRUE)+
  xlab("Fósoforo")+
  ylab("Gomosidadeução estimada (to/ha)")+
  labs(color = "Tipo.de.queijoássio")

emmip(mod, Tipo.de.queijo~Sal, type = "response", CIs = TRUE)+
  xlab("Tipo.de.queijoássio")+
  ylab("Gomosidadeução estimada (to/ha)")+
  labs(color = "Sal")

##------------------------------------------------------------------#
### Comparações múltiplas quando a interação não significativa ###
#-------------------------------------------------------------------#
# Tukey para fósforo #
mf <- emmeans(mod, ~ Sal)
pairs(mf, adjust="Tukey")

# Tukey para Tipo.de.queijoássio #
mp <- emmeans(mod, ~ Tipo.de.queijo)
pairs(mp, adjust="Tukey")



# 4
library(rstatix)
library(tidyverse)
library(ggpubr)
library(emmeans) 
library(afex)
library(car)
library(PMCMRplus)
library(psych)
library(coin)
library(nlme)
library(lme4)
library(lmerTest)
library(ARTool)

bolotas <- read.csv2 ("bolotas.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                      dec=",", header=T, sep=";", encoding = "UTF-8") 
summary(bolotas)


# b) Admitindo que falhou a normalidade que conclusões se podem retirar da análise dos dados?
bolotas1<-subset(bolotas, Fenolicos!="NA")
modelo_art <- art(Fenolicos ~ Especie * Periodo + (1 | Arvore), data = bolotas1) # Transformação dos dados para ranks alinhados
anova(modelo_art)
## para periodos dentro da Especie
bolotas_Fenolicos1 <- subset(bolotas1, Especie == "A")
modelo_art_raca1 <- art(Fenolicos ~ Periodo + (1 | Arvore), data = bolotas_Fenolicos1)
art.con(modelo_art_raca1, "Periodo", adjust = "holm")
bolotas_Fenolicos2 <- subset(bolotas1, Especie == "S")
modelo_art_raca2 <- art(Fenolicos ~ Periodo + (1 | Arvore), data = bolotas_Fenolicos2)
art.con(modelo_art_raca2, "Periodo", adjust = "holm")

## para raças dentro de períodos
bolotas_Periodo1 <- subset(bolotas, Periodo == "I")
modelo_art_periodo1 <- art(Fenolicos ~ Especie, data = bolotas_Periodo1)
art.con(modelo_art_periodo1, "Especie", adjust = "holm")
bolotas_Periodo2 <- subset(bolotas1, Periodo == "II")
modelo_art_periodo2 <- art(Fenolicos ~ Especie, data = bolotas_Periodo2)
art.con(modelo_art_periodo2, "Especie", adjust = "holm")
bolotas_Periodo3 <- subset(bolotas, Periodo == "II")
modelo_art_periodo2 <- art(Fenolicos ~ Especie, data = bolotas_Periodo2)
art.con(modelo_art_periodo2, "Especie", adjust = "holm")

# c) Admitindo que falhou a homocedasticidade
bolotas1 <- subset(bolotas, Fenolicos!="NA")
modelo_lme <- lme(Fenolicos ~ Especie * Periodo, 
                  random = ~1 | Arvore, 
                  data = bovinos1, 
                  weights = varIdent(form = ~1 | Especie))  # Permite variâncias diferentes para cada Especie
anova(modelo_lme)
posthoc_lmep <- emmeans(modelo_lme, pairwise ~ Periodo | Especie, adjust = "holm")
print(posthoc_lmep)
posthoc_lmer <- emmeans(modelo_lme, pairwise ~ Especie | Periodo, adjust = "tukey")
print(posthoc_lmer)

# 5
rm(list=ls(all=TRUE))

### Carregar Pacotes Necessários ###
library (car)
library (moments)
library(effects)
library(multcomp)
library(reshape)
library(ggplot2)

### Ler os dados que se encontram no ficheiro ea.csv: neste caso a base de dados fica designada por "ea"###
ea <- read.csv2 ("ea.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                     dec=",", header=T, sep=";", encoding = "UTF-8") 
### Ler os valores da base de dados ###
ea
### Designações das variáveis na base de dados ###
names(ea)
summary(ea)
# médias e desvios padrão por Tratamento  #
tapply(ea$seis_minutos_Fim,  ea$grupo,  mean, na.rm = TRUE)
tapply(ea$seis_minutos_Fim,  ea$grupo,  sd, na.rm = TRUE)
### Modelo ###
contrasts(ea$grupo)=contr.poly(4) 
mod<-aov(seis_minutos_Fim ~ seis_minutos_Inicio+grupo, data=ea) 
Anova(mod, type="III") 
# ------- Linearidade entre covariável e variável dependente ------- #
ggplot(ea, aes(seis_minutos_Inicio, seis_minutos_Fim)) +
  geom_point(size = 3) + geom_smooth(method = "lm", alpha = 0.1) + labs(x = "Peso incial", y = "peso final")

# --------Homogeneidade de declives-------- #
ggplot(ea, aes(seis_minutos_Inicio, seis_minutos_Fim, colour = grupo))+
  geom_point(aes(shape = grupo), size = 3) + geom_smooth(method = "lm", aes(fill = grupo), alpha = 0.1) + labs(x = "Peso inicial", y = "Peso final")

modi<-aov(seis_minutos_Fim ~ seis_minutos_Inicio*grupo, data=ea) 
Anova(modi, type="III") 
# ------ independência entre covariável e factor (neste caso não faz sentido devido à forma como a experiência foi feita----------) #
modind<-aov(seis_minutos_Inicio ~ grupo, ea)
summary(modind)
## Resíduos ##
rs<-rstandard (mod)
#------Normalidade------#
# Gráfico qplot #
qqPlot(rs)
# Shapiro-Wilk #
shapiro.test(rs)
# D'Agostino #
agostino.test(rs) 
# Anscombe-Glynn #
anscombe.test (rs)
# REJEITAMOS A NORMALIDADE
#---Homocedasticidade----#
# Resíduos vs valores ajustados #
plot(mod$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")
# Levene #
leveneTest(ea$seis_minutos_Fim~ea$grupo)
# ADMITIMOS HOMOCEDASTICIDADE
##--- Médias ajustadas--- ##
adjustedMeans<-effect("grupo", mod, se=TRUE)
summary(adjustedMeans)
### Falham os pressupostos ###
# Modelo ANCOVA Robusta
library(robustbase)
mod_robust <- lmrob(seis_minutos_Fim ~ seis_minutos_Inicio + grupo, data = ea)
Anova(mod_robust, type = 3)

library(multcomp)
posthoc <- glht(mod_robust, linfct = mcp(grupo = "Tukey"))
summary(posthoc)

### Modelo ANOVA ###
modlog<-aov(log(seis_minutos_Fim) ~ seis_minutos_Inicio*grupo, data=ea)
# Resíduos #
rslog<-rstandard (modlog)
#------Normalidade------#
qqPlot(rslog)
# Shapiro-Wilk #
shapiro.test(rslog)
# ADMITIMOS A NORMALIDADE
#---Homocedasticidade----#
plot(modlog$fit,  rslog,  xlab="valores  ajustados",  ylab="resíduos")
# ADMITIMOS A HOMOCEDASTICIDADE

##---Comparações múltiplas---##
Tuk<-glht(rslog, linfct = mcp(Tratamentos = "Tukey"))
summary(Tuk)
confint(Tuk)
plot(Tuk)