rm(list=ls(all=TRUE))

### Carregar Pacotes Necessários ###
library (car)
library (moments)
library(effects)
library(multcomp)
library(reshape)
library(ggplot2)

### Ler os dados que se encontram no ficheiro ostras.csv: neste caso a base de dados fica designada por "ostras"###
ostras <- read.csv2 ("ostras.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                     dec=",", header=T, sep=";", encoding = "UTF-8") 
### Ler os valores da base de dados ###
ostras
### Designações das variáveis na base de dados ###
names(ostras)
summary(ostras)

# médias e desvios padrão por Tratamento  #
tapply(ostras$Peso_final,  ostras$Tratamentos,  mean, na.rm = TRUE)
tapply(ostras$Peso_final,  ostras$Tratamentos,  sd, na.rm = TRUE)


### Modelo ###
contrasts(ostras$Tratamentos)=contr.poly(5) 
mod<-aov(Peso_final ~ Peso_inicial+Tratamentos, data=ostras) 
Anova(mod, type="III") 

# ------- Linearidade entre covariável e variável dependente ------- #
ggplot(ostras, aes(Peso_inicial, Peso_final)) +
  geom_point(size = 3) + geom_smooth(method = "lm", alpha = 0.1) + labs(x = "Peso incial", y = "peso final")


# --------Homogeneidade de declives-------- #
ggplot(ostras, aes(Peso_inicial, Peso_final, colour = Tratamentos))+
  geom_point(aes(shape = Tratamentos), size = 3) + geom_smooth(method = "lm", aes(fill = Tratamentos), alpha = 0.1) + labs(x = "Peso inicial", y = "Peso final")

modi<-aov(Peso_final ~ Peso_inicial*Tratamentos, data=ostras) 
Anova(modi, type="III") 

# ------ independência entre covariável e factor (neste caso não faz sentido devido à forma como a experiência foi feita----------) #
modind<-aov(Peso_inicial ~ Tratamentos, ostras)
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

#---Homocedasticidade----#
# Resíduos vs valores ajustados #
plot(mod$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")
# Levene #
leveneTest(ostras$Peso_final~ostras$Tratamentos)


##--- Médias ajustadas--- ##
adjustedMeans<-effect("Tratamentos", mod, se=TRUE)
summary(adjustedMeans)


##---Comparações múltiplas---##
Tuk<-glht(mod, linfct = mcp(Tratamentos = "Tukey"))
summary(Tuk)
confint(Tuk)
plot(Tuk)


## ------Contrastes------ ##
# Definição dos contrastes #
contrasts(ostras$Tratamentos)<-cbind(c(1, 1,-1, -1, 0), c(1,-1,1,-1, 0), c(-1,-1,-1,-1,4), c(1,-1,-1,1, 0)/4)
# Verificação da ortogonalidade #
round(crossprod(contrasts(ostras$Tratamentos)), 2)
# Análise de variância #
modc<-aov(Peso_final ~ Peso_inicial+Tratamentos, data=ostras)
summary(modc, split=list(Tratamentos=list("Fria vs Quente"=1, "Fundo vs superfície"=2, "Controlo vs Restantes"=3, "Interação"=4)))


#####------------------------------------------------------#####
#### OBSERVAÇÃO #####
### Ignorando o peso inicial ####
mod2<-aov(Peso_final ~ Tratamentos, data=ostras) 
summary(mod2)
rs<-rstandard(mod2) 
qqPlot(rs)
shapiro.test(rs)
plot(mod$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")
abline(h=0)
leveneTest(ostras$Peso_final~ostras$Tratamentos)
(extuk  <-  TukeyHSD(mod2))
plot(extuk)


### Falham os pressupostos ###
# Modelo ANCOVA Robusta
library(robustbase)
mod_robust <- lmrob(Peso_final ~ Peso_inicial + Tratamentos, data = ostras)
Anova(mod_robust, type = 3)

library(multcomp)
posthoc <- glht(mod_robust, linfct = mcp(Tratamentos = "Tukey"))
summary(posthoc)







