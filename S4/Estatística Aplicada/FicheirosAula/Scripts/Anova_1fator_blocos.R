library (car)
library(nortest)
library(moments)
library(PMCMRplus)

pragas <- read.csv2("pragas.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                    dec=",", header=T, encoding = "UTF-8")
summary(pragas)

# médias e desvios padrão por Tratamentos e por Blocos #
with(pragas, tapply(nsemanas,  Tratamentos,  mean, na.rm = TRUE))
with(pragas, tapply(nsemanas,  Tratamentos,  sd, na.rm = TRUE))
with(pragas, tapply(nsemanas,  Tratamentos,  quantile , p=(c(0.25, 0.5, 0.75)), na.rm=TRUE))


#---------------------------------------------#
### ANOVA - 1 factor em Blocos Casualizados ###
#---------------------------------------------#

### Modelo ANOVA ###
mod<-aov(nsemanas~Tratamentos+Blocos, pragas) # efeito dos blocos: fixo #
summary (mod)

# Resíduos #
rs<-rstandard (mod)

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

plot(mod$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")
leveneTest(nsemanas ~ Tratamentos, data = pragas)
# caso haja forte suspeição de não homocedasticidade nos restantes fatores
leveneTest(nsemanas ~ Blocos, data = pragas)

#----Não aditividade-----#
library(ggplot2)

# Gráfico de interação: Blocos x Tratamentos
ggplot(pragas, aes(x = Blocos, y = nsemanas, color = Tratamentos, group = Tratamentos)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Interação entre Blocos e Tratamentos",
       x = "Blocos",
       y = "Número de Semanas",
       color = "Tratamentos") +
  theme_minimal() +
  theme(text = element_text(size = 14), legend.position = "bottom")

# Gráfico de interação: Tratamentos x Blocos
ggplot(pragas, aes(x = Tratamentos, y = nsemanas, color = Blocos, group = Blocos)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Interação entre Tratamentos e Blocos",
       x = "Tratamentos",
       y = "Número de Semanas",
       color = "Blocos") +
  theme_minimal() +
  theme(text = element_text(size = 14), legend.position = "bottom")

## Teste  de  não  aditividade (apenas caso de blocos fixos) ##
library(asbio)
tukey.add.test(y=pragas$nsemanas, A = pragas$Tratamentos, B = pragas$Blocos)


######################################################################################
############################ Transformação logartitmo ################################
######################################################################################

### Modelo ANOVA ###
modlog<-aov(log(nsemanas)~Tratamentos+Blocos, pragas)

# Resíduos #
rslog<-rstandard (modlog)

#------Normalidade------#
qqPlot(rslog)

#---Homocedasticidade----#
plot(modlog$fit,  rslog,  xlab="valores  ajustados",  ylab="resíduos")


######################################################################################
############################ Transformação Raiz ######################################
######################################################################################

### Modelo ANOVA ###
modr<-aov(sqrt(nsemanas)~Tratamentos+Blocos, pragas)

# Resíduos #
rsr<-rstandard (modr)

#------Normalidade------#
qqPlot(rsr)

#---Homocedasticidade----#
plot(modr$fit,  rsr,  xlab="valores  ajustados",  ylab="resíduos")


######################################################################################
############################ Transformação Inversa ###################################
######################################################################################

### Modelo ANOVA ###
modi<-aov(1/(nsemanas)~Tratamentos+Blocos, pragas)

# Resíduos #
rsi<-rstandard (modi)

#------Normalidade------#
qqPlot(rsi)

# Shapiro-Wilk #
shapiro.test(rsi)

#---Homocedasticidade----#
plot(modi$fit,  rsi,  xlab="valores  ajustados",  ylab="resíduos")
leveneTest(1/nsemanas ~ Tratamentos, data = pragas)

##----Não  aditividade-----##
tukey.add.test(y = 1/pragas$nsemanas, A = pragas$Tratamentos, B = pragas$Blocos)

##---outras representações gráficos que podem ser úteis---##
plot(as.numeric(pragas$Tratamentos),  rsi,  xlab="valores  ajustados",  ylab="resíduos")
plot(as.numeric(pragas$Blocos) ,rsi,  xlab="valores  ajustados",  ylab="resíduos")

# Resultados da ANOVA
summary(modi)

# Tamanho do efeito
library(effectsize)
options(es.use_symbols = TRUE)
(om<-omega_squared(modi)) # percentagem da variabilidade da resposta explicada pelo efeito do tratamento. Importância do fator em explicar a variabilidade da resposta.
cohens_f(modi, method="omega") 

#------------------------------#
### Comparações múltiplas ###
#------------------------------#

## Teste de comparações múltiplas de Tukey para os Tratamentos ##
tuk<-TukeyHSD(modi, "Tratamentos")  
tuk
plot(tuk)

#-----------------------------------------------------------------#
###################################################################
################## Abordagem não paramétrica ######################
#-----------------------------------------------------------------#
## OBS: Neste caso não era necessário, pois com a transformação podemos admitir o pressuposto da normalidade ##

### Teste de Friedmann ###
friedman.test(1/nsemanas ~ Tratamentos|Blocos, pragas)

### Comparações múltiplas ###

# Nemenyi #
y <- matrix(c(1/pragas$nsemanas),nrow=12, ncol=4,dimnames=list(1:12,c("A","B","C","D")))
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
mod_misto <- lmer(1/nsemanas ~ Tratamentos + (1 | Blocos), data = pragas)
Anova(mod_misto) 

# Resíduos #
rs <- residuals(mod_misto)

#------Normalidade------#
qqPlot(rs)  # Gráfico Q-Q
shapiro.test(rs)  # Teste de Shapiro-Wilk

#---Homocedasticidade----#
plot(fitted(mod_misto), rs, xlab="Valores Ajustados", ylab="Resíduos")

#----Normalidade do efeito dos blocos-------#
efeitos_blocos <- ranef(mod_misto)$Blocos[[1]]
qqPlot(efeitos_blocos, main = "QQ-Plot dos Efeitos Aleatórios dos Blocos")
shapiro.test(efeitos_blocos) 
library(moments)
agostino.test(efeitos_blocos)
anscombe.test(efeitos_blocos)

### Comparações Múltiplas com Blocos Aleatórios ###
comp_mult <- emmeans(mod_misto, pairwise ~ Tratamentos, adjust = "tukey")
summary(comp_mult)

#-----------------------------------------------------------------#
################## Abordagem Não Paramétrica ######################
#-----------------------------------------------------------------#

### Teste de Friedmann ###
friedman.test(1/nsemanas ~ Tratamentos | Blocos, data = pragas)

### Comparações Múltiplas ###
frdAllPairsNemenyiTest(1/nsemanas ~ Tratamentos | Blocos, data = pragas)

