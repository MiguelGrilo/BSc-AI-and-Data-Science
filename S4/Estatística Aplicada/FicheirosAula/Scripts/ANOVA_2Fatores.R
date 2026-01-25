#Carregar pacotes necessários#
library (car)
library(emmeans)
library(ggplot2)


## Carregar o ficheiro dos dados ##
fospot <- read.csv2 ("fospot.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                     dec=",", header=T, sep=";", encoding = "UTF-8") 
# visualizar dados #
fospot
# designaçõees das variáveis
names(fospot)
# estrutura da base de dados e sumário das variáveis #
summary(fospot)

## Médias marginais e desvios padrão ##
with(fospot, tapply(prod,  list(fos, pot),  mean, na.rm=TRUE))
(mediasfosforo  <-  tapply(fospot$prod,  fospot$fos,  mean, na.rm=TRUE))
(mediaspotassia  <-  tapply(fospot$prod,  fospot$pot,  mean, na.rm=TRUE))
with(fospot, tapply(prod,  list(fos, pot),  sd, na.rm=TRUE))
(mediasfosforo  <-  tapply(fospot$prod,  fospot$fos,  sd, na.rm=TRUE))
(mediaspotassia  <-  tapply(fospot$prod,  fospot$pot,  sd, na.rm=TRUE))

## Representar as médias de um factor separando para cada nível do outro ##

# Gráfico de interação: Fósforo x Potássio
ggplot(fospot, aes(x = fos, y = prod, group = pot, color = pot)) +
  stat_summary(fun = mean, geom = "point", size = 3) +  # Médias dos grupos
  stat_summary(fun = mean, geom = "line", aes(group = pot), linewidth = 1) +  # Linhas de interação
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Erros padrão
  labs(title = "Interação entre Fósforo e Potássio",
       x = "Fósforo",
       y = "Produção média (to/ha)",
       color = "Potássio") +
  theme_minimal()

# Gráfico de interação: Potássio x Fósoforo
ggplot(fospot, aes(x = pot, y = prod, group = fos, color = fos)) +
  stat_summary(fun = mean, geom = "point", size = 3) +  # Médias dos grupos
  stat_summary(fun = mean, geom = "line", aes(group = fos), linewidth = 1) +  # Linhas de interação
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Erros padrão
  labs(title = "Interação entre Fósforo e Potássio",
       x = "Potássio",
       y = "Produção média (to/ha)",
       color = "Fósoforo") +
  theme_minimal()

#-------------------------#
### ANOVA - 2 fatores  ###
#------------------------_#

(mod  <-  aov(prod  ~  fos+pot+fos:pot, fospot))  # fos+pot+fos:pot   OU    fos*pot
# OU
(mod  <-  aov(prod  ~  fos*pot, fospot))

model.tables(mod, "means") # médias dos tratamentos
model.tables(mod, "effects") # efeitos dos tratamentos

rs<-rstandard(mod) # Resíduos

#------Normalidade------#
#-----------------------#

# Gráfico qplot #
qqPlot(rs)

## Testes de normalidade ##
# Shapiro-Wilk #
shapiro.test(rs)
# em caso de rejeição da normalidade
library(moments)
agostino.test(rs)
anscombe.test(rs)

#---Homocedasticidade----#
#------------------------#

## Resíduos vs valores ajustados ##
plot(mod$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")
abline(h=0, lty=3)

# Levene #
leveneTest(rs ~ fospot$fos*fospot$pot)


#### Resultados da ANOVA ####
#####....................####
summary(mod)

#-------------------------------------------------------------#
### Comparações múltiplas quando a interação significativa ###
#-------------------------------------------------------------#

# todas as comparações 
tuk  <-  TukeyHSD(mod, "fos:pot")
tuk
plot(tuk, cex.axis=0.5)

# Comparar os níveis de um factor dentro de cada nível do outro #

# Tukey para Fósforo #
mf <- emmeans(mod, ~ fos | pot)
(comp1<-pairs(mf, adjust="Tukey"))
plot(comp1)

# Tukey para Potássio #
mp <- emmeans(mod, ~ pot | fos)
(comp2<-pairs(mp, adjust="Tukey"))
plot(comp2)

# representações dos valores preditos
emmip(mod, fos~pot, type = "response", CIs = TRUE)+
  xlab("Fósoforo")+
  ylab("Produção estimada (to/ha)")+
  labs(color = "Potássio")

emmip(mod, pot~fos, type = "response", CIs = TRUE)+
  xlab("Potássio")+
  ylab("Produção estimada (to/ha)")+
  labs(color = "Fósforo")

##------------------------------------------------------------------#
### Comparações múltiplas quando a interação não significativa ###
#-------------------------------------------------------------------#
# Tukey para fósforo #
mf <- emmeans(mod, ~ fos)
pairs(mf, adjust="Tukey")

# Tukey para potássio #
mp <- emmeans(mod, ~ pot)
pairs(mp, adjust="Tukey")


####...............................................................................###
#### Para delineamentos desequilibrados devem usar-se as somas de quadrados tipo III  ###
options(contrasts = c("contr.sum", "contr.poly")) 
modne  <-  lm(prod  ~  fos+pot+fos:pot, fospot)
Anova(modne, type="III")


##################################################################
#########....... ART - Aligned Rank Transform............#########
##################################################################

### falha da normalidade - ART ###
library(ARTool)

# Aplicar o alinhamento e ranqueamento dos dados
mod_art <- art(prod ~ fos * pot, data = fospot)
anova(mod_art)

# Comparações múltiplas dentro da interação (se for significativa)
art.con(mod_art, "fos:pot")

# Comparar fos dentro de cada nível de pot
for (nivel_pot in unique(fospot$pot)) {
  cat("\nComparações para Fósforo dentro de", nivel_pot, "de Potássio\n")
  
  # Criar subconjunto dos dados onde pot é fixo
  dados_subset <- subset(fospot, pot == nivel_pot)
  
  # Ajustar o modelo ART para este subconjunto
  modelo_art_subset <- art(prod ~ fos, data = dados_subset)
  
  # Comparações múltiplas para fos dentro deste nível de pot
  print(art.con(modelo_art_subset, "fos"))
}

for (nivel_fos in unique(fospot$fos)) {
  cat("\nComparações para Potássio dentro de", nivel_pot, "de Fósforo\n")
  
  # Criar subconjunto dos dados onde pot é fixo
  dados_subset <- subset(fospot, fos == nivel_fos)
  
  # Ajustar o modelo ART para este subconjunto
  modelo_art_subset <- art(prod ~ pot, data = dados_subset)
  
  # Comparações múltiplas para fos dentro deste nível de pot
  print(art.con(modelo_art_subset, "pot"))
}

#### Não sendo a interação significativa ####
art.con(mod_art, "fos", adjust = "holm")
art.con(mod_art, "pot", adjust = "holm")

##################################################################
#########....... ...ANOVA por Permutação............##############
##################################################################

#### falha a homocedasticidade também #####
library(permuco)

# Aplicar ANOVA por permutação
modelo_perm <- aovperm(prod ~ fos * pot, data = fospot, np = 5000)  # 5000 permutações
summary(modelo_perm)

### interação significativa ###
for (nivel_pot in unique(fospot$pot)) {
  cat("\nComparações para Fósforo dentro de", nivel_pot, " de Potássio\n")
  dados_subset <- subset(fospot, pot == nivel_pot)
  print(pairwise.t.test(dados_subset$prod, dados_subset$fos, p.adjust.method = "holm", paired = FALSE))
}

for (nivel_fos in unique(fospot$fos)) {
  cat("\nComparações para Potássio dentro de", nivel_pot, " de Fósforo\n")
  dados_subset <- subset(fospot, fos == nivel_fos)
  print(pairwise.t.test(dados_subset$prod, dados_subset$pot, p.adjust.method = "holm", paired = FALSE))
}

### Interação não signifciativa ###
pairwise.t.test(fospot$prod, fospot$fos, p.adjust.method = "holm", paired = FALSE)
pairwise.t.test(fospot$prod, fospot$pot, p.adjust.method = "holm", paired = FALSE)

##################################################################
#########............ANOVA White/HC3..............################
##################################################################

### falha apenas a homocedasticidade ###

library(car)
modelo_lm <- lm(prod ~ fos * pot, data = fospot)
Anova(modelo_lm, white.adjust = TRUE)

## com interação significativa ##
mf <- emmeans(modelo_lm, ~ fos | pot)
(comp1<-pairs(mf, adjust="Tukey"))
plot(comp1)

mp <- emmeans(modelo_lm, ~ pot | fos)
(comp2<-pairs(mp, adjust="Tukey"))
plot(comp2)


### sem interação significativa ###
# Tukey para fósforo #
mf <- emmeans(modelo_lm, ~ fos)
pairs(mf, adjust="Tukey")

# Tukey para potássio #
mp <- emmeans(modelo_lm, ~ pot)
pairs(mp, adjust="Tukey")
