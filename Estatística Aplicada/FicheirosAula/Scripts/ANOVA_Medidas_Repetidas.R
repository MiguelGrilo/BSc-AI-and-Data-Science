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

bovinos <- read.csv2 ("bovinos_hemograma.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                      dec=",", header=T, sep=";", encoding = "UTF-8") 
summary(bovinos)

#### 1 fator de medidas repetidas ####
#### ............................ ####
### Vamos admitir que todos os animoas são todos da mesma raça

# representação gráfica #
library (ggplot2)
bp1<-ggplot(bovinos, aes(x=periodo, y=Ca)) + 
  geom_boxplot()+
  xlab("Raça")+
  coord_flip() + # caixas na horizontal
  scale_y_continuous(expand = c(0,0), limits=c(8,12), breaks=seq(8,12,1),
                     name="Cálcio")+ #formatação do eixo das abcissas
  scale_fill_manual(values = "chocolate")+ #cores das caixas
  theme(text = element_text(size=12, face="bold", colour="black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.y=element_blank())+
  labs(color = "Período")
bp1

## deteção de outliers ##
outlier<-bovinos %>%
  group_by(periodo) %>%
  identify_outliers(Ca)
data.frame(outlier)

# Normalidade
bovinos %>% 
  group_by(periodo) %>%
  shapiro_test(Ca)
ggqqplot(bovinos, "Ca", facet.by = "periodo")

# Homocedasticidade
leveneTest(Ca~periodo*raça, data=bovinos)

# modelo com teste à esfericidade
mod <-aov_car(Ca~periodo +Error(animal/periodo),data=bovinos) 
summary(mod)

# Comparações múltiplas
mm<-emmeans(mod, "periodo", data=bovinos) 
pairs(mm, adjust="tukey")

### caso falhe a normalidade ###
### ........................ ###
friedman.test(Ca~periodo | animal, data = bovinos) #Friedmann
frdAllPairsConoverTest(bovinos$Ca, bovinos$periodo, bovinos$anima, p.adjust = "holm") #Conover

### caso falhe também a homocedasticidade ###
### ..................................... ###

quade_test(Ca~periodo | animal, data = bovinos) # Quade
frdAllPairsConoverTest(bovinos$Ca, bovinos$periodo, bovinos$anima, p.adjust = "holm") #Conover

### apenas falha a homocedasticidade ###
modelo_lme <- lme(Ca ~ periodo, random = ~1 | animal, 
                  data = bovinos, 
                  weights = varIdent(form = ~1 | periodo)) # Modelo Linear Misto que permite heterocedasticidade entre períodos
anova(modelo_lme)
posthoc_lme <- emmeans(modelo_lme, pairwise ~ periodo, adjust = "holm")
print(posthoc_lme)


#### 1 fator de medidas repetidas e um fator between ####
#### ................................................####

## Médias e desvios padrão ##
with (bovinos, tapply(GB,  list(raça,periodo), mean, na.rm=TRUE))
with(bovinos, tapply(GB,  list(raça,periodo), sd, na.rm=TRUE))
with (bovinos, tapply(GB,  raça,  mean, na.rm=TRUE))
with(bovinos, tapply(GB,  periodo,  median, na.rm=TRUE))
with (bovinos, tapply(GB,  periodo,  sd, na.rm=TRUE))

# modelo com teste à esfericidade
mod <-aov_car(GB~raça*periodo +Error(animal/periodo),data=bovinos, anova_table=list(correction = "GG", es = "pes")) 
summary(mod)
mod

# Normalidade
bovinos %>% 
  group_by(periodo) %>%
  shapiro_test(GB)
ggqqplot(bovinos, "GB", facet.by = "periodo")

# Homocedasticidade
P1<-subset(bovinos, periodo=="I")
P2<-subset(bovinos, periodo=="II")
P3<-subset(bovinos, periodo=="III")
leveneTest(GB~raça, data=P1)
leveneTest(GB~raça, data=P2)
leveneTest(GB~raça, data=P3)
leveneTest(GB ~ raça * periodo, data = bovinos)

emmeans(mod, specs = ~ periodo*raça) # estimativas das médias de cada combinação

bovinos %>%
  ggplot(aes(periodo, GB, group = animal, 
             color = raça, 
             shape = raça, 
             linetype = raça)) +
  geom_line() +
  geom_point() +
  facet_wrap(~raça) # Representação por animal

mod %>%
  emmeans::emmip(raça ~ periodo) # Médias estimadas por periodo para cada raça

# Comparações múltiplas ao período fixando a raça
mp<-emmeans(mod, ~ periodo |raça, data=bovinos) 
pairs(mp, adjust="holm")

# Comparações múltiplas às raças fixando o periodo
mr<-emmeans(mod, ~raça |periodo, data=bovinos) 
pairs(mr, adjust="Tukey")

#### Caso a interacção não seja significativa ####
mp1<-emmeans(mod, ~ periodo, data=bovinos) 
pairs(mp1, adjust="holm")
mr1<-emmeans(mod, ~ raça, data=bovinos) 
pairs(mr1, adjust="tukey")


##### Falha a normalidade #######

bovinos1<-subset(bovinos, GB!="NA")
modelo_art <- art(GB ~ raça * periodo + (1 | animal), data = bovinos1) # Transformação dos dados para ranks alinhados
anova(modelo_art)  
## para periodos dentro da raça
bovinos_raca1 <- subset(bovinos1, raça == "Alentejana")
modelo_art_raca1 <- art(GB ~ periodo + (1 | animal), data = bovinos_raca1)
art.con(modelo_art_raca1, "periodo", adjust = "holm")
bovinos_raca2 <- subset(bovinos1, raça == "Frisia")
modelo_art_raca2 <- art(GB ~ periodo + (1 | animal), data = bovinos_raca2)
art.con(modelo_art_raca2, "periodo", adjust = "holm")
bovinos_raca3 <- subset(bovinos1, raça == "Mertolenga")
modelo_art_raca3 <- art(GB ~ periodo + (1 | animal), data = bovinos_raca3)
art.con(modelo_art_raca1, "periodo", adjust = "holm")
## para raças dentro de períodos
bovinos_periodo1 <- subset(bovinos1, periodo == "I")
modelo_art_periodo1 <- art(GB ~ raça, data = bovinos_periodo1)
art.con(modelo_art_periodo1, "raça", adjust = "holm")
bovinos_periodo2 <- subset(bovinos1, periodo == "II")
modelo_art_periodo2 <- art(GB ~ raça, data = bovinos_periodo2)
art.con(modelo_art_periodo2, "raça", adjust = "holm")
bovinos_periodo3 <- subset(bovinos1, periodo == "III")
modelo_art_periodo3 <- art(GB ~ raça, data = bovinos_periodo3)
art.con(modelo_art_periodo3, "raça", adjust = "holm")

# sem interação significativa #
art.con(modelo_art, "periodo", adjust = "holm")
art.con(modelo_art, "raça", adjust = "tukey")

#### Falha a homocedasticidade ####
bovinos1 <- subset(bovinos, GB!="NA")
modelo_lme <- lme(GB ~ raça * periodo, 
                  random = ~1 | animal, 
                  data = bovinos1, 
                  weights = varIdent(form = ~1 | raça))  # Permite variâncias diferentes para cada raça
anova(modelo_lme)
posthoc_lmep <- emmeans(modelo_lme, pairwise ~ periodo | raça, adjust = "holm")
print(posthoc_lmep)
posthoc_lmer <- emmeans(modelo_lme, pairwise ~ raça | periodo, adjust = "tukey")
print(posthoc_lmer)

# sem interação significativa #
posthoc_lmep <- emmeans(modelo_lme, pairwise ~ periodo, adjust = "holm")
print(posthoc_lmep)
posthoc_lmer <- emmeans(modelo_lme, pairwise ~ raça, adjust = "tukey")
print(posthoc_lmer)

# OBS: falhando ambos o ART é mais robusto #