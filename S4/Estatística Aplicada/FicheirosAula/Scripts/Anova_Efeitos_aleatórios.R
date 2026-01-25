teares <- read.csv2("Teares.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                    sep=";", dec=",", header=T, encoding = "UTF-8")
summary(teares)

library(lme4)
mod_aleat <- lmer(Resistência ~ (1 | Tear), data = teares) 
summary(mod_aleat)
plot(mod_aleat) # Homocedasticidade #   
library(car)
qqPlot(resid(mod_aleat)) ## QQplot para a normalidade
shapiro.test(resid(mod_aleat)) # Shapiro-Wilk
(Efeitos_Aleatórios=as.data.frame(ranef(mod_aleat)))
qqPlot(Efeitos_Aleatórios$condval) #Efeitos aleatórios 
mod_aleat <- lmer(Resistência ~ (1 | Tear), data = teares, REML = F) 
mod_fix <- lm(Resistência ~ 1, data = teares) # Modelo sem efeito aleatório
library(RLRsim)
exactLRT(mod_aleat, mod_fix)
