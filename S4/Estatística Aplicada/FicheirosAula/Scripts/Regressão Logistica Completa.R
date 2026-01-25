library(tidyverse)
library(aod)
library(gtsummary)
library(ResourceSelection)
library(pROC)
library(ggplot2)
library(mfp)
library(broom)
library(Epi)
library(epiR)
library(sjPlot)
library(performance)
library(car)
library(caret)

# Limpar memória #
rm(list=ls(all=TRUE))
# Ler base de dados #
AF <- read.csv2("AF.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                dec=".", header=T, encoding = "UTF-8")
# Designação das variáveis na base de dados #
summary(AF)
# AF$numvezesporsemana[AF$numvezesporsemana==20]<-2 #corrigir gralha
summary(AF)
glimpse(AF)

########-------------------------------------------------------------------########
###################           Regressão simples                 ###################
########-------------------------------------------------------------------########

## ajustar modelo nulo ##
mod0<-glm(praticacaminhadas~1, family = binomial(link = logit), data=AF)
summary(mod0)

## variável numvezesporsemana ##
mod1<-glm(praticacaminhadas~numvezesporsemana, family = binomial(link = logit), data=AF)
summary(mod1)
anova(mod0, mod1, test="Chisq") # TRV
# p<0.0001

## variável conheceequipamentoscme ##
table(AF$praticacaminhadas, AF$conheceequipamentoscme)
mod2<-glm(praticacaminhadas~conheceequipamentoscme, family = binomial(link = logit), data=AF)
summary(mod2)
anova(mod0, mod2, test="Chisq") 
# p=0.3867

## variável utilizaecopista  ##
table(AF$praticacaminhadas, AF$utilizaecopista)
mod3<-glm(praticacaminhadas~utilizaecopista , family = binomial(link = logit), data=AF)
summary(mod3)
#ajustar modelos com os mesmos dados
mod0a<-glm(praticacaminhadas~1, family = binomial(link = logit), data=subset(AF, utilizaecopista!="NA"))
mod3a<-glm(praticacaminhadas~utilizaecopista , family = binomial(link = logit), data=subset(AF, utilizaecopista!="NA"))
anova(mod0a, mod3a, test="Chisq")
# p=0.6654

## variável sexo ##
table(AF$praticacaminhadas, AF$sexo)
mod4<-glm(praticacaminhadas~sexo, family = binomial(link = logit), data=AF)
summary(mod4)
anova(mod0, mod4, test="Chisq")
# p<0.0001 

## variável situacaoperantetrabalho ##
table(AF$praticacaminhadas, AF$situacaoperantetrabalho)
mod5<-glm(praticacaminhadas~situacaoperantetrabalho, family = binomial(link = logit), data=AF)
summary(mod5)
anova(mod0, mod5, test="Chisq")
# p<0.0001
# juntar a categoria outra e desempregado
library(forcats)
AF$trabalho<-fct_recode(AF$situacaoperantetrabalho, "Desempregado ou Outra"="Desempregado", "Desempregado ou Outra"="Outra")
summary(AF$trabalho)
mod5a<-glm(praticacaminhadas~trabalho, family = binomial(link = logit), data=AF)
summary(mod5a)

## variável instrucao ##
table(AF$instrucao, AF$praticacaminhadas)
mod6<-glm(praticacaminhadas~instrucao, family = binomial(link = logit), data=AF)
summary(mod6)
mod0a<-glm(praticacaminhadas~1, family = binomial(link = logit), data=subset(AF, instrucao!="NA"))
mod6a<-glm(praticacaminhadas~instrucao , family = binomial(link = logit), data=subset(AF, instrucao!="NA"))
anova(mod0a, mod6a, test="Chisq")
# p<0.0001

## variável tempodepratica ##
table(AF$tempodepratica, AF$praticacaminhadas)
mod7<-glm(praticacaminhadas~tempodepratica, family = binomial(link = logit), data=AF)
summary(mod7)
anova(mod0, mod7, test="Chisq")
# p<0.0001

## variável zonaresidencia ##
table(AF$zonaresidencia, AF$praticacaminhadas)
mod8<-glm(praticacaminhadas~zonaresidencia, family = binomial(link = logit), data=AF)
summary(mod8)
anova(mod0, mod8, test="Chisq")
# p=0.2026

## variável idade ##
mod9<-glm(praticacaminhadas~idade, family = binomial(link = logit), data=AF)
summary(mod9)
anova(mod0, mod9, test="Chisq")
# p<0.001


########-------------------------------------------------------------------########
###################          Regressão muúltipla                ###################
########-------------------------------------------------------------------########

## ajustar modelo com todas as significativas para valor p<0.20 e a variável utilizaecopista ##
mod10<-glm(praticacaminhadas~numvezesporsemana+ utilizaecopista+sexo+trabalho+ instrucao+ tempodepratica+ idade, family = binomial(link = logit), data=AF)
summary(mod10)

## retirar instrucao, pois é a que tem maior valor p das não significativas e tem 4 coeficientes ##
mod11<-glm(praticacaminhadas~numvezesporsemana+ utilizaecopista+sexo+trabalho+ tempodepratica+ idade, family = binomial(link = logit),data=subset(AF, instrucao!="NA"))
summary(mod11)
anova(mod11, mod10, test="Chisq") 

## retirar trabalho
mod11<-glm(praticacaminhadas~numvezesporsemana+ utilizaecopista+sexo+trabalho+ tempodepratica+ idade, family = binomial(link = logit),data=AF)
mod12<-glm(praticacaminhadas~numvezesporsemana+ utilizaecopista+sexo+ tempodepratica+ idade, family = binomial(link = logit),data=AF)
summary(mod12)
anova(mod12, mod11, test="Chisq") 
# não pode ser retirada, pois o valor p=0.0015

## incluir a variável conheceequipamentoscme que inicialmente não era significativa
mod13<-glm(praticacaminhadas~numvezesporsemana+ utilizaecopista+sexo+trabalho+ tempodepratica+ idade+conheceequipamentoscme, family = binomial(link = logit),data=AF)
summary(mod13)
anova(mod13, mod11, test="Chisq") 
# A variável não entra no modelo, pois valorp=0.1417

## incluir a variável zonaresidencia que inicalmente não era signifciativa
mod14<-glm(praticacaminhadas~numvezesporsemana+utilizaecopista+sexo+trabalho+ tempodepratica+ idade+zonaresidencia, family = binomial(link = logit),data=AF)
summary(mod14)
anova(mod14, mod11, test="Chisq") 
# A variável não entra no modelo, pois valor p=0.4354

### Tornar modelo mais parcimonioso ###
###.................................###
AF1=subset(AF, instrucao!="NA" & utilizaecopista!="NA") # retirar missings
mod11<-glm(praticacaminhadas~numvezesporsemana+ utilizaecopista+sexo+trabalho+ tempodepratica+ idade, family = binomial(link = logit),data=AF1)
# juntar tempode pratica 0-30 com 30-60
AF1$tempopraticanew<-fct_recode(AF1$tempodepratica, "(0, 60]"="(0, 30]", "(0, 60]"="(30, 60]")
AF1$tempopraticanew<-factor(AF1$tempopraticanew)
summary(AF1$tempopraticanew)
mod15<-glm(praticacaminhadas~numvezesporsemana+utilizaecopista+sexo+trabalho+ tempopraticanew+ idade, family = binomial(link = logit),data=AF1)
summary(mod15)
anova(mod15, mod11, test="Chisq")
# dois modelos não diferem estatisticamente p=0.1389

# juntar reformado e desempregado e outro
AF1$trabalhonew<-fct_recode(AF1$trabalho, "Ref/Desemp/Outro"="Reformado", "Ref/Desemp/Outro"="Desempregado ou Outra")
AF1$trabalhonew<-factor(AF1$trabalhonew)
summary(AF1$trabalhonew)
mod16<-glm(praticacaminhadas~numvezesporsemana+utilizaecopista+sexo+trabalhonew+ tempopraticanew+ idade, family = binomial(link = logit),data=AF1)
summary(mod16)
anova(mod16, mod15, test="Chisq")
# dois modelos não diferem estatisticamente p=0.9422

# juntar estudantes com trabalhador por conta própria
AF1$trabalhonew2<-fct_recode(AF1$trabalhonew, "TrabContaPrópria/Estudante"="Estudante", "TrabContaPrópria/Estudante"="Trabalhador por conta própria")
AF1$trabalhonew2<-factor(AF1$trabalhonew2)
summary(AF1$trabalhonew2)
mod17<-glm(praticacaminhadas~numvezesporsemana+utilizaecopista+sexo+trabalhonew2+ tempopraticanew+ idade, family = binomial(link = logit),data=AF1)
summary(mod17)
anova(mod17, mod16, test="Chisq")
# dois modelos não diferem estatisticamente p=0.42278

#### Linearidade com o logit ####
###---------------------------##
#### variável numvezessemana ####
# Interese reside em comparar que pratica até 3 vezes com os restantes
AF1$praticasemana<-ifelse(AF1$numvezesporsemana<=3, "Até 3 vezes", "Mais de 3 vezes")
AF1$praticasemana<-factor(AF1$praticasemana)
mod17a<-glm(praticacaminhadas~praticasemana+utilizaecopista+sexo+trabalhonew2+ tempopraticanew+ idade, family = binomial(link = logit),data=AF1)
summary(mod17a)

### Variável Idade ####
crPlot(mod17, variable = "idade")
partial_resid <- residuals(mod17, type = "partial") # Obter efeito parcial da variável no modelo completo
range(partial_resid[, "idade"], na.rm = TRUE)
# Identificar o máximo (outlier)
which.max(partial_resid[, "idade"])
AF1[which.max(partial_resid[, "idade"]), ]

plot(lowess(predict(mod17)~AF1$idade), type="l", xlab="Idade", ylab="logOdds")
mod17b<-mfp(praticacaminhadas~praticasemana+utilizaecopista+sexo+trabalhonew2+ tempopraticanew+ fp(idade), family = binomial(link = logit), data=AF1)
summary(mod17b)
# Nota: neste caso o modelo sugerido é igual ao inicial e portanto validamos a linearidade com o logit

## Interações ##
##............##
mod18<-glm(praticacaminhadas~praticasemana*utilizaecopista+sexo+trabalhonew2+ tempopraticanew+ idade, family = binomial(link = logit),data=AF1)
summary(mod18)
anova(mod17, mod18, test="Chisq")
# sig a 5% #

mod19<-glm(praticacaminhadas~praticasemana+utilizaecopista+praticasemana*sexo+trabalhonew2+ tempopraticanew+ idade, family = binomial(link = logit),data=AF1)
summary(mod19)
anova(mod17, mod19, test="Chisq")

mod20<-glm(praticacaminhadas~praticasemana+utilizaecopista+sexo+praticasemana*trabalhonew2+ tempopraticanew+ idade, family = binomial(link = logit),data=AF1)
summary(mod20)
anova(mod17, mod20, test="Chisq")

mod21<-glm(praticacaminhadas~praticasemana+utilizaecopista+sexo+trabalhonew2+ praticasemana*tempopraticanew+ idade, family = binomial(link = logit),data=AF1)
summary(mod21)
anova(mod17, mod21, test="Chisq")

mod22<-glm(praticacaminhadas~praticasemana+utilizaecopista+sexo+trabalhonew2+ tempopraticanew+ praticasemana*idade, family = binomial(link = logit),data=AF1)
summary(mod22)
anova(mod17, mod22, test="Chisq")

mod23<-glm(praticacaminhadas~praticasemana+utilizaecopista*sexo+trabalhonew2+ tempopraticanew+ idade, family = binomial(link = logit),data=AF1)
summary(mod23)
anova(mod17, mod23, test="Chisq")

mod24<-glm(praticacaminhadas~praticasemana+utilizaecopista+sexo+utilizaecopista*trabalhonew2+ tempopraticanew+ idade, family = binomial(link = logit),data=AF1)
summary(mod24)
anova(mod17, mod24, test="Chisq")

mod25<-glm(praticacaminhadas~praticasemana+utilizaecopista+sexo+trabalhonew2+ utilizaecopista*tempopraticanew+ idade, family = binomial(link = logit),data=AF1)
summary(mod25)
anova(mod17, mod25, test="Chisq")

mod26<-glm(praticacaminhadas~praticasemana+utilizaecopista+sexo+trabalhonew2+ tempopraticanew+utilizaecopista*idade, family = binomial(link = logit),data=AF1)
summary(mod26)
anova(mod17, mod26, test="Chisq")
# sig a 5%

mod27<-glm(praticacaminhadas~praticasemana+utilizaecopista+sexo*trabalhonew2+ tempopraticanew+idade, family = binomial(link = logit),data=AF1)
summary(mod27)
anova(mod17, mod27, test="Chisq")

mod28<-glm(praticacaminhadas~praticasemana+utilizaecopista+sexo+trabalhonew2+ sexo*tempopraticanew+idade, family = binomial(link = logit),data=AF1)
summary(mod28)
anova(mod17, mod28, test="Chisq")
# sig a 5%

mod29<-glm(praticacaminhadas~praticasemana+utilizaecopista+sexo+trabalhonew2+ tempopraticanew+sexo*idade, family = binomial(link = logit),data=AF1)
summary(mod29)
anova(mod17, mod29, test="Chisq")

mod30<-glm(praticacaminhadas~praticasemana+utilizaecopista+sexo+trabalhonew2*tempopraticanew+idade, family = binomial(link = logit),data=AF1)
summary(mod30)
anova(mod17, mod30, test="Chisq")

mod31<-glm(praticacaminhadas~praticasemana+utilizaecopista+sexo+trabalhonew2+tempopraticanew+trabalhonew2*idade, family = binomial(link = logit),data=AF1)
summary(mod31)
anova(mod17, mod31, test="Chisq")

mod32<-glm(praticacaminhadas~praticasemana+utilizaecopista+sexo+trabalhonew2+tempopraticanew*idade, family = binomial(link = logit),data=AF1)
summary(mod32)
anova(mod17, mod32, test="Chisq")

#modelo com 3 interações
mod33<-glm(praticacaminhadas~praticasemana*utilizaecopista+sexo+trabalhonew2+ sexo*tempopraticanew+utilizaecopista*idade, family = binomial(link = logit),data=AF1)
summary(mod33)
mod33a<-glm(praticacaminhadas~praticasemana*utilizaecopista+sexo+ sexo*tempopraticanew+utilizaecopista*idade, family = binomial(link = logit),data=AF1)
summary(mod33a)
anova(mod33a, mod33, test="Chisq")

mod34<-glm(praticacaminhadas~praticasemana*utilizaecopista+sexo+trabalhonew2+ tempopraticanew+utilizaecopista*idade, family = binomial(link = logit),data=AF1)
summary(mod34)
mod34a<-glm(praticacaminhadas~praticasemana*utilizaecopista+sexo+ tempopraticanew+utilizaecopista*idade, family = binomial(link = logit),data=AF1)
summary(mod34a)
anova(mod34, mod34a, test="Chisq")
anova(mod34, mod33, test="Chisq")
# sexo*tempoprtica valor p=0.03475

mod35<-glm(praticacaminhadas~praticasemana*utilizaecopista+sexo*tempopraticanew+trabalhonew2+ tempopraticanew+idade, family = binomial(link = logit),data=AF1)
summary(mod35)
anova(mod35, mod33, test="Chisq")
# utilizaecopista*idade valor p=0.0208

mod36<-glm(praticacaminhadas~praticasemana+sexo*tempopraticanew+trabalhonew2+ tempopraticanew+utilizaecopista*idade, family = binomial(link = logit),data=AF1)
summary(mod36)
mod36a<-glm(praticacaminhadas~praticasemana+sexo*tempopraticanew+ tempopraticanew+utilizaecopista*idade, family = binomial(link = logit),data=AF1)
summary(mod36a)
anova(mod36, mod36a, test="Chisq")
anova(mod36, mod33, test="Chisq")
# utilizaecopista*numvezessemana valor p=0.0408


#### Bondade de Ajustamento e Capacidade Discriminativa ####
AF1$resp<-as.numeric(AF1$praticacaminhadas)-1
(hl<-hoslem.test(AF1$resp, fitted(mod33), g = 10))
hl$expected
(hl<-hoslem.test(AF1$resp, fitted(mod34), g = 10))
hl$expected
(hl<-hoslem.test(AF1$resp, fitted(mod35), g = 10))
hl$expected
(hl<-hoslem.test(AF1$resp, fitted(mod36), g = 10))
hl$expected
# maior valor p do modelo 35

## Teste de Cessie-van Houwelingen (quando existem variáveis contínuas) ##
fit1 <- lrm(praticacaminhadas~praticasemana*utilizaecopista+sexo+trabalhonew2+ sexo*tempopraticanew+utilizaecopista*idade , data=AF1, x=TRUE, y=TRUE)
resid(fit1, 'gof')
print(fit1)
fit2 <- lrm(praticacaminhadas~praticasemana*utilizaecopista+sexo+trabalhonew2+ tempopraticanew+utilizaecopista*idade, data=AF1, x=TRUE, y=TRUE)
resid(fit2, 'gof')
print(fit2)
fit3 <- lrm(praticacaminhadas~praticasemana*utilizaecopista+sexo*tempopraticanew+trabalhonew2+ tempopraticanew+idade, data=AF1, x=TRUE, y=TRUE)
resid(fit3, 'gof')
print(fit3)
fit4 <- lrm(praticacaminhadas~praticasemana+sexo*tempopraticanew+trabalhonew2+ tempopraticanew+utilizaecopista*idade, data=AF1, x=TRUE, y=TRUE)
resid(fit4, 'gof')
print(fit4)


### Admitindo que o modelo 36 é o que faz mais sentido 
modf<-mod36
summary(modf)
ROC(form=praticacaminhadas~praticasemana+sexo*tempopraticanew+trabalhonew2+ tempopraticanew+utilizaecopista*idade+ tempopraticanew+ idade,data=AF1, plot="ROC",PV=T,MX=T,AUC=T)
# Modelo com capacidade discriminativa muito bom AUC=0,868
# Para um ponto de corte igual a 0,393 obtem-se uma sensibilidade de 74,5% e uma especificidade de 85,1%
library(pROC)
ci.auc(AF1$resp, fitted(modf), conf.level = 0.95) # intervalo de confiança para AUC
# IC95% (AUC)=(0.829, 0.906)

##### Análise de resíduos ######
AF1$resp <- as.numeric(AF1$praticacaminhadas)-1 # variável resposta como numérica de zeros e uns 
(modelo.mf <- model.frame(modf)) # Extrair os padrões de covariáveis #
(modelo.cp <- epi.cp(modelo.mf[-1])) # Neste modelo temos 282 padrões e 346 indivíduos 
modelo.obs <- as.vector(by(AF1$resp, as.factor(modelo.cp$id), FUN = sum)) # número de sucessos por padrão
modelo.fit <- as.vector(tapply(fitted(modf), as.factor(modelo.cp$id), min)) # probabilidade estimada de cada padrão das covariáveis 
(modelo.res <- epi.cpresids(obs = modelo.obs, fit = modelo.fit, covpattern = modelo.cp)) ## resíduos, deltabetas e deltaquis 

plot(modelo.fit, modelo.res$sdeltabeta, xlab="Probabilidades Estimadas", ylab="Distância de Cook") # Distância de Cook
identify (modelo.fit, modelo.res$sdeltabeta) # identificar padrões que se destacam 
subset(modelo.mf, modelo.cp$id==220) # indivíduos com o padrão 220

raio <- sqrt(modelo.res$deltabeta/pi) # dimensionar correctamente por área
Delta_Dev<-modelo.res$deviance^2/(1-modelo.res$leverage)
symbols(modelo.fit, Delta_Dev, circles=raio, inches=0.35, xlab="Probabilidades estimadas", ylab="Alteração na Deviance")
text(modelo.fit, Delta_Dev, modelo.res$cpid, cex=0.5) # mostrar padrão correspondente

## impacto do id 25 ##
AFsem25<-subset(AF1, id!=25)
mod36a<-glm(praticacaminhadas~praticasemana+sexo*tempopraticanew+trabalhonew2+ tempopraticanew+utilizaecopista*idade, family = binomial(link = logit),data=AFsem25)
((coef(mod36)-coef(mod36a))/coef(mod36))*100 ## alteração dos coeficientes
((deviance(mod36)-deviance(mod36a))/deviance(mod36))*100 ## alteração da deviance
# Nota: apenas impacto residual na deviance 
# maior impacto nos parâmetros é de 81,3%

## impacto do padrão 220 ##
subset(modelo.mf, modelo.cp$id==220) 
linhas_a_excluir <- c(273, 277, 313, 319)
AF1sem220<-AF1[-linhas_a_excluir,]
mod36a<-glm(praticacaminhadas~praticasemana+sexo*tempopraticanew+trabalhonew2+ tempopraticanew+utilizaecopista*idade+ tempopraticanew+ idade , family = binomial(link = logit), data=AF1sem220)
((coef(mod36)-coef(mod36a))/coef(mod36))*100 ## alteração dos coeficientes
((deviance(mod36)-deviance(mod36a))/deviance(mod36))*100 ## alteração da deviance
# Nota: apenas impacto residual na deviance 
# maior impacto nos parâmetros é de 32,9%


#####-------------------------#####
##### interpretação do modelo #####
##### ------------------------#####

interpretar_OR <- function(modelo, pos) {
  mc <- vcov(modelo)
  beta <- coef(modelo)[pos]
  se <- sqrt(mc[pos, pos])
  OR <- exp(beta)
  IC95 <- exp(c(beta - 1.96 * se, beta + 1.96 * se))
  cat("OR =", round(OR, 2), "| IC95% =", round(IC95[1], 2), "-", round(IC95[2], 2), "\n")
}

interpretar_inv_OR <- function(modelo, pos) {
  mc <- vcov(modelo)
  beta <- coef(modelo)[pos]
  se <- sqrt(mc[pos, pos])
  OR <- 1/exp(beta)
  IC95 <- 1/exp(c(beta - 1.96 * se, beta + 1.96 * se))
  cat("OR =", round(OR, 2), "| IC95% =", round(IC95[1], 2), "-", round(IC95[2], 2), "\n")
}

cat("\nEfeito da frequência de prática (praticasemana):\n")
interpretar_OR(modf, 2)
# Quem pratica AF mais de 3 vezes tem cerca de 5 vezes mais possibilidade de praticar caminhadas IC95%(OR)=(2.8, 9.5)

cat("\nEfeito do grupo de trabalho (trabalhonew2):\n")
interpretar_inv_OR(modf, 5)
# Reformados ou Desempregrados têm quase 3 vezes mais possibilidade de praticar caminhadas relativamente aos restantes ecxetp TCO IC95%(OR)=(1.1, 8.2)

cat("\nEfeito do sexo (até 60 minutos):\n")
interpretar_inv_OR(modf, 3)
# o efeito do sexo não é significativo

cat("\nEfeito do sexo (mais de 60 minutos):\n")
a <- 3; b <- 9; mc <- vcov(modf)
sd <- sqrt(mc[a, a] + mc[b, b] + 2 * mc[a, b])
OR <- 1/exp(coef(modf)[a] + coef(modf)[b])
IC95 <- 1/exp(c(coef(modf)[a] + coef(modf)[b] - 1.96 * sd, coef(modf)[a] + coef(modf)[b] + 1.96 * sd))
cat("OR =", round(OR, 2), "| IC95% =", round(IC95[1], 2), "-", round(IC95[2], 2), "\n")
# Sexo Feminino tem 6.6 vezes mais possibilidades de praticar caminahdas que o sexo masculino IC95%(OR)=(2.1, 21.1)


# Tempo de prática
# sexo feminino #
# valor p = 0.707, logo o efeito do tempo de prática não é significativo

# sexo masculino
a<-4
b<-9
sd<-sqrt(mc[a,a]+mc[b,b]+2*mc[a,b])
1/c(exp(coef(modf)[a]+coef(modf)[b]-1.96*sd), exp(coef(modf)[a]+coef(modf)[b]), exp(coef(modf)[a]+coef(modf)[b]+1.96*sd))
# Quem pratica até 60 minutos tem 5 vezes mais possibilidades de praticar caminahdas do que quem pratica mais tempo IC95%(OR)=(1.9, 13.4)

# Idade
# Não utilizadores de ecopista #
# valor p = 0.079, logo o efeito da idade não é significativo

# utilizadores de ecopista
a<-8
b<-10
inc_idade<-seq(1, 20, 1)
sd<-inc_idade*sqrt(mc[a,a]+mc[b,b]+2*mc[a,b])
lior<-exp(inc_idade*(coef(modf)[a]+coef(modf)[b])-1.96*sd)
or<-exp(inc_idade*(coef(modf)[a]+coef(modf)[b]))
lsor<-exp(inc_idade*(coef(modf)[a]+coef(modf)[b])+1.96*sd)
(dt<-data.frame(inc_idade, lior, or, lsor))
library(ggplot2)
ggplot(dt, aes(x =inc_idade , y = or)) + 
  geom_ribbon(aes(ymin = lior,ymax = lsor),
              alpha = 0.2) + geom_line(aes(colour = or), size = 1)+
  xlab("Diferenças de idade")+ylab("OR")+
  scale_x_continuous(breaks = seq(1,20,1))+scale_y_continuous(breaks = seq(0,8,1))+
  theme(legend.position="none")


# Utiliza ecopista
a<-7
b<-10
summary(AF1$idade)
id<-seq(15, 86, 1)
sd<-sqrt(mc[a,a]+id*id*mc[b,b]+2*id*mc[a,b])
lior<-exp(coef(modf)[a]+id*coef(modf)[b]-1.96*sd)
or<-exp(coef(modf)[a]+id*coef(modf)[b])
lsor<-exp(coef(modf)[a]+id*coef(modf)[b]+1.96*sd)
(dt<-data.frame(id, lior, or, lsor))
library(ggplot2)
ggplot(dt, aes(x =id , y = or)) + 
  geom_ribbon(aes(ymin = lior,ymax = lsor),
              alpha = 0.2) + geom_line(aes(colour = or), size = 1)+
  xlab("Idade")+ylab("OR")+
  scale_x_continuous(expand = c(0,0), limit = c(15, 86), breaks = seq(15,86,5))+scale_y_continuous(breaks = seq(0,40,2))+
  geom_hline(yintercept=1, color="red", linetype='dashed')+
  theme(legend.position="none")
# efeito da ecopista significativo a partir dos 50 anos

## Perfis de Probabilidade com bandas de confiança ##
summary(modf)
novosdados <- with(AF1, data.frame(praticasemana="Até 3 vezes", 
                                   sexo="Feminino", 
                                   tempopraticanew="(0, 60]", 
                                   trabalhonew2="TrabContaPrópria/Estudante",
                                   utilizaecopista="Não",
                                   idade=21))
predições <- cbind(novosdados, predict(modf, newdata = novosdados, type = "link",se = TRUE))
pred_com_ic <- within(predições, { 
  Prob <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
cat(sprintf("Probabilidade estimada de praticar caminhadas: %.2f%%\n", pred_com_ic$Prob*100))
cat("Limite Inferior do Intervalo de confiança a 95%: ", sprintf("%.2f%%\n", pred_com_ic$LL*100), sep="")
cat("Limite Superior do Intervalo de confiança a 95%: ", sprintf("%.2f%%\n", pred_com_ic$UL*100), sep="")

#### Predição ####
  
set.seed(123) 
# Dados de treino e dados de teste
train_index <- createDataPartition(AF1$praticacaminhadas, p = 0.7, list = FALSE)
train_data <- AF1[train_index, ]
test_data  <- AF1[-train_index, ]
mod_train <- glm(praticacaminhadas~praticasemana+sexo*tempopraticanew+trabalhonew2+ tempopraticanew+utilizaecopista*idade, family = binomial(link = logit), data = train_data)
pred_probs_test <- predict(mod_train, newdata = test_data, type = "response")
pred_class_test <- ifelse(pred_probs_test >0.178 , "Sim", "Não")
conf_matrix_test <- confusionMatrix(as.factor(pred_class_test), as.factor(test_data$praticacaminhadas), positive = "Sim")
print(conf_matrix_test)

## Como obter o ponto de corte ótimo pelo critério de Youden ##
roc_curve <- roc(response = test_data$praticacaminhadas,
                 predictor = pred_probs_test,
                 levels = c("Não", "Sim"),  # "Não" = classe negativa, "Sim" = positiva
                 direction = "<")  
youden_result <- coords(roc_curve,
                        x = "best",
                        best.method = "youden",
                        ret = c("threshold", "sensitivity", "specificity", "ppv", "npv", "accuracy", "fpr", "tpr"))
print(youden_result)
optimal_cutoff <- as.numeric(youden_result["threshold"])

