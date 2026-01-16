library(tidyverse)
library(aod)
library(gtsummary)
library(ResourceSelection)
library(pROC)
library(ggplot2)
library(mfp)
library(broom)
library(Epi)
library(sjPlot)
library(performance)

rm(list = ls(all = TRUE))

# Carregar base de dados
AF <- read.csv2("AF.csv", stringsAsFactors = TRUE, na.strings = c("NULL", "", "NA"),
                dec = ".", header = TRUE, encoding = "UTF-8")

# Visualização inicial
glimpse(AF)
summary(AF)

# Modelo nulo (apenas intercepto)
mod0 <- glm(praticacaminhadas ~ 1, family = binomial(link = "logit"), data = AF)
summary(mod0)

# Sexo
mod1 <- glm(praticacaminhadas ~ sexo, family = binomial(link = "logit"), data = AF)
summary(mod1)
anova(mod0, mod1, test = "Chisq") # Teste da Razão de Verosimilhanças

# tempodepratica
mod2 <- glm(praticacaminhadas ~ tempodepratica, family = binomial(link = "logit"), data = AF)
summary(mod2)
anova(mod0, mod2, test = "Chisq")  

# tempodepratica
mod3 <- glm(praticacaminhadas ~ idade, family = binomial(link = "logit"), data = AF)
summary(mod3)
anova(mod0, mod3, test = "Chisq") 

# Modelo com múltiplas variáveis
mod4 <- glm(praticacaminhadas ~ tempodepratica + sexo + idade, family = binomial(link = "logit"), data = AF)
summary(mod4)

# Component+Residual Plots #
crPlot(mod3, variable = "idade")
## ---------- Método dos polinómios fraccionários  -----------  ##
mod4p<-mfp(praticacaminhadas ~ tempodepratica + sexo + fp(idade), family = binomial(link = logit), data=AF)
summary(mod4p)

mod5 <- glm(praticacaminhadas ~ tempodepratica + sexo + I((idade/100)^-0.5), family = binomial(link = "logit"), data = AF)
summary(mod5)

# Teste de Hosmer-Lemeshow
AF$resp <- as.numeric(AF$praticacaminhadas) - 1
hoslem.test(AF$resp, fitted(mod4), g = 10)
hoslem.test(AF$resp, fitted(mod5), g = 10)

# Curva ROC
ROC(form=praticacaminhadas ~ tempodepratica + sexo + I((idade/100)^-0.5), data=AF, plot="ROC",PV=T,MX=T,AUC=T)
ci.auc(as.numeric(AF$praticacaminhadas), fitted(mod4), conf.level = 0.95)

# Analisar resíduos # 
resp <- as.numeric(AF$praticacaminhadas)-1 # variável resposta como numérica de zeros e uns 
(modelo.mf <- model.frame(mod5)) # Extrair os padrões de covariáveis #
library(epiR)
(modelo.cp <- epi.cp(modelo.mf[-1])) # Neste modelo temos 198 padrões e 363indivíduos 
modelo.obs <- as.vector(by(resp, as.factor(modelo.cp$id), FUN = sum)) # número de sucessos por padrão
modelo.fit <- as.vector(tapply(fitted(mod5), as.factor(modelo.cp$id), min)) # probabilidade estimada de cada padrão das covariáveis 
(modelo.res <- epi.cpresids(obs = modelo.obs, fit = modelo.fit, covpattern = modelo.cp)) ## resíduos, deltabetas e deltaquis 

plot(modelo.fit, modelo.res$sdeltabeta, xlab="Probabilidades Estimadas", ylab="Distância de Cook") # Distância de Cook
identify (modelo.fit, modelo.res$sdeltabeta) # identificar padrões que se destacam 
subset(modelo.mf, modelo.cp$id==157) # indivíduos com o padrão 157

raio <- sqrt(modelo.res$deltabeta/pi) # dimensionar correctamente por área
symbols(modelo.fit, Delta_Dev, circles=raio, inches=0.35, xlab="Probabilidades estimadas", ylab="Alteração na Deviance")
text(modelo.fit, Delta_Dev, modelo.res$cpid, cex=0.5) # mostrar padrão correspondente


# Interpretação com IC dos OR #
tbl_regression(mod4, exponentiate = TRUE)

# Gráfico dos OR com IC
plot_model(mod5, show.values = TRUE, value.offset = .3, vline.color = "red")

# Gráfico das probabilidades preditas por categoria de tempodepratica
AF$pred <- predict(mod5, type = "response")
ggplot(AF, aes(x = tempodepratica, y = pred)) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "darkblue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "darkblue") +
  labs(title = "Probabilidade estimada de praticar caminhadas",
       x = "Tempo de prática", y = "Probabilidade estimada") +
  theme_minimal()+coord_flip()


