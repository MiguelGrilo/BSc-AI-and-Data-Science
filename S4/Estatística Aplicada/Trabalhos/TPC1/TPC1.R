# Miguel Grilo    58387
# Jorge Couto     58656

##### SCRIPT EMPRESAS ####

emp <- read.csv2("Empresas.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                    sep=";", dec=",", header=T, encoding = "UTF-8")
summary(emp)

library(lme4)
mod_aleat <- lmer(Produtividade ~ (1 | Empresa), data = emp) 
summary(mod_aleat)
plot(mod_aleat) # Homocedasticidade #
# Observando o gráfico verificamos que não existe afunilamento então podemos 
# admitir homocedasticidade

library(car)
qqPlot(resid(mod_aleat)) ## QQplot para a normalidade
# A partir da observação gráfica não rejeitamos diretamente a normalidade pois 
# não existem desvios significantes embora existam dois possiveis outliers 
# identificados com os números 70 e 72, então seguimos para o teste de 
# Shapiro-Wilk para confirmar
shapiro.test(resid(mod_aleat)) # Shapiro-Wilk
# Valor p = 0.7242 > 0.05
# Assim, o valor p é maior que alfa e admitimos normalidade
(Efeitos_Aleatórios=as.data.frame(ranef(mod_aleat)))
# Analisando os valores apresentados na coluna condval concluimos que as
# empresas A, C, E e F têm produtividade acima da média enquanto as empresas
# B, D, G e H têm produtividade abaixo da média
# Analisando agora a coluna condsd concluímos que o valor é constante para
# todas as empresas, ou seja, os efeitos aleatórios das empresas desviam 
# aproximadamente 0.6609626
qqPlot(Efeitos_Aleatórios$condval) #Efeitos aleatórios
# Após análise do gráfico identificamos dois possiveis outliers identificados
# com os números 3 e 6, no entanto, como ainda se encontram dentro da banda de 
# significância, podemos admitir que os efeitos aleatórios seguem uma 
# distribuição aproximadamente normal

mod_aleat <- lmer(Produtividade ~ (1 | Empresa), data = emp, REML = F) 
mod_fix <- lm(Produtividade ~ 1, data = emp) # Modelo sem efeito aleatório
library(RLRsim)
exactLRT(mod_aleat, mod_fix)
# Valor p < 2.2e-16 < 0.05
# H0: Produtividade dos funcionários afetada somente por variações individuais
# H1: Produtividade dos funcionários depende da empresa onde trabalham
# Assim, rejeitamos H0 e concluímos que a produtividade dos funcionários 
# depende da empresa onde trabalham.