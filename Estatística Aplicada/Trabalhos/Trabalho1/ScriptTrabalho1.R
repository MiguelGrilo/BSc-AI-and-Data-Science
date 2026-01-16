##### Trabalho 1 #####
### João Lopes    58358
### Miguel Grilo  58387
### Jorge Couto   58656

rm(list=ls(all=TRUE))

###---------------###
#### EXERCÍCIO 1 ####
###---------------###
# ANOVA 1 Fator
feridas <- read.csv2("Feridas.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                     sep=";", dec=",", header=T, encoding = "UTF-8")
summary(feridas)
names(feridas)
# Modelo ANOVA com 'Duração' como variável contínua e 'Tipologia' como variável categórica
mod <-aov(Duração ~  Tipologia,  data=feridas) 
# Resíduos standardizados do modelo
rs<-rstandard(mod) 

# Normalidade
library(car)
qqPlot(rs)
# Analisando a normalidade graficamente com recurso ao qqPlot dos resíduos 
# rejeitamos de imediato a normalidade, observando inúmeros outliers
# Ainda assim seguimos para o teste de Shapiro-Wilk aos resíduos meramente para 
# descargo de consciência, para que esta conclusão seja corroborada
shapiro.test(rs)
# Valor p < 2.2e-16 , ou seja,
# Valor p < 0.001
# Rejeitamos H0
# O teste de Shapiro-Wilk corrobora a nossa conclusão anterior e rejeitamos a 
# normalidade, pelo menos por agora...
# Seguimos então para os testes de D'Agostino e Anscombe-Glynn aos resíduos
library(moments)
agostino.test(rs)
# Valor p < 2.2e-16 , ou seja,
# Valor p < 0.001
# Rejeitamos H0
anscombe.test (rs)
# Valor p < 2.2e-16 , ou seja,
# Valor p < 0.001
# Rejeitamos H0
# Após os resultados obtidos pelos testes de D'Agostino e Anscombe-Glynn aos
# resíduos concluímos que de facto não apresenta distribuição normal, sendo 
# rejeitada, e avançamos para a análise do último pressuposto

#Homogeneidade de Variâncias
plot(mod$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")
abline(h=0)
# Analisando graficamente a homocedasticidade com recurso ao plot dos valores 
# ajustados pelos resíduos observamos um claro padrão e afunilamento, permitindo
# rejeitar de imediato o pressuposto da homocedasticidade
# No entanto, este gráfico sugere que os valores estão separados de forma 
# categórica, então seguimos para o teste de Levene, em busca de que o valor p 
# dado corrobore a conclusão anterior
leveneTest(feridas$Duração~feridas$Tipologia)
# Valor p = 0.001735 , ou seja,
# Valor p < 0.001
# Rejeitamos H0
# Após os resultados obtidos pelo teste de Levene concluímos que este corrobora 
# a nossa conclusão anterior e rejeitamos, de facto, a homocedasticidade

# Assim, rejeitamos ambos os pressupostos da ANOVA 1 Fator, a normalidade e 
# homocedasticidade, sendo então necessário seguir por uma de duas abordagens, 
# recorrer a transformações para obter um modelo em que idealmente admitimos 
# ambos os pressupostos ou em que admitimos pelo menos um destes, ou seguir a 
# abordagem de último recurso caso nenhuma transformação resulte, o teste de 
# Games-Howell


# A primeira a ser testada é a Transformação Logaritmo
# Modelo ANOVA
modlog<-aov(log(Duração)~Tipologia, feridas)
# Resíduos
rslog<-rstandard (modlog)

# Normalidade
qqPlot(rslog)
# Observamos com clareza vários outliers no qqPlot dos resíduos, no entanto
# realizamos o teste de Shapiro-Wilk aos resíduos para confirmar esta conclusão
shapiro.test(rslog)
# Valor p = 2.189e-05 , ou seja,
# Valor p < 0.001
# Rejeitamos H0
# O teste de Shapiro confirma a nossa conlusão anterior e não admitimos a 
# normalidade por agora
# Avançamos então para os testes de D'Agostino e Anscombe-Glynn aos resíduos
library(moments)
agostino.test(rslog)
# Valor p = 0.0003598 , ou seja,
# Valor p < 0.001
# Rejeitamos H0
anscombe.test (rslog)
# Valor p = 0.02105 < 0.05
# Rejeitamos H0
# Rejeitando ambos os testes concluímos então que a normalidade é, de facto, 
# rejeitada

# Homocedasticidade
plot(modlog$fit,  rslog,  xlab="valores  ajustados",  ylab="resíduos")
# Pela observação do gráfico existe novamente um agrupamento dos valores mas 
# desta vez em apenas dois pontos dos valores ajustados, dificultando a análise 
# gráfica da homocedasticidade, mas ainda assim sugerindo a rejeição da mesma
leveneTest(log(Duração) ~ Tipologia, data = feridas)
# Valor p = 0.000282 , ou seja, 
# Valor p < 0.001
# Rejeitamos H0
# Assim, rejeitamos também a homocedasticade e concluímos que ambos os 
# pressupostos são novamente rejeitados, logo esta transformação não tem 
# utilidade para estudo


# A segunda a ser testada é a Transformação Raiz
# Modelo ANOVA
modr <- aov(sqrt(Duração)~Tipologia, feridas)
# Resíduos
rsr<-rstandard (modr)

# Normalidade
qqPlot(rsr)
# Apenas através da observação gráfica, voltamos a poder rejeitar de imediato a
# normalidade, dado o elevado número de outliers

#Homocedasticidade
plot(modr$fit,  rsr,  xlab="valores  ajustados",  ylab="resíduos")
# Existe mais uma vez agrupação em 3/4 valores mas conseguimos observar um 
# afunilamento então rejeitamos a homocedasticidade e continuamos a rejeitar 
# ambos os pressupostos, indicando que esta transformação também não tem uso 
# para o nosso estudo


# A terceira e última a ser testada é a Transformação Inversa
# Modelo ANOVA
modi<-aov(1/(Duração)~Tipologia, feridas)
# Resíduos
rsi<-rstandard (modi)

# Normalidade
qqPlot(rsi)
# A partir da observção gráfica rejeitamos obviamente a normalidade, sendo esta
# a pior transformação, apresentando inúmeros e muito mais evidentes outliers
shapiro.test(rsi)
# Valor p < 2.2e-16 , ou seja, 
# Valor p < 0.001
# Rejeitamos H0
# Não admitimos então a normalidade novamente

# Homocedasticidade
plot(modi$fit,  rsi,  xlab="valores  ajustados",  ylab="resíduos")
# Mais uma vez estão agrupados categoriacamente mas podemos mesmo assim observar
# afunilamento e rejeitar a homocedasticidade
leveneTest(1/Duração~Tipologia, data = feridas)
# Valor p = 5.56e-11 , ou seja,
# Valor p < 0.001
# Rejeitamos H0
# Rejeitamos novamente ambos os pressupostos na última transformação a ser 
# testada então não iremos recorrer a transformações para obter uma conclusão

# Assim, rejeitamos ambos os pressupostos e seguimos para o teste de Games-Howell
library(rstatix)
games_howell_test (feridas, Duração~Tipologia)
# Existem as seguintes diferenças significativas:
# Ferida cirúrgica vs Outra       , valor p = 0.003 < 0.05
# Ferida traumática vs Outra      , valor p = 0.000655 < 0.001
# Ferida traumática vs UP/LH/LPT  , valor p = 0.034 < 0.05

# μ(Outra) - μ(Ferida cirúrgica) = 441.0
# μ(Outra) - μ(Ferida traumática) = 491.0
# μ(UP/LH/LPT) - μ(Ferida traumática) = 410.0

# μ(Outra) > μ(Ferida cirúrgica)
# μ(Outra) > μ(Ferida traumática)
# μ(UP/LH/LPT) > μ(Ferida traumática)

# Concluindo, a tipologia da ferida está associada à sua duração e as feridas de
# tipologia 'Outra' e 'UP/LH/LPT' tendem a apresentar maior duração.


# Terminada a análise conforme a abordagem correta, seguimos para a análise das
# abordagens incorretas, para comparar as conclusões

# Caso se admitisse a normalidade e homocedasticidade, ou seja, os pressupostos
# se verificassem, seguiriamos pelo teste de Tukey para comparações múltiplas:
TukeyHSD(mod)
# Existem as seguintes diferenças significativas:
# Outra vs Ferida cirúrgica       , valor p = 0.0041666 < 0.05
# Outra vs Ferida traumática      , valor p = 0.0075810 < 0.05

# μ(Outra) - μ(Ferida cirúrgica) = 441.19610  
# μ(Outra) - μ(Ferida traumática) = 490.93868   

# μ(Outra) > μ(Ferida cirúrgica)
# μ(Outra) > μ(Ferida traumática)

# Concluindo, a tipologia da ferida está associada à sua duração e as feridas de
# tipologia 'Outra' tendem a apresentar maior duração. Assim, esta abordagem
# diferia um pouco da abordagem correta.


# Caso se admitisse a normalidade e falhasse homocedasticidade, seguiriamos pelo
# teste de Welch para verificar a diferença entre médias:
oneway.test(Duração ~ Tipologia, data = feridas)
# Valor p = 1.952e-05 < 0.001
# Rejeitamos H0
# Assim, concluímos que pelo menos 2 grupos diferem entre si, seguimos então para
# o teste de comparação múltipla de Tukey com estimativas robustas das 
# variâncias: método max-t:
library (multcomp)
library(sandwich)
mcomp <- glht(aov(Duração ~  Tipologia,  data=feridas), mcp(Tipologia="Tukey"), vcov=vcovHC)
# O argumento vcov = vcovHC especifica o uso da estimativa HC3, consistente no caso de heterocedasticidade.
summary(mcomp)
confint (mcomp)
plot(mcomp)
# Existem as seguintes diferenças significativas:
# Outra vs Ferida cirúrgica       , valor p = 0.002481 < 0.05
# Outra vs Ferida traumática      , valor p = 0.000404 < 0.001
# UP/LH/LPT vs Ferida traumática  , valor p = 0.022216 < 0.05

# μ(Outra) - μ(Ferida cirúrgica) = 441.20     
# μ(Outra) - μ(Ferida traumática) = 490.94     
# μ(UP/LH/LPT) - μ(Ferida traumática) = 409.61     

# μ(Outra) > μ(Ferida cirúrgica)
# μ(Outra) > μ(Ferida traumática)
# μ(UP/LH/LPT) > μ(Ferida traumática)

# Concluindo, a tipologia da ferida está associada à sua duração e as feridas de
# tipologia 'Outra' e 'UP/LH/LPT' tendem a apresentar maior duração. Assim, esta
# abordagem apresenta as mesmas conclusões que a abordagem correta.


# Caso não se admitisse a normalidade e se admitisse apenas a homocedasticidade, 
# seguiriamos pelo teste de Kruskal-Wallis:
kruskal.test(Duração~Tipologia, data=feridas)
# Valor p < 2.2e-16 < 0.001
# Rejeitamos H0
# Logo concluímos que existe diferença significativa entre as diferentes tipologias
# Seguimos então para o teste de Dunn
library(dunn.test)
dunn.test(feridas$Duração, feridas$Tipologia, method="holm")
# Existem as seguintes diferenças significativas:
# Outra vs Ferida cirúrgica       , valor p = 0.0000 < 0.001
# Outra vs Ferida traumática      , valor p = 0.0000 < 0.001
# UP/LH/LPT vs Ferida cirúrgica   , valor p = 0.0000 < 0.001
# UP/LH/LPT vs Ferida traumática  , valor p = 0.0000 < 0.001

# μ(Outra) > μ(Ferida cirúrgica)
# μ(Outra) > μ(Ferida traumática)
# μ(UP/LH/LPT) > μ(Ferida cirúrgica)
# μ(UP/LH/LPT) > μ(Ferida traumática)

# Concluindo, a tipologia da ferida está associada à sua duração e as feridas de
# tipologia 'Outra' e 'UP/LH/LPT' tendem a apresentar maior duração. Assim, esta
# abordagem apresenta uma ligeira diferença porém as conclusões são muito
# semelhantes.



###---------------###
#### EXERCÍCIO 2 ####
###   ALÍNEA A    ###
###---------------###
# ANCOVA
library(car)
library(moments)
library(effects)
library(multcomp)
library(reshape)
library(ggplot2)
form <- read.csv2 ("formacao.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                     dec=",", header=T, sep=";", encoding = "UTF-8") 
form
names(form)
summary(form)
# Modelo ANCOVA com: 
# 'Nota Final' como variável resposta;
# 'Grupo' como fator categórico;
# 'Horas_Estudo' como covariável contínua
contrasts(form$Grupo)=contr.poly(4) 
mod<-aov(Nota_Final ~ Horas_Estudo+Grupo, data=form) 
Anova(mod, type="III") 

# Linearidade entre Covariável e Variável Dependente
ggplot(form, aes(Horas_Estudo, Nota_Final)) +
  geom_point(size = 3) + geom_smooth(method = "lm", alpha = 0.1) + labs(x = "Horas_Estudo", y = "Nota_Final")
# Analisando o ggplot verificamos que a relação entre a covariável 'Horas_Estudo'
# e a variável dependente 'Nota_Final' é linear dentro de cada grupo, sugerido
# pela reta de regressão apresentada.

# Homogeneidade de Declives
ggplot(form, aes(Horas_Estudo, Nota_Final, colour = Grupo))+
  geom_point(aes(shape = Grupo), size = 3) + geom_smooth(method = "lm", aes(fill = Grupo), alpha = 0.1) + labs(x = "Horas_Estudo", y = "Nota_final")
# Por análise gráfica verificamos que os Grupos A e D correspondentes ao ensino
# assíncrono e ensino baseado em projetos têm declive inferior aos Grupos B e C,
# significando que estudar mais aumenta menos a Nota_Final do que nos outros.
# Seguimos agora para a análise formal deste pressuposto.
modi<-aov(Nota_Final ~ Horas_Estudo*Grupo, data=form) 
Anova(modi, type="III") 
# Valor p = 0.2689 > 0.05
# Admite-se H0
# Assim, admitimos a homogeneidade de declives.

# Independência entre Covariável e Grupo
modind<-aov(Horas_Estudo ~ Grupo, form)
summary(modind)
# Neste estudo faz sentido ter em conta a independência entre covariável e grupo
# visto que este é medido após a experiência
# Valor p = 4.22e-08 < 0.001
# Rejeitamos H0
# Rejeitamos então o pressuposto da independência entre covariável e grupo

# Resíduos
rs<-rstandard (mod)

# Normalidade
qqPlot(rs)
# Pela análise do qqPlot dos resíduos é sugerido que devemos rejeitar a 
# normalidade mas seguimos para o teste de Shapiro-Wilk para confirmação
shapiro.test(rs)
# Valor p = 0.009711 < 0.05
# Rejeitamos H0
# O teste de Shapiro-Wilk confirma o que foi sugerido anteriormente, devemos
# rejeitar a normalidade por agora, embora ainda realizemos os testes de
# D'Agostino e de Anscombe-Glynn para uma última tentativa
agostino.test(rs)
# Valor p = 0.01565 < 0.05
# Rejeitamos H0
anscombe.test (rs)
# Valor p = 0.09583 > 0.05
# Admitimos H0
# Assim, após verificação dos testes de D'Agostino e de Anscombe-Glynn chegámos
# à conclusão de que devemos rejeitar a normalidade definitivamente, falhando no
# teste de D'Agostino, sendo necessário passar em ambos para admitir a
# normalidade

# Homocedasticidade
plot(mod$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")
# Analisando o plot não existe evidência de qualquer padrão ou afunilamento,
# então é sugerido que devemos admitir a homocedasticidade mas avançamos para
# o teste de Levene para confirmar
leveneTest(Nota_Final ~ Grupo, data = form)
# Valor p = 0.5728 > 0.05
# Admitimos H0
# Após os resultados obtidos pelo teste de Levene concluímos que este corrobora 
# a nossa conclusão anterior e admitimos, de facto, a homocedasticidade

# Concluindo, os pressupostos falham na normalidade e na independência entre
# covariável e grupo então seguimos para a ANCOVA Robusta e usamos glht com
# recurso ao método Tukey para retirar conclusões finais
library(robustbase)
mod_robust <- lmrob(Nota_Final ~ Horas_Estudo + Grupo, data = form,
                    control = lmrob.control(max.it = 500, k.max = 500, init = "lts"))
Anova(mod_robust, type = 3)
library(multcomp)
posthoc <- glht(mod_robust, linfct = mcp(Grupo = "Tukey"))
summary(posthoc)
# Existem as seguintes diferenças significativas entre os grupos:
# A vs B      , valor p < 0.001
# A vs C      , valor p < 0.001
# A vs D      , valor p < 0.001
# B vs C      , valor p < 0.001
# C vs D      , valor p < 0.001

# μ(B) - μ(A) = 5.8182     
# μ(C) - μ(A) = 3.0970     
# μ(D) - μ(A) = 7.0612  <---
# μ(C) - μ(B) = -2.7212     
# μ(D) - μ(C) = 3.9641     

# μ(B) > μ(A)
# μ(C) > μ(A)
# μ(D) > μ(A)
# μ(B) > μ(C)
# μ(D) > μ(C)

# Concluindo, podemos verificar que os métodos 'Ensino Síncrono', 
# 'Énsino Híbrido' e 'Ensino Baseado em Projetos' são mais eficazes que o método 
# 'Ensino Assíncrono', os métodos 'Ensino Síncrono' e 'Ensino Baseado em 
# Projetos' são mais eficazes que o método 'Énsino Híbrido' e, por fim, que o 
# método 'Ensino Baseado em Projetos' é, de facto, o mais eficaz entre todos, ou
# seja, se tivesse de ser escolhido um método ideal, seria precisamente este.



###---------------###
#### EXERCÍCIO 2 ####
###   ALÍNEA B    ###
###---------------###
# Contrastes
(contrasts(form$Grupo)<-cbind(c(-1/3, -1/3, -1/3, 1), c(1, -1/2, -1/2, 0), c(0, -1, 1, 0)))
round(crossprod(contrasts(form$Grupo)), 2)
modc<-aov(Nota_Final ~ Horas_Estudo+Grupo, data=form)
summary(modc, split=list(Grupo=list("Projeto VS Resto"=1, "Assíncrono VS Síncrono"=2, "Síncrono VS Híbrido"=3)))
# Existem diferenças significativas nos seguintes contrastes:
# Projeto vs Resto          , valor p = 1.14e-05 < 0.001
# Assíncrono vs Síncrono    , valor p = 2.03e-08 < 0.001
# Síncrono vs Híbrido       , valor p = 0.00168 < 0.05

# Concluindo, o 'Ensino Baseado em Projetos' destaca-se em relação aos restantes,
# o 'Ensino Assíncrono' é significativamente diferente do 'Ensino Síncrono', ou 
# seja, dos métodos de 'Ensino Síncrono' e 'Ensino Híbrido' e, por fim, o
# 'Ensino Síncrono' apresenta claras diferenças em relação ao 'Ensino Híbrido'.



###---------------###
#### EXERCÍCIO 3 ####
###---------------###
# ANOVA 2 Fatores
library (car)
library(emmeans)
library(ggplot2)
queijos <- read.csv2 ("queijos1.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                     dec=",", header=T, sep=";", encoding = "UTF-8") 

names(queijos)
summary(queijos)
queijos$sal <- as.factor(queijos$sal)
queijos$dias <- as.factor(queijos$dias)
summary(queijos)
# Modelo ANOVA com 'masticabilidade' como variável resposta e 'sal' e 'dias'
# sendo os dois fatores
mod  <-  aov(masticabilidade  ~  sal*dias, queijos)
# Resíduos
rs<-rstandard(mod) 

# Normalidade
qqPlot(rs)
# Pela observação gráfica do qqPlot dos resíduos observamos vários outliers 
# significativos o que nos sugere a rejeição da normalidade, ainda assim 
# seguimos para a confirmação formal com recurso aos testes adequados.
shapiro.test(rs)
# Valor p = 0.003132 < 0.05
# Rejeitamos H0
# Pelo teste de Shapiro-Wilk a normalidade é rejeitada então seguimos para os
# testes de D'Agostino e de Anscombe-Glynn para uma última hipótese desta ser
# admitida
library(moments)
agostino.test(rs)
# Valor p = 1 > 0.05
# Embora suspeito seguimos partindo do princípio que H0 é admitido no teste de 
# D'Agostino
anscombe.test(rs)
# Valor p = 0.0476 < 0.05
# Rejeitamos H0
# Assim, falhando pelo menos um dos dois testes anteriores, D'Agostino e
# Anscombe-Glynn, rejeitamos definitivamente a normalidade, falhando este
# pressuposto

# Homocedasticidade
plot(mod$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")
abline(h=0, lty=3)
# Por observação gráfica verificamos um claro afunilamento nos pontos então
# é sugerido rejeitar a homocedasticidade para estes dados, ainda assim seguimos
# para a verificação formal pelo teste de Levene
leveneTest(rs ~ factor(queijos$sal)*factor(queijos$dias))
# Valor p = 2.2e-16 < 0.001
# Rejeitamos H0
# Assim, temos a confirmação final de que devemos de facto rejeitar a
# homocedasticidade
# Concluindo, ambos os pressupostos falham, então somos forçados a seguir para
# transformações do modelo com o objetivo de que pelo menos um pressuposto seja
# verificado, permitindo assim prosseguir com o estudo


# A primeira a ser testada é a Transformação Logaritmo
# Modelo ANOVA
modlog<-aov(log(masticabilidade)~sal*dias, queijos)
# Resíduos
rslog<-rstandard (modlog)

# Normalidade
qqPlot(rslog)
# Após análise gráfica do qqPlot dos resíduos transformados é sugerida a 
# admissão da normalidade dado que não são apresentados outliers significativos,
# no entanto avançamos para a verificação formal pelo teste de Shapiro-Wilk
shapiro.test(rslog)
# Valor p = 0.1283 > 0.05
# Admitimos H0
# Assim, com a Transformação Logaritmo podemos admitir a normalidade 

# Homocedasticidade
plot(modlog$fit,  rslog,  xlab="valores  ajustados",  ylab="resíduos")
# Analisando o plot não detetamos um afunilamento claro, no entanto parece
# existir uma ligeira simetria na sua distribuição então precisamos avançar para
# a análise formal para retirar uma conclusão em concreto
leveneTest(log(masticabilidade) ~ dias*sal, data = queijos)
# Valor p < 2.2e-16 < 0.001
# Rejeitamos H0
# Assim, com a Transformação Logaritmo continuamos a rejeitar a 
# homocedasticidade, avançamos para a próxima transformação para verificar se
# existe uma que verifique ambos os pressupostos


# A segunda a ser testada é a Transformação Raiz
# Modelo ANOVA
modr<-aov(sqrt(masticabilidade)~sal*dias, queijos)
# Resíduos
rsraiz<-rstandard (modr)

# Normalidade
qqPlot(rsraiz)
# Observando o gráfico qqPlot verificamos outliers no entanto não aparentam ser 
# significativos, avançamos para a verificação formal para confirmar a
# conclusão a tirar
shapiro.test(rsraiz)
# Valor p = 0.1426 > 0.05
# Admitimos H0
# Assim, com a Transformação Raiz podemos admitir a normalidade

# Homocedasticidade
plot(modr$fit,  rsraiz,  xlab="valores  ajustados",  ylab="resíduos")
# Pela observação do plot verificamos um claro afunilamento então a
# homocedasticidade será rejeitada, no entanto verificamos formalmente esta
# conclusão
leveneTest(sqrt(masticabilidade) ~ dias*sal, data = queijos)
# Valor p < 2.2e-16 < 0.001
# Rejeitamos H0
# Assim, com a Transformação Raiz continuamos a rejeitar a 
# homocedasticidade, avançamos para a próxima transformação para verificar se
# existe uma que verifique ambos os pressupostos


# A terceira e última a ser testada é a Transformação Inversa
# Modelo ANOVA
modi<-aov(1/(masticabilidade)~sal*dias, queijos)
# Resíduos
rsi<-rstandard (modi)

# Normalidade
qqPlot(rsi)
# São observados outliers significativos no qqPlot desta transformação então é 
# sugerida a rejeição da normalidade, no entanto verificamos formalmente pelo
# teste de Shapiro-Wilk
shapiro.test(rsi)
# Valor p = 2.058e-05 < 0.001 , ou seja,
# Rejeitamos H0
# Assim, com a Transformação Inversa continuamos a rejeitar a normalidade

# Homocedasticidade
plot(modi$fit,  rsi,  xlab="valores  ajustados",  ylab="resíduos")
# Observamos novamente afunilamento no plot então é sugerida a rejeição da
# homocedasticidade mais uma vez, mas avançamos para a verificação formal
leveneTest(1/masticabilidade~sal*dias, data = queijos)
# Valor p < 2.2e-16 < 0.001, ou seja,
# Rejeitamos H0
# Assim, com a Transformação Inversa continuamos a rejeitar a 
# homocedasticidade, sendo esta a pior transformação, continuando a rejeitar
# ambos os pressupostos

# Concluindo, embora possamos admitir a normalidade com a Transformação
# Logaritmo e a Transformação Raiz, em ambas as transformações rejeitamos
# fortemente a homocedasticidade, não sendo fiável avançar para o estudo com uma
# destas transformações, avançamos então para uma análise mais robusta com 
# recurso a uma ANOVA por Permutação


# ANOVA por Permutação
library(permuco)
# ANOVA por Permutação
modelo_perm <- aovperm(masticabilidade ~ sal * dias, data = queijos, np = 5000)  # 5000 Permutações
summary(modelo_perm)
# Valor p = 0.3032 > 0.05
# Admitimos H0
# Analisando a interação sal:dias verificamos que a interação entre os mesmos
# não é significativa, devendo avançar então para os Testes t não pareados
pairwise.t.test(queijos$masticabilidade, queijos$sal, p.adjust.method = "holm", paired = FALSE)
# Os valores p para diferenças significativas entre as concentrações de sal são 
# todas iguais a 1 logo não admitimos que hajam diferenças significativas
pairwise.t.test(queijos$masticabilidade, queijos$dias, p.adjust.method = "holm", paired = FALSE)
# Existem as seguintes diferenças significativas:
# 1 vs 28     , valor p = 0.018 < 0.05
# 1 vs 42     , valor p = 1.2e-05 < 0.001
# 1 vs 56     , valor p = 2.8e-11 < 0.001
# 14 vs 28    , valor p = 0.050 <= 0.05  Já considerado como rejeição de H0
# 14 vs 42    , valor p = 5.0e-05 < 0.001
# 14 vs 56    , valor p = 7.4e-11 < 0.001
# 28 vs 42    , valor p = 0.018 < 0.05
# 28 vs 56    , valor p = 6.6e-09 < 0.001
# 42 vs 56    , valor p = 7.3e-06 < 0.001

# Assim, apenas não são apresentadas diferenças significativas entre 1 e 14 dias
# de maturação, entre todos os outros dias são apresentadas diferenças 
# significativas

# Concluindo, o tempo de maturação em dias tem um efeito estatisticamente
# significativo, tendo um forte impacto sobre a masticabilidade dos queijos 
# entre quaisquer patamares de maturação (entre quase todos os números de dias 
# de maturação), a concentração de sal é significativa, no entanto não apresenta 
# diferenças significativas entre as diferentes concentrações para admitir que 
# influencie a masticabilidade e, por fim, não existe interação significativa
# entre o sal e os dias, o que indica que estes atuam de forma independente na
# masticabilidade do queijo



####-------------####
###  EXERCÍCIO 4  ###
###---------------###
# ANOVA Medidas Repetidas com 1 Fator Within(hour) e 1 Fator Between(breed)
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
library(ggplot2)
ovinos <- read.csv2 ("ovinos.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                      dec=",", header=T, sep=";", encoding = "UTF-8") 
summary(ovinos)
# Modelo com teste à Esfericidade
mod <-aov_car(RT~breed*hour +Error(Animal/hour),data=ovinos, anova_table=list(correction = "GG", es = "pes")) 
summary(mod)
mod
# Teste de Mauchly:
# Valor p = 7.2831e-10 < 0.001
# Rejeita H0
# A pressuposto da esfericidade não é admitido então avançamos para a correção 
# do valor p do fator within por GG ou HF
# Greenhouse-Geiser apresenta ε = 0.69569 < 0.75 e valor p = 0.1016 > 0.05 para
# a interação, não sendo então considerada como significativa

# Normalidade
ovinos %>% 
  group_by(hour) %>%
  shapiro_test(RT)
ggqqplot(ovinos, "RT", facet.by = "hour")
# Para as 14:00     valor p = 0.0194 < 0.05
# Rejeitamos H0
# Para as 14:30     valor p = 0.211  > 0.05
# Admitimos H0
# Para as 14:45     valor p = 0.0361 < 0.05
# Rejeitamos H0
# Para as 15:00     valor p = 0.0119 < 0.05
# Rejeitamos H0
# A normalidade não é verificada para 3 das 4 horas
# Logo não se verifica a normalidade em geral e consequentemente não se admite a
# normalidade para o modelo

# Homocedasticidade
P1<-subset(ovinos, hour=="14:00")
P2<-subset(ovinos, hour=="14:30")
P3<-subset(ovinos, hour=="14:45")
P4<-subset(ovinos, hour=="15:00")
leveneTest(RT~breed, data=P1)
# Valor p = 0.07841 > 0.05
# Admitimos H0
leveneTest(RT~breed, data=P2)
# Valor p = 0.5606 > 0.05
# Admitimos H0
leveneTest(RT~breed, data=P3)
# Valor p = 0.3752 > 0.05
# Admitimos H0
leveneTest(RT~breed, data=P4)
# Valor p = 0.6913 > 0.05
# Admitimos H0
leveneTest(RT ~ breed * hour, data = ovinos)
# Valor p = 0.1877 > 0.05
# Admitimos H0
# Assim, a homocedasticidade verifica-se em todas as repetições e
# consequentemente no geral

# Concluindo, admitimos a homocedasticidade e a esfericidade com correções do 
# valor p do fator within por Greenhouse-Geiser e rejeitamos a normalidade
# Seguimos então a abordagem não paramétrica de ART para tirar conclusões finais
# para o nosso estudo

# Transformação dos dados para ranks alinhados
ovinos1<-subset(ovinos, RT!="NA")
modelo_art <- art(RT ~ breed * hour + (1 | Animal), data = ovinos1) 
anova(modelo_art)
# Verificámos anteriormente que a interação breed:hour não é significativa então
# avançamos para ART com comparações múltiplas de 'holm' para o fator within
# 'hour' e ART com comparações múltiplas de 'tukey' para o fator between 'breed'
art.con(modelo_art, "hour", adjust = "holm")
# Existem as seguintes diferenças significativas:
# 14:00 vs 14:30        , valor p < 0.0001 < 0.001
# 14:00 vs 14:45        , valor p < 0.0001 < 0.001
# 14:00 vs 15:00        , valor p < 0.0001 < 0.001
# 14:30 vs 14:45        , valor p = 0.0003 < 0.001
# 14:30 vs 15:00        , valor p < 0.0001 < 0.001

# μ(14:00) - μ(14:30)   = 78.78 
# μ(14:00) - μ(14:45)   = 105.06 
# μ(14:00) - μ(15:00)   = 113.90  
# μ(14:30) - μ(14:45)   = 26.28
# μ(14:30) - μ(15:00)   = 35.12

# μ(14:00) > μ(14:30) 
# μ(14:00) > μ(14:45) 
# μ(14:00) > μ(15:00)
# μ(14:30) > μ(14:45)
# μ(14:30) > μ(15:00)

# Concluindo, podemos verificar que ás 14:00 a frequência respiratória foi mais
# alta que todos os restantes horários, as horas de maior RT foram ás 14:00 e
# 14:30 e a frequência respiratória tendeu a diminuir ao longo dos horários,
# o que indica que o stress térmico foi mais intenso no início das medições e
# tendeu a diminuir ao longo do tempo.

art.con(modelo_art, "breed", adjust = "tukey")
# Existem as seguintes diferenças significativas:
# Ile vs Santa Inês         , valor p = 0.0006 < 0.001
# MN vs Suffolk             , valor p = 0.0339 < 0.05
# Santa Inês vs Suffolk     , valor p < 0.0001 < 0.001
# Santa Inês vs Texel       , valor p = 0.0001 < 0.001

# μ(Ile) - μ(Santa Inês)      = 118.19
# μ(MN) - μ(Suffolk)          = -82.19
# μ(Santa Inês) - μ(Suffolk)  = -138.10 
# μ(Santa Inês) - μ(Texel)    = -132.38 

# μ(Ile) > μ(Santa Inês)
# μ(Suffolk) > μ(MN)
# μ(Suffolk) > μ(Santa Inês)
# μ(Texel) > μ(Santa Inês)

# Concluindo, podemos verificar que a raça 'Santa Inês' foi a que apresentou
# menores frequências respiratórias em comparação com a maioria das restantes 
# raças, tendo menor RT que 3 das 4 restantes raças, indicando talvez uma melhor
# adaptação ao stress térmico. Por outro lado, a raça 'Suffolk' apresentou RT 
# superior ás raças 'MN' e 'Santa Inês', indicando uma resposta mais forte por
# parte da raça 'Suffolk' face ao calor, assim como acontece entre a raça 'Ile'
# e 'Santa Inês', onde a raça 'Ile' apresenta uma resposta mais acentuada face
# ao calor.

# Assim, este estudo revela que a frequência respiratória é influenciada pela
# hora da medição e pela raça dos ovinos, apresentando diferentes resultados
# relativos ao stress térmico conforme esses fatores são alterados.