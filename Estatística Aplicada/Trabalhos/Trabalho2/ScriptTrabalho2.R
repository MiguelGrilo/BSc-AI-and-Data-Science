##### Trabalho 2 #####
### João Lopes    58358
### Miguel Grilo  58387
### Jorge Couto   58656

rm(list=ls(all=TRUE))

###---------------###
#### EXERCÍCIO 1 ####
###---------------###
feridas <- read.csv2("feridas.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                     sep=";",dec=".", header=T, encoding = "UTF-8")
names(feridas)
summary(feridas)



###---------------###
#### EXERCÍCIO 1 ####
###   ALÍNEA A    ###
###---------------###
library(rcompanion)
library(irr)
library(correlation)
library(forcats)
library(moments)

# Tipologia: Ferida cirúrgica, Ferida traumática, Outra e UP/LH/LPT (4 cat.)

# Ponto i) a sua localização

# Local_Ferida: Abdómen ou Membro Inferior, Membro Superior e Outro Local (3 cat.)

# Tabela de Contingência n x k
(tc<-table(feridas$Tipologia, feridas$Local_Ferida))
#                       Abdómen ou Membro Inferior    Membro Superior     Outro Local
# Ferida cirúrgica                              79                 12              53
# Ferida traumática                             49                 25              13
# Outra                                        164                 10              23
# UP/LH/LPT                                     42                  3               9

# Teste do Qui-Quadrado
(tcq<-chisq.test(tc))
# H0: Não há associação entre a tipologia e localização da ferida
# H1: Existe associação entre a tipologia e localização da ferida

# Valor p = 2.375e-14 < 0.0001 < alfa
# Logo, rejeita-se H0 e a independência entre variáveis, o que indica a 
# existência de uma associação estatisticamente significativa entre a tipologia 
# e localização da ferida.
# Não foi lançado warning.

# Valores esperados sob a hipótese de independência
tcq$expected
#                       Abdómen ou Membro Inferior    Membro Superior     Outro Local
# Ferida cirúrgica                        99.78423          14.937759        29.27801
# Ferida traumática                       60.28631           9.024896        17.68880
# Outra                                  136.51037          20.435685        40.05394
# UP/LH/LPT                               37.41909           5.601660        10.97925

# Não foi lançado warning no Teste do Qui-Quadrado então podemos saltar este
# passo e prosseguir.

# Resíduos standardizados
tcq$stdres
#                       Abdómen ou Membro Inferior    Membro Superior     Outro Local
# Ferida cirúrgica                      -4.4839510         -0.9587829       5.8654787
# Ferida traumática                     -2.8977449          6.2048201      -1.3797333
# Outra                                  5.5217884         -3.1710938      -3.9260981
# UP/LH/LPT                              1.4341670         -1.2321836      -0.7101899

# Conclusões:
# Há mais feridas categorizadas como 'Outra' (5.52) localizadas no 'Abdómen ou 
# Membro Inferior' do que nas restantes tipologias.
# Há mais feridas categorizadas como 'Ferida traumática' (6.20) localizadas no 
# 'Membro Superior' do que nas restantes tipologias.
# Há mais feridas categorizadas como 'Ferida cirúrgica' (5.87) localizadas em 
# 'Outro Local' do que nas restantes tipologias.

# V de Cramer e Intervalo de Confiança
cramerV(tc, ci=TRUE)
# V de Cramer = 0.2808
# IC95% (V de Cramer) = [0.217, 0.362]
# Assim, com um valor V de 0.28 consideramos que existe uma associação
# estatisticamente significativa e moderada.


# Ponto ii) a sua origem

# Origem: Domicílio, Hospital, Outra e UCSP

# Tabela de Contingência n x k
(tc2<-table(feridas$Tipologia, feridas$Origem))
#                       Domicílio       Hospital         Outra           UCSP
# Ferida cirúrgica              8            126             1              9
# Ferida traumática            77              3             4              3
# Outra                       178              7             1             11
# UP/LH/LPT                    39             11             3              1

# Teste do Qui-Quadrado
(tcq2<-chisq.test(tc2))
# H0: Não há associação entre a tipologia e origem da ferida
# H1: Existe associação entre a tipologia e origem da ferida

# Valor p < 2.2e-16 < 0.0001 < alfa
# Logo, rejeita-se H0 e a independência entre variáveis, o que indica a 
# existência de uma associação estatisticamente significativa entre a tipologia 
# e origem da ferida.
# Foi lançado warning.

# Valores esperados sob a hipótese de independência
tcq2$expected
#                       Domicílio       Hospital         Outra           UCSP
# Ferida cirúrgica       90.22407       43.91701      2.688797       7.170124
# Ferida traumática      54.51037       26.53320      1.624481       4.331950
# Outra                 123.43154       60.08091      3.678423       9.809129
# UP/LH/LPT              33.83402       16.46888      1.008299       2.688797

# Foi lançado warning no Teste do Qui-Quadrado então analisamos os valores 
# estimados e verificamos que todos apresentam valor superior a 1, então 
# podemos prosseguir.

# Resíduos standardizados
tcq2$stdres
#                       Domicílio       Hospital         Outra           UCSP
# Ferida cirúrgica    -16.9157083     17.7420192    -1.2415280      0.8371709
# Ferida traumática     5.5062370     -6.0535789     2.0783515     -0.7252070
# Outra                10.4524242    -10.6824859    -1.8333374      0.5072727
# UP/LH/LPT             1.5422841     -1.7154146     2.1248299     -1.1212199

# Conclusões:
# Há mais feridas categorizadas como 'Ferida traumática' (5.51) com origem
# ao 'Domicílio' do que nas restantes tipologias.
# Há mais feridas categorizadas como 'Outra' (10.45) com origem ao 'Domicílio' 
# do que nas restantes tipologias.
# Há mais feridas categorizadas como 'Ferida cirúrgica' (17.74) com origem no
# 'Hospital' do que nas restantes tipologias.
# Há mais feridas categorizadas como 'Ferida traumática' (2.07) com origem
# 'Outra' do que nas restantes tipologias.
# Há mais feridas categorizadas como 'UP/LH/LPT' (2.12) com origem 'Outra' do 
# que nas restantes tipologias.
# As feridas de origem 'UCSP' não apresentam diferenças significativas entre
# tipologias.

# V de Cramer e Intervalo de Confiança
cramerV(tc2, ci=TRUE)
# V de Cramer = 0.4882   
# IC95% (V de Cramer) = [0.464, 0.520]
# Assim, com um valor V de 0.49 consideramos que existe uma associação
# estatisticamente significativa e forte.



###---------------###
#### EXERCÍCIO 1 ####
###   ALÍNEA B    ###
###---------------###
feridas$UP_LH_LPT <- ifelse(feridas$Tipologia == "UP/LH/LPT", "Sim", "Não")
(tcc <- with(feridas, table(UP_LH_LPT, resp, Origem)))
(tcc <- tcc[c("Sim", "Não"), , ])
# Origem: Domicílio
# UP/LH/LPT     Complexa      Não Complexa
# Sim                 27                12
# Não                130               133

# Origem: Hospital
# UP/LH/LPT     Complexa      Não Complexa
# Sim                  4                 7
# Não                 21               115

# Origem: Outra
# UP/LH/LPT     Complexa      Não Complexa
# Sim                  2                 1
# Não                  2                 4

# Origem: UCSP
# UP/LH/LPT     Complexa      Não Complexa
# Sim                  0                 1
# Não                 11                12

# Teste de Mantel-Haenszel à Independência Condicional
mantelhaen.test (tcc, correct=FALSE)
# H0: Complexidade (i) UP/LH/LPT condicional à Origem (Independência Condicional)
# H1: Complexidade (i/) UP/LH/LPT condicional à Origem (Associação Condicional)

# Valor p = 0.0057 < alfa
# Logo, rejeita-se H0 e admitimos uma Associação Condicional
# Avançamos, assim, para o Teste de Breslow-Day

# Teste de Breslow-Day à Homogeneidade dos Odds Ratio
library(DescTools)
BreslowDayTest (tcc)
# H0: Associação entre Complexidade e UP/LH/LPT é homogénea
# H1: Associação entre Complexidade e UP/LH/LPT depende das categorias da Origem

# Valor p = 0.5114 > alfa
# Logo, não rejeitamos H0 e então a associação entre Complexidade e UP/LH/LPT é
# homogénea, ou seja, o OR é comum entre as diferentes origens.

# Voltamos agora ao teste de Mantel-Haenszel para analisar o OR
mantelhaen.test (tcc, correct=FALSE)
# OR = 2.32
# IC95% (OR) = [1.26, 4.29]

# Concluindo, as feridas de tipologia UP/LH/LPT têm cerca de 2.32 vezes mais 
# possibilidades de ser complexa do que as restantes tipologias e com 95% de 
# confiança, esse aumento estará entre 1.26 e 4.29 vezes.



###---------------###
#### EXERCÍCIO 1 ####
###   ALÍNEA C    ###
###---------------###
# Ponto i) Complexidade das feridas

# Idade: Contínua
# Complexidade: Nominal (2 categorias)

# Bisserial por Pontos
cor_test(feridas, "Idade", "resp", ci=.95, method="pearson")
# H0: r = 0
# H1: r \= 1

# Valor p < 0.001 < alfa
# Logo, rejeitamos H0 e admitimos que existe uma correlação estatisticamente 
# significativa.
# r = -0.17
# IC95% (r) = [-0.26, -0.08]
# Assim, existe uma correlação fraca com r igual a -0.17, sendo este negativo
# podemos concluir que quanto maior for a idade, menor será a probabilidade de
# a ferida ser complexa.


# Ponto ii)  Ferida Cirúrgica ou Não Cirúrgica (UP/LH/LPT)

# Idade: Contínua
# Ferida Cirúrgica vs Não Cirúrgica: Nominal (2 categorias)

# Bisserial por Pontos
feridas$Ferida_Cirurgica <- ifelse(feridas$Tipologia %in% c("Ferida cirúrgica", "Ferida traumática", "Outra"), "Sim", "Não")
cor_test(feridas, "Idade", "Ferida_Cirurgica", ci=.95, method="pearson")
# H0: r = 0
# H1: r \= 1

# Valor p < 0.001 < alfa
# Logo, rejeitamos H0 e admitimos que existe uma correlação estatisticamente 
# significativa.
# r = -0.18
# IC95% (r) = [-0.27, -0.09]
# Assim, existe uma correlação fraca com r igual a -0.18, sendo este negativo
# podemos concluir que quanto maior for a idade, menor será a probabilidade de
# a ferida ser cirúrgica.



###---------------###
#### EXERCÍCIO 1 ####
###   ALÍNEA D    ###
###---------------###
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

summary(feridas)
glimpse(feridas)

# Altera a variável resposta para ter o nível de referência "Não Complexa"
feridas$resp <- relevel(factor(feridas$resp), ref = "Não Complexa")

# Passo 1

# Regressão Simples
# Ajustar modelo nulo
mod0 <- glm(resp ~ 1, family = binomial(link = "logit"), data = feridas)
summary(mod0)

# Modelo com Idade
mod1 <- glm(resp ~ Idade, family = binomial(link = "logit"), data = feridas)
summary(mod1)
anova(mod0, mod1, test = "Chisq")
# H0: A inclusão da variável 'Idade' não melhora significativamente o modelo.
# H1: A inclusão da variável 'Idade' melhora significativamente o modelo.

# Valor p = 0.0002 < 0.2
# Logo, o modelo é melhorado significativamente com esta variável.
# Devemos incluir a variável 'Idade' no Passo 2.


# Modelo com Sexo
mod2 <- glm(resp ~ Sexo, family = binomial(link = "logit"), data = feridas)
summary(mod2)
anova(mod0, mod2, test = "Chisq")
# H0: A inclusão da variável 'Sexo' não melhora significativamente o modelo.
# H1: A inclusão da variável 'Sexo' melhora significativamente o modelo.

# Valor p = 0.9801 > 0.2
# Logo, o modelo não é melhorado significativamente com esta variável.
# Não devemos incluir a variável 'Sexo' no Passo 2.


# Modelo com Habilitações
mod3 <- glm(resp ~ Habilitações, family = binomial(link = "logit"), data = feridas)
summary(mod3)
anova(mod0, mod3, test = "Chisq")
# H0: A inclusão da variável 'Habilitações' não melhora significativamente o modelo.
# H1: A inclusão da variável 'Habilitações' melhora significativamente o modelo.

# Valor p = 0.0013 < 0.2
# Logo, o modelo é melhorado significativamente com esta variável.
# Devemos incluir a variável 'Habilitações' no Passo 2.


# Modelo com Origem
mod4 <- glm(resp ~ Origem, family = binomial(link = "logit"), data = feridas)
summary(mod4)
anova(mod0, mod4, test = "Chisq")
# H0: A inclusão da variável 'Origem' não melhora significativamente o modelo.
# H1: A inclusão da variável 'Origem' melhora significativamente o modelo.

# Valor p = 9.583e-12 < 0.0001 < 0.2
# Logo, o modelo é melhorado significativamente com esta variável.
# Devemos incluir a variável 'Origem' no Passo 2.


# Modelo com Tipologia
mod5 <- glm(resp ~ Tipologia, family = binomial(link = "logit"), data = feridas)
summary(mod5)
anova(mod0, mod5, test = "Chisq")
# H0: A inclusão da variável 'Tipologia' não melhora significativamente o modelo.
# H1: A inclusão da variável 'Tipologia' melhora significativamente o modelo.

# Valor p < 2.2e-16 < 0.0001 < 0.2
# Logo, o modelo é melhorado significativamente com esta variável.
# Devemos incluir a variável 'Tipologia' no Passo 2.


# Modelo com Desnutrição
mod6 <- glm(resp ~ Desnutrição, family = binomial(link = "logit"), data = feridas)
summary(mod6)
anova(mod0, mod6, test = "Chisq")
# H0: A inclusão da variável 'Desnutrição' não melhora significativamente o modelo.
# H1: A inclusão da variável 'Desnutrição' melhora significativamente o modelo.

# Valor p = 0.0845 < 0.2
# Logo, o modelo é melhorado significativamente com esta variável.
# Devemos incluir a variável 'Desnutrição' no Passo 2.


# Modelo com Obesidade
mod7 <- glm(resp ~ Obesidade, family = binomial(link = "logit"), data = feridas)
summary(mod7)
anova(mod0, mod7, test = "Chisq")
# H0: A inclusão da variável 'Obesidade' não melhora significativamente o modelo.
# H1: A inclusão da variável 'Obesidade' melhora significativamente o modelo.

# Valor p = 0.0011 < 0.2
# Logo, o modelo é melhorado significativamente com esta variável.
# Devemos incluir a variável 'Obesidade' no Passo 2.


# Modelo com Local_Ferida
mod8 <- glm(resp ~ Local_Ferida, family = binomial(link = "logit"), data = feridas)
summary(mod8)
anova(mod0, mod8, test = "Chisq")
# H0: A inclusão da variável 'Local_Ferida' não melhora significativamente o modelo.
# H1: A inclusão da variável 'Local_Ferida' melhora significativamente o modelo.

# Valor p < 2.2e-16 < 0.0001 < 0.2
# Logo, o modelo é melhorado significativamente com esta variável.
# Devemos incluir a variável 'Local_Ferida' no Passo 2.


# Passo 2

# Regressão múltipla
# Vamos agora ajustar um modelo que inclua todas as variáveis significativas do
# Passo 1, como temos menos de 20 variáveis tomámos valor alfa = 0.20 para esta
# etapa.
# Ajustamos então com as variáveis explicativas: Idade, Habilitações, Origem,
# Tipologia, Desnutrição, Obesidade e Local_Ferida.
# Modelo múltiplo com todas as variáveis que passaram o critério do Passo 1
mod9 <- glm(resp ~ Idade + Habilitações + Origem + Tipologia + Desnutrição + Obesidade + Local_Ferida, family = binomial(link = "logit"), data = feridas)
summary(mod9)


# Passo 3

# Após ajustar o modelo de regressão múltipla, retiramos as variáveis não
# significativas do Passo 2 por ordem decrescente dos valores p.
summary(mod9)
# Variável                      Valor p
# Idade                         0.6639        > alfa  (Não significativa)
# Habilitações3º Ciclo          0.3602        > alfa  (Não significativa)
# HabilitaçõesAté 1º Ciclo      0.8243        > alfa  (Não significativa)
# HabilitaçõesSecundário        0.8263        > alfa  (Não significativa)
# HabilitaçõesSuperior          0.4806        > alfa  (Não significativa)
# OrigemHospital                0.3795        > alfa  (Não significativa)
# OrigemOutra                   0.7878        > alfa  (Não significativa)
# OrigemUCSP                    0.4877        > alfa  (Não significativa)
# TipologiaFerida traumática    0.9167        > alfa  (Não significativa)
# TipologiaOutra                1.65e-05 ***
# TipologiaUP/LH/LPT            0.0009 ***
# DesnutriçãoSim                0.0149 *  
# ObesidadeSim                  0.0518        > alfa  (Não Significativa apesar de estar no limite da significância)
# Local_FeridaMembro Superior   0.0001 ***
# Local_FeridaOutro Local       0.0003 ***

# Começamos por verificar que o maior valor p corresponde a uma categoria da
# variável Tipologia, no entanto, as restantes categorias de Tipologia 
# apresentam valor p estatisticamente significativo, então mantemo-la.

# Avançamos então para retirar a variável Habilitações, com o segundo maior valor p
mod10 <- glm(resp ~ Idade + Origem + Tipologia + Desnutrição + Obesidade + Local_Ferida, family = binomial(link = "logit"), data = feridas)
summary(mod10)
anova(mod10, mod9, test = "Chisq")
# H0: A remoção da variável 'Habilitações' não afeta significativamente o modelo.
# H1: A remoção da variável 'Habilitações' afeta significativamente o modelo.

# Valor p = 0.4862 > alfa
# Logo, a remoção da variável 'Habilitações' não afeta significativamente o 
# modelo e podemos removê-la.


# Retiramos agora a variável Origem
mod11 <- glm(resp ~ Idade + Tipologia + Desnutrição + Obesidade + Local_Ferida, family = binomial(link = "logit"), data = feridas)
summary(mod11)
anova(mod11, mod10, test = "Chisq")
# H0: A remoção da variável 'Origem' não afeta significativamente o modelo.
# H1: A remoção da variável 'Origem' afeta significativamente o modelo.

# Valor p = 0.6753 > alfa
# Logo, a remoção da variável 'Origem' não afeta significativamente o modelo e 
# podemos removê-la.


# Por fim, retiramos a variável Idade
mod12 <- glm(resp ~ Tipologia + Desnutrição + Obesidade + Local_Ferida, family = binomial(link = "logit"), data = feridas)
summary(mod12)
anova(mod12, mod11, test = "Chisq")
# H0: A remoção da variável 'Idade' não afeta significativamente o modelo.
# H1: A remoção da variável 'Idade' afeta significativamente o modelo.

# Valor p = 0.3387 > alfa
# Logo, a remoção da variável 'Idade' não afeta significativamente o modelo e 
# podemos removê-la.


# A variável Obesidade também se encontra no limite da significância mas ainda
# a mantemos no modelo por agora.


# Passo 4

# Neste passo incluímos as variáveis que ficaram de fora no Passo 2 por ordem
# crescente dos valores p.
# A única variável que ficou de fora no Passo 2 foi a variável Sexo.
# Incluímos a variável Sexo
mod13 <- glm(resp ~ Tipologia + Desnutrição + Obesidade + Local_Ferida + Sexo, family = binomial(link = "logit"), data = feridas)
summary(mod13)
anova(mod13, mod12, test = "Chisq")
# H0: A inclusão da variável 'Sexo' não melhora significativamente o modelo.
# H1: A inclusão da variável 'Sexo' melhora significativamente o modelo.

# Valor p = 0.2761 > alfa
# Logo, o modelo não é melhorado significativamente com esta variável.
# Não devemos incluir a variável 'Sexo' no Passo 4.


# Passo 5

# Este passo está dividido em duas etapas, a primeira etapa corresponde a tornar
# o modelo mais parcimonioso e a segunda etapa corresponde a validar o
# pressuposto da linearidade das variáveis contínuas com o logit e multicolinearidade

# Tornar o modelo mais parcimonioso
summary(mod12)
#                                 Estimativa    Desvio Padrão   
# TipologiaFerida traumática      0.5284        0.4089 
# TipologiaOutra                  2.5843        0.3149  
# TipologiaUP/LH/LPT              2.2218        0.3945 
# DesnutriçãoSim                  1.0752        0.4158  
# ObesidadeSim                    1.5597        0.7971 
# Local_FeridaMembro Superior     -3.0627       0.7575 
# Local_FeridaOutro Local         -1.2418       0.3210 

# Para Tipologia
# Junção Ferida traumática e Ferida cirúrgica
# Diferença = 0.5284
# Maior Desvio Padrão = 0.4089
# 0.5284 > 0.4089
# Não devemos juntar estas categorias

# Junção Outra e Ferida cirúrgica
# Diferença = 2.5843
# Maior Desvio Padrão = 0.3149
# 2.5843 > 0.3149
# Não devemos juntar estas categorias

# Junção UP/LH/LPT e Ferida cirúrgica
# Diferença = 2.2218
# Maior Desvio Padrão = 0.3945
# 2.2218 > 0.3945
# Não devemos juntar estas categorias

# Junção Outra e UP/LH/LPT
2.5843-2.2218
# Diferença = 0.3625
# Maior Desvio Padrão = 0.3945
# 0.3625 < 0.3945
# Devemos juntar estas categorias

# Junção Ferida traumática e UP/LH/LPT
2.2218-0.5284
# Diferença = 1.6934
# Maior Desvio Padrão = 0.4089
# 1.6934 > 0.4089
# Não devemos juntar estas categorias

# Junção Ferida traumática e Outra
2.5843-0.5284
# Diferença = 2.0559
# Maior Desvio Padrão = 0.4089
# 2.0559 > 0.4089
# Não devemos juntar estas categorias

# Assim, juntamos as categorias Outra e UP/LH/LPT para a variável Tipologia
feridas$TipologiaNew <- fct_recode(feridas$Tipologia, "UP/LH/LPT / Outra" = "UP/LH/LPT", "UP/LH/LPT / Outra" = "Outra")
feridas$TipologiaNew <- factor(feridas$TipologiaNew)

mod14 <- glm(resp ~ TipologiaNew + Desnutrição + Obesidade + Local_Ferida, family = binomial(link = "logit"), data = feridas)
summary(mod14)
anova(mod14, mod12, test = "Chisq")
# H0: A junção não prejudica significativamente o ajuste.
# H1: A junção prejudica significativamente o ajuste.

# Valor p = 0.2936 > alfa
# Logo, a junção não prejudicaria significativamente o ajuste, então 
# passamos a utilizar a variável TipologiaNew com a junção das categorias Outra 
# e UP/LH/LPT da variável Tipologia.

# Voltamos a analisar se existem categorias a juntar após a junção anterior
summary(mod14)
#                                 Estimativa    Desvio Padrão   
# TipologiaNewFerida traumática   0.5188        0.4081
# TipologiaNewUP/LH/LPT / Outra   2.4936        0.3013
# DesnutriçãoSim                  1.0054        0.4109
# ObesidadeSim                    1.5348        0.7936
# Local_FeridaMembro Superior     -3.0794       0.7610
# Local_FeridaOutro Local         -1.2491       0.3215

# Confirmamos se ainda há margem para melhoria ao juntar mais categorias para TipologiaNew
# Junção Ferida traumática e Ferida cirúrgica
# Diferença = 0.5188
# Maior Desvio Padrão = 0.4081
# 0.5188 > 0.4081
# Não devemos juntar estas categorias

# Junção UP/LH/LPT / Outra e Ferida cirúrgica
# Diferença = 2.4936
# Maior Desvio Padrão = 0.3013
# 2.4936 > 0.3013
# Não devemos juntar estas categorias

# Junção Ferida traumática e UP/LH/LPT / Outra
2.4936-0.5188
# Diferença = 1.9748
# Maior Desvio Padrão = 0.4081
# 1.9748 > 0.4081
# Não devemos juntar estas categorias

# Avançamos então para a hipótese de juntar categorias dentro da variável Local_Ferida
# Junção Abdómen ou Membro Inferior e Membro Superior
# Diferença = 3.0794
# Maior Desvio Padrão = 0.7610
# 3.0794 > 0.7610
# Não devemos juntar estas categorias

# Junção Abdómen ou Membro Inferior e Outro Local
# Diferença = 1.2491
# Maior Desvio Padrão = 0.3215
# 1.2491 > 0.3215
# Não devemos juntar estas categorias

# Junção Membro Superior e Outro Local
3.0794-1.2491
# Diferença = 1.8303
# Maior Desvio Padrão = 0.7610
# 1.8303 > 0.7610
# Não devemos juntar estas categorias

# Não existem mais categorias para juntar, então admitimos que o modelo já é o
# mais parcimonioso possível, avançamos agora para verificar se a variável 
# Obesidade é necessária no modelo, visto que se apresenta no limite da 
# significância com valor p = 0.0531.

# Por fim, testamos retirar a variável Obesidade que se encontra no limite da signifiância
mod15 <- glm(resp ~ TipologiaNew + Desnutrição + Local_Ferida, family = binomial(link = "logit"), data = feridas)
summary(mod15)
anova(mod15, mod14, test = "Chisq")
# H0: A remoção da variável 'Obesidade' não afeta significativamente o modelo.
# H1: A remoção da variável 'Obesidade' afeta significativamente o modelo.

# Valor p = 0.0285 < alfa
# Logo, a remoção da variável 'Obesidade' afeta significativamente o modelo e 
# não podemos removê-la.


# Validação do pressuposto da linearidade das variáveis contínuas com o logit
# Como todas as variáveis de mod14 são categóricas, o pressuposto da linearidade 
# do logit não se aplica e seguimos para o próximo passo.

# Validação do pressuposto da multicolinearidade
vif(mod14)
# GVIF TipologiaNew = 1.044418 < 5
# GVIF Desnutrição = 1.047911 < 5
# GVIF Obesidade = 1.010348 < 5
# GVIF Local_Ferida = 1.031338 < 5
# Como todos os valores VIF são inferiores a 5, não precisamos calcular os 
# coeficientes de correlação e podemos admitir o pressuposto da 
# multicolinearidade como validado no modelo.


# Passo 6

# Avançamos agora para a procura de interações significativas neste modelo
mod_int1 <- glm(resp ~ TipologiaNew*Desnutrição + Obesidade + Local_Ferida, family=binomial(link="logit"), data=feridas)
summary(mod_int1)
anova(mod14, mod_int1, test="Chisq")
# H0: A interação não é significativa para o modelo.
# H1: A interação é significativa para o modelo.

# Valor p = 0.2569 > alfa
# Logo, não rejeitamos H0, ou seja, a interação não é significativa.

mod_int2 <- glm(resp ~ TipologiaNew*Obesidade + Desnutrição + Local_Ferida, family=binomial(link="logit"), data=feridas)
summary(mod_int2)
anova(mod14, mod_int2, test="Chisq")
# H0: A interação não é significativa para o modelo.
# H1: A interação é significativa para o modelo.

# Valor p = 0.2766 > alfa
# Logo, não rejeitamos H0, ou seja, a interação não é significativa.

mod_int3 <- glm(resp ~ TipologiaNew*Local_Ferida + Desnutrição + Obesidade, family=binomial(link="logit"), data=feridas)
summary(mod_int3)
anova(mod14, mod_int3, test="Chisq")
# H0: A interação não é significativa para o modelo.
# H1: A interação é significativa para o modelo.

# Valor p = 0.2045 > alfa
# Logo, não rejeitamos H0, ou seja, a interação não é significativa.

mod_int4 <- glm(resp ~ Desnutrição*Obesidade + TipologiaNew + Local_Ferida, family=binomial(link="logit"), data=feridas)
summary(mod_int4)
anova(mod14, mod_int4, test="Chisq")
# H0: A interação não é significativa para o modelo.
# H1: A interação é significativa para o modelo.

# Valor p = ?
# Não foi dado qualquer valor p mas os graus de liberdade são 0 e a deviance é
# também 0, logo assumimos que a interação não é significativa.

mod_int5 <- glm(resp ~ Desnutrição*Local_Ferida + TipologiaNew + Obesidade, family=binomial(link="logit"), data=feridas)
summary(mod_int5)
anova(mod14, mod_int5, test="Chisq")
# H0: A interação não é significativa para o modelo.
# H1: A interação é significativa para o modelo.

# Valor p = 0.0134 < alfa
# Logo, rejeitamos H0, ou seja, a interação é significativa.

mod_int6 <- glm(resp ~ Obesidade*Local_Ferida + TipologiaNew + Desnutrição, family=binomial(link="logit"), data=feridas)
summary(mod_int6)
anova(mod14, mod_int6, test="Chisq")
# H0: A interação não é significativa para o modelo.
# H1: A interação é significativa para o modelo.

# Valor p = 0.9172 > alfa
# Logo, não rejeitamos H0, ou seja, a interação não é significativa.

mod_int7 <- glm(resp ~ TipologiaNew*Desnutrição*Local_Ferida + Obesidade, family=binomial(link="logit"), data=feridas)
summary(mod_int7)
anova(mod14, mod_int7, test="Chisq")
# H0: A interação não é significativa para o modelo.
# H1: A interação é significativa para o modelo.

# Valor p = 0.1089 > alfa
# Logo, não rejeitamos H0, ou seja, a interação não é significativa.

mod_int8 <- glm(resp ~ Obesidade*Desnutrição*Local_Ferida + TipologiaNew, family=binomial(link="logit"), data=feridas)
summary(mod_int8)
anova(mod14, mod_int8, test="Chisq")
# H0: A interação não é significativa para o modelo.
# H1: A interação é significativa para o modelo.

# Valor p = 0.0693 > alfa
# Logo, não rejeitamos H0, ou seja, a interação não é significativa.


# Passo 7

# Assim, admitimos o seguinte modelo como modelo final para avaliação:
mod_final <- glm(resp ~ Desnutrição*Local_Ferida + TipologiaNew + Obesidade, family=binomial(link="logit"), data=feridas)
summary(mod_final)

# Avançamos então para a avaliação da bondade do ajustamento.
feridas$resp_num<-as.numeric(feridas$resp)-1

# Teste de Hosmer
(hl<-hoslem.test(feridas$resp_num, fitted(mod_final), g = 10))
# H0: O modelo ajusta-se aos dados.
# H1: O modelo não se ajusta aos dados.

# Valor p = 0.7335 > alfa
# Logo, admitimos que o modelo se ajusta aos dados, com um valor p alto
# que é indicativo de um ajustamento bom aos dados.

hl$expected

# Teste de Cessie-van Houwelingen
# No modelo final apresentado não temos variáveis explicativas contínuas, apenas
# categóricas, logo podemos omitir este teste.

# Curva ROC
ROC(form = resp ~ Desnutrição*Local_Ferida + TipologiaNew + Obesidade, data=feridas, plot="ROC",PV=T,MX=T,AUC=T)
# AUC = 0.841
# Ponto Corte = 0.325
# Sensibilidade = 80.7%
# Especificidade = 78.2%
# Assim, para um AUC = 0.841, o modelo tem uma capacidade discriminativa muito
# boa. Para um Ponto Corte = 0.325 obtem-se uma Sensibilidade de 80.7% e uma
# Especificidade de 78.2%

# Teste DeLong
library(pROC)
ci.auc(feridas$resp, fitted(mod_final), conf.level = 0.95)
# IC95% (AUC) = [0.8071, 0.8742]
# Assim, com confiança a 95% o AUC estará entre 0.8071 e 0.8742.


# Passo 8

# Análise de resíduos
(modelo.mf <- model.frame(mod_final))
(modelo.cp <- epi.cp(modelo.mf[-1]))
# Neste modelo temos 19 padrões e 482 indivíduos 
modelo.obs <- as.vector(by(feridas$resp_num, as.factor(modelo.cp$id), FUN = sum)) 
modelo.fit <- as.vector(tapply(fitted(mod_final), as.factor(modelo.cp$id), min))
(modelo.res <- epi.cpresids(obs = modelo.obs, fit = modelo.fit, covpattern = modelo.cp))

# Distância de Cook
plot(modelo.fit, modelo.res$sdeltabeta, xlab="Probabilidades Estimadas", ylab="Distância de Cook") 

# Identificar padrões que se destacam 
identify (modelo.fit, modelo.res$sdeltabeta, tolerance = 2) 
# Observamos imediatamente no gráfico 3 padrões que indicam ser influentes, 
# que apresentam distância de Cook muito superior a 1.
# O padrão 6 é claramente influente pela distância de Cook altíssima, 
# aproximadamente 10.
# Os padrões 5 e 1 são também influentes pela distância de Cook um pouco 
# superior 4.
# Nenhum dos outros padrões parece levantar grandes problemas, dada a distância 
# de Cook inferior a 1.

# Subset com os indivíduos dos padrões 1, 5 e 6
subset(modelo.mf, modelo.cp$id %in% c(1, 5, 6)) 
# Ao invés de fazermos subsets individuais, usamos uma lista para os três 
# padrões em simultâneo diretamente.

raio <- sqrt(modelo.res$deltabeta/pi)
Delta_Dev<-modelo.res$deviance^2/(1-modelo.res$leverage)
symbols(modelo.fit, Delta_Dev, circles=raio, inches=0.35, xlab="Probabilidades estimadas", ylab="Alteração na Deviance")
text(modelo.fit, Delta_Dev, modelo.res$cpid, cex=0.5)
# Concluindo agora, observamos que o padrão 6 é de facto muito problemático, 
# enquanto o 5 e 1 são aceitáveis.
# Portanto, vamos manter o 1 e o 5 mas devemos remover o 6.

subset(modelo.mf, modelo.cp$id == 6)
linhas_p6 <- which(modelo.cp$id == 6)
feridas_sem_p6 <- feridas[-linhas_p6, ]
mod_sem_p6 <- glm(resp ~ Desnutrição * Local_Ferida + TipologiaNew + Obesidade, family = binomial(link = "logit"), data = feridas_sem_p6)
((coef(mod_final) - coef(mod_sem_p6)) / coef(mod_final)) * 100
((deviance(mod_final) - deviance(mod_sem_p6)) / deviance(mod_final)) * 100
# Portanto, o modelo varia bastante sem o padrão 6, dado que este era altamente 
# influente.

set.seed(123)
train_index <- createDataPartition(feridas$resp, p = 0.7, list = FALSE)
train_data <- feridas[train_index, ]
test_data <- feridas[-train_index, ]

mod_train <- glm(resp ~ Desnutrição*Local_Ferida + TipologiaNew + Obesidade, 
                 family = binomial(link = "logit"), data = train_data)

# Probabilidades preditas
pred_probs_test <- predict(mod_train, newdata = test_data, type = "response")

# Classificação com cutoff arbitrário (p.ex., 0.5)
pred_class_test <- ifelse(pred_probs_test > 0.5, "Complexa", "Não Complexa")
conf_matrix <- confusionMatrix(as.factor(pred_class_test), as.factor(test_data$resp), positive = "Complexa")
print(conf_matrix)
#                 Reference
# Prediction      Não Complexa    Complexa
# Não Complexa              65          12
# Complexa                  20          47

# Exatidão = 0.7778         
# IC95% (Exatidão) = [0.701, 0.8428]
# NIR = 0.5903         
# P-Value [Acc > NIR] : 1.61e-06       

# Kappa : 0.5499         

# Mcnemar's Test P-Value : 0.2159         
                                         
# Sensibilidade = 0.7966         
# Especificidade = 0.7647         
# PPV = 0.7015         
# NPV = 0.8442         
# Prevalência = 0.4097         
# DR = 0.3264         
# DP = 0.4653         
# BE = 0.7807 

roc_curve <- roc(response = test_data$resp_num,
                 predictor = pred_probs_test,
                 levels = c("0", "1"),
                 direction = "<")

youden_result <- coords(roc_curve, x = "best", best.method = "youden",
                        ret = c("threshold", "sensitivity", "specificity", "ppv", "npv", "accuracy", "fpr", "tpr"))
print(youden_result)
#             threshold   sensitivity   specificity   ppv         npv         accuracy    fpr         tpr
# threshold   0.5067364   0.7966102     0.7647059     0.7014925   0.8441558   0.7777778   0.2352941   0.7966102

(optimal_cutoff <- as.numeric(youden_result["threshold"]))
# Ponto corte ótimo = 0.5067364



###---------------###
#### EXERCÍCIO 2 ####
###---------------###
# Análise de Clusters
library(reshape2)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(pheatmap)
library(FactoMineR)
library(factoextra)
library(cluster)
library(openxlsx)

dados <- read.csv2("QOLF.csv", sep = ";", header = TRUE)
str(dados)
summary(dados)


# Normalização e Escolha do Nº de Clusters
dados_norm <- scale(dados[, 6:10])

# Método do Cotovelo
wss <- vector()
for (k in 1:10) {
  wss[k] <- sum(kmeans(dados_norm, centers = k, nstart = 25)$withinss)
}
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Número de Clusters", ylab = "Soma dos Quadrados Intra-cluster")
# Após análise do grafico para o Método do Cotovelo, verificamos que de k = 1 
# para k = 2 existe redução significativa da Soma dos Quadrados Intra-Cluster, 
# mas de k = 2 para k = 3 a redução já não é significativa, porque essas 
# reduções estabilizam, ou seja, o gráfico fica achatado, por isso concluímos
# que melhor valor de k é o valor k = 2.


# Calcular Média da Silhueta para k = 2 a 6
silhouette_medios <- numeric()
intervalo_k <- 2:6
for (k in intervalo_k) {
  set.seed(123)
  km <- kmeans(dados_norm, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(dados_norm))
  silhouette_medios[k - 1] <- mean(sil[, 3])
}
plot(intervalo_k, silhouette_medios, type = "b", pch = 19, col = "darkorange",
     xlab = "Número de Clusters (k)", ylab = "Silhouette Médio",
     main = "Silhouette Médio para Diferentes Valores de k")
grid()
# A análise do Média da Silhueta reforça a conclusão anterior baseada no método 
# do cotovelo, o valor ideal de k é 2. Apesar de a qualidade dos clusters ser 
# apenas fraca a média, não há melhoria ao aumentar o número de clusters, o que 
# justifica a escolha de dois perfis de qualidade de vida.


# K-means com k = 2
set.seed(123)
kmeans_result <- kmeans(dados_norm, centers = 2, nstart = 25)
dados$cluster <- as.factor(kmeans_result$cluster)


# Gráfico de Perfis dos Cluster
perfil_clusters <- aggregate(dados[, 6:10], by = list(Cluster = dados$cluster), mean)
perfil_melt <- melt(perfil_clusters, id.vars = "Cluster")

ggplot(perfil_melt, aes(x = variable, y = value, group = Cluster, color = Cluster)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Perfis de Clusters de Qualidade de Vida",
       x = "Domínio de Qualidade de Vida",
       y = "Média das Pontuações (0 a 100)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Perfil do Cluster:
# 1: Qualidade de Vida Baixa – Menor pontuação em todos os domínios avaliados, 
# pode indicar maior vulnerabilidade física, emocional e social, inidica 
# mau-estar e perceção negativa da vida.
# 2: Qualidade de Vida Alta – Maiores pontuações em todos os domínios avaliados, 
# especialmente nos aspectos psicológicos e sociais, indica maior bem-estar e 
# percepção positiva da vida.


# Árvore de Decisão
modelo_arvore <- rpart(cluster ~ ., data = dados[, 6:11], method = "class",
                       cp = 0.001, minsplit = 2, parms = list(split = "gini"))
rpart.plot(modelo_arvore, type = 4, extra = 101,
           main = "Árvore de Decisão para Previsão de Cluster",
           cex = 0.55)
# A árvore mostra que apenas 3 variáveis são suficientes para distinguir os 
# perfis com alta precisão.
# A variável Domínio Psicológico é o fator mais decisivo, sendo o primeiro a 
# ser utilizado para dividir a árvore. 
# Depois temos a variável Domínio Meio Ambiente que é decisivo principalmente 
# para os indivíduos do Cluster 1, correspondente à Qualidade de Vida Baixa.
# Por fim, temos a variável Domínio Geral que é decisivo principalmente para os
# indivíduos do Cluster 2, correspondente à Qualidade de Vida Alta


# Mapa de Calor dos Perfis
matriz_cluster <- aggregate(dados[, 6:10], by = list(Cluster = dados$cluster), mean)
rownames(matriz_cluster) <- paste("Cluster", matriz_cluster$Cluster)
matriz_cluster <- as.matrix(matriz_cluster[, -1])
pheatmap(matriz_cluster,
         cluster_rows = FALSE, cluster_cols = FALSE,
         display_numbers = TRUE, number_format = "%.1f",
         fontsize = 12, fontsize_number = 10, angle_col = 45,
         color = colorRampPalette(c("white", "#0072B2"))(100),
         main = "Mapa de Calor dos Perfis Médios por Cluster")
# O Mapa de Calor dos Perfis mostra aquilo que já tinhamos visualizado 
# anteriormente no Gráfico de Perfis dos Cluster, o Cluster 1 apresenta menor 
# pontuação em todos os domínios avaliados, enquanto o Cluster 2 apresenta 
# maiores pontuações em todos os domínios avaliados.


# Gráfico de Pizza com o Tamanho de cada Cluster
df_pizza <- as.data.frame(table(dados$cluster))
colnames(df_pizza) <- c("Cluster", "Total")
df_pizza$Percent <- round(df_pizza$Total / sum(df_pizza$Total) * 100, 1)
df_pizza$Label <- paste0("Cluster ", df_pizza$Cluster, "\n", df_pizza$Percent, "%")

ggplot(df_pizza, aes(x = "", y = Total, fill = Cluster)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_manual(values = c("tomato", "limegreen", "royalblue")) +
  theme_void() +
  labs(title = "Distribuição de Indivíduos por Cluster") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
# O Gráfico de Pizza com o Tamanho de cada Cluster mostra que os clusters estão
# quase igualmente distribuídos, o Cluster 1 tem 50.8% dos indivíduos e o 
# Cluster 2 tem 49.2% dos indivíduos, o que nos indica que aproximadamente
# metade dos indivíduos possui Qualidade de Vida Baixa.


# PCA para Visualizar Clusters
res.pca <- PCA(dados[, 6:10], scale.unit = TRUE, graph = FALSE)
fviz_pca_ind(res.pca,
             habillage = dados$cluster,
             palette = c("tomato", "limegreen", "royalblue"),
             addEllipses = TRUE, ellipse.type = "convex",
             repel = TRUE,
             title = "Visualização dos Clusters em Componentes Principais")
# A PCA confirma a eficácia do agrupamento realizado pelo K-means, os indivíduos 
# estão bem separados entre os dois perfis e o modelo tem poder discriminativo 
# elevado, não existindo sobreposição entre os dois clusters.


# Análise de Silhueta
sil <- silhouette(as.numeric(dados$cluster), dist(dados_norm))
plot(sil, col = c("tomato", "limegreen", "royalblue"), main = "Gráfico de Silhouette")
# A largura média de silhueta é 0.31, o que se classifica como fraca a média. 
# Isso indica que, embora exista separação entre os clusters, os agrupamentos 
# não são extremamente nítidos.

# Cluster 1:
# Média da silhueta = 0.25, No limite inferior da qualidade aceitável.
# Este grupo tem mais indivíduos sugere alguma ambiguidade na alocação de
# alguns pontos.
# Cluster	    Nº de Indivíduos	  Média da Silhueta
# 1	          64	                0.25

# Cluster 2:
# Média da silhueta = 0.36, classifica-se como "fraca a média" e é melhor do 
# que a do Cluster 1.
# Este cluster mostra melhor separação em relação ao outro grupo, com mais 
# indivíduos bem agrupados.
# Cluster	    Nº de Indivíduos	  Média da Silhueta
# 2	          62	                0.36


# Tabela Final e Exportação
perfil_final <- aggregate(dados[, 6:10], by = list(Cluster = dados$cluster), FUN = mean)
tamanhos <- as.data.frame(table(dados$cluster))
colnames(tamanhos) <- c("Cluster", "Total")
(perfil_final <- merge(perfil_final, tamanhos, by = "Cluster"))
# Cluster   Domínio.Físico  Domínio.Psicológico   Domínio.Relações.Sociais  Domínio.Meio.Ambiente   Domínio.Geral   Total
# 1         69.69866        68.7500               67.05729                  67.13867                62.10938        64
# 2         83.87097        85.9543               85.21505                  79.38508                77.41935        62

write.xlsx(list(
  "Dados_com_Clusters" = dados,
  "Perfis_por_Cluster" = perfil_final
), file = "Resultados_Clusters.xlsx")

