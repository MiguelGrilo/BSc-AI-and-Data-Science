##### TRABALHO PE 2024/25
##### Alunos: 58387
#####         58656
##### NOTA = 20

## Base de Dados 1
Compras<-read.csv2("Compras.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                    sep=";", dec=",", header=T, encoding = "UTF-8")
Compras
summary(Compras)
## Base de Dados 2
Tempos1<-read.csv2("Tempos1.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                   sep=";", dec=",", header=T, encoding = "UTF-8")
Tempos1
summary(Tempos1)
## Base de Dados 3
Tempos2<-read.csv2("Tempos2.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                   sep=";", dec=",", header=T, encoding = "UTF-8")
Tempos2
summary(Tempos2)
## Base de Dados 4
Tempos3<-read.csv2("Tempos3.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                   sep=";", dec=",", header=T, encoding = "UTF-8")
Tempos3
summary(Tempos3)

## EX1 ##
## Intervalo de confiança para a Proporção
0.6*200 # = 120
binom.test(120, 200, conf.level = 0.95) # IC para a proporção
# Limite Superior IC95% (p) = 0.6684537 = 66.84537%


## EX2 ##
## Alínea 1 ##
## Teste de hipóteses e intervalo de confiança para a diferença de Proporções em amostras independentes
prop.test(c(135, 120),c(450, 600))
# Valor p = 0.0002455

## Alínea 2 ##
prop.test(c(135, 120),c(450, 600), conf.level = 0.95)
Erro_Estimativa<-(0.15502052-0.04497948)/2
Erro_Estimativa
#Erro Estimativa = 0.05502052 = 5.502052%

## EX3 ##
                # Tratamento B
# Tratamento A    Ajudou      Não Ajudou
# Ajudou          30          30
# Não ajudou      8           32
## Duas proporções em amostras emparelhadas
tabc <- as.table(rbind(c(30, 30), c(8, 32))) # tabela de contingência
dimnames(tabc)<-list(Tratamento_A = c("Ajudou", "Não ajudou"),
                     Tratamento_B = c("Ajudou", "Não ajudou"))
tabc
mcnemar.test(tabc)  # Teste de McNemar para igualdade de proporções 
# Valor p = 0.0006577


## EX4 ##
## Alínea 1 ##
# Obter a amostra a testar
BancoY<-subset(Tempos2, Banco=="Y")
#### Uma amostra ####
# Normalidade
shapiro.test(BancoY$Tempo) # teste de Shapiro-Wilk
# Valor p = 0.134
# p > 0.01 logo não se rejeita
# Em caso de não rejeição...
t.test(BancoY$Tempo, mu=8.5, alternative="greater") # Teste unilateral direito
# Valor p = 0.001185
# Assim, rejeita-se H0 e concluímos que o tempo médio de transação é superior a 8.5s.

## Alínea 2
# Normalidade
shapiro.test(BancoY$Tempo) # teste de Shapiro-Wilk
# Valor p = 0.134
# p > 0.02 logo também não se rejeita
t.test(BancoY$Tempo, conf.level=0.98) # Intervalo de confiança a 98
# IC (miu) = [8.624901 ; 9.282099] =  [8.6 ; 9.3]


## EX5 ##
## Alínea 1 ##
# Obter as amostras a testar
AlgA<-subset(Tempos1, Algoritmo=="A")
AlgB<-subset(Tempos1, Algoritmo=="B")
#### Amostras Independentes #####
# Normalidade
shapiro.test(AlgB$Tempo) # teste de Shapiro-Wilk
# Valor p = 0.8449
# p > 0.05 logo não se rejeita a normalidade
shapiro.test(AlgA$Tempo) # teste de Shapiro-Wilk
# Valor p = 0.7501
# p > 0.05 logo não se rejeita a normalidade
# Igualdade de Variâncias
var.test(AlgB$Tempo, AlgA$Tempo) # teste F de igualdade de variâncias
# Valor p = 0.3562
# p > 0.05 logo não se rejeitam variâncias iguais
t.test(AlgB$Tempo, AlgA$Tempo, var.equal=TRUE, conf.level = 0.95) # teste de Welch de comparação de médias (bilateral) e intervalo de confiança
# IC(miuB - miuA) = [0.741481 ; 1.299519] = [0.7415 ; 1.2995]
# H0 rejeita-se, admitimos então que o Algoritmo B é, em média, mais rápido que o
# Algoritmo A, porém o intervalo é menor que o do enunciado, está contido nele
# Logo é Falso

## Alínea 2 ##
t.test(AlgB$Tempo, AlgA$Tempo, var.equal=TRUE, conf.level = 0.95) # teste de Welch de comparação de médias (bilateral) e intervalo de confiança
Erro_Estimativa2<-(1.299519-0.741481)/2 
Erro_Estimativa2
# Erro Estimativa = 0.279019
# 0.279019 > 0.25
# Logo é Falso

## Alínea 3 ##
#### Amostras Independentes #####
# Normalidade
shapiro.test(AlgB$Tempo) # teste de Shapiro-Wilk
# Valor p = 0.8449
# p > 0.01 logo não se rejeita a normalidade
shapiro.test(AlgA$Tempo) # teste de Shapiro-Wilk
# Valor p = 0.7501
# p > 0.01 logo não se rejeita a normalidade
# Igualdade de Variâncias
var.test(AlgB$Tempo, AlgA$Tempo) # teste F de igualdade de variâncias
# Valor p = 0.3562
# p > 0.01 logo não se rejeitam variâncias iguais
t.test(AlgB$Tempo, AlgA$Tempo, mu=0.5, var.equal=TRUE, alternative="greater")
# Valor p = 0.0002724
# p < 0.01 logo rejeitamos H0
# Concluímos que o Algoritmo B consegue ser mais rápido, em média, mais de meio
# segundo que o Algoritmo A.


## EX6 ##
# criar amostra das diferenças
Tempos3$dif<-Tempos3$Depois-Tempos3$Antes
# Normalidade
shapiro.test(Tempos3$dif) # teste de Shapiro-Wilk aos valores das diferenças
# Valor p = 0.04627
# p > 0.01 logo não se rejeita a normalidade
t.test(Tempos3$Depois, Tempos3$Antes, paired=TRUE, mu=-2, alternative="less")
# Valor p = 0.0008515


## EX7 ##
# criar amostra das diferenças
Compras$dif<-Compras$Depois-Compras$Antes
# Normalidade
shapiro.test(Compras$dif) # teste de Shapiro-Wilk aos valores das diferenças
# Valor p = 0.1001
# p > 0.02 logo não se rejeita a normalidade
t.test(Compras$Depois, Compras$Antes, paired=TRUE, conf.level = 0.98)
# Limite Inferior IC98% (miuDepois-miuAntes) = 8.664899