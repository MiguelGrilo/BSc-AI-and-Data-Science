#EXERCICIO 1
# criar a base de dados
cart<-read.csv2("cartoes1.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                sep=";", dec=",", header=T, encoding = "UTF-8")
cart
summary(cart)
# criar amostra das diferenças
cart$dif<-cart$Parte2_Amarelos-cart$Parte1_Amarelos
# Normalidade
shapiro.test(cart$dif) # teste de Shapiro-Wilk aos valores das diferenças
# Em caso de não rejeição
t.test(cart$Parte2_Amarelos, cart$Parte1_Amarelos, mu=0, paired=TRUE, alternative="greater") # Teste de Hipóteses


#EXERCICIO 2
#ALINEA A
# leitura da base de dados
dados <- read.csv2("commodities2.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                   sep=";", dec=",", header=T, encoding = "UTF-8")
summary(dados) # Síntese dos dados
names(dados) # designações das variáveis
# Obter as amostras a testar
Ano2022<-subset(dados, Ano2 == "2022")
Ano2023<-subset(dados, Ano2 =="2023")
#### Amostras Independentes #####
# Normalidade
shapiro.test(Ano2022$Ultimo2) # teste de Shapiro-Wilk aos valores da amostra do retorno diário da Bitcoin
shapiro.test(Ano2023$Ultimo2) # teste de Shapiro-Wilk aos valores da amostra do retorno diário da Dogecoin
# Em caso de rejeição da normalidade...
# Wilcoxon-Mann_Whitney
wilcox.test(Ano2022$Ultimo2, Ano2023$Ultimo2, mu = 215, alternative = "less")

#ALINEA B
t.test(Ano2023$Ultimo2, Ano2022$Ultimo2, var.equal = TRUE, conf.level = 0.98) # teste de Welch de comparação de médias (bilateral) e intervalo de confiança para a diferença de rendimentos médios
ErroEstimativa <- abs((-217.4471 -(-207.6683))/2)
ErroEstimativa


#EXERCICIO 3
dados <- read.csv2("commodities3.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                   sep=";", dec=",", header=T, encoding = "UTF-8")
summary(dados) # Síntese dos dados
names(dados) # designações das variáveis
# Obter as amostras a testar
Ano2022<-subset(dados, Ano3 == "2022")
#### Amostras Independentes #####
# Normalidade
shapiro.test(Ano2022$Ultimo3) # teste de Shapiro-Wilk aos valores da amostra do retorno diário da Bitcoin
# Em caso de não rejeição...
t.test(Ano2022$Ultimo3, mu=687, alternative="less") # H0:μ1-μ2≤m vs H1:μ1-μ2>m


#EXERCICIO 4
# leitura da base de dados
dados <- read.csv2("commodities2.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                   sep=";", dec=",", header=T, encoding = "UTF-8")
summary(dados) # Síntese dos dados
names(dados) # designações das variáveis
# Obter a amostra a testar
Ano2023<-subset(dados, Ano2 == "2023")
#### Uma amostra ####
# Normalidade
shapiro.test(Ano2023$Ultimo2) # teste de Shapiro-Wilk aos valores da amostra do retorno diário da Ethereum
# Em caso de não rejeição...
t.test(Ano2023$Ultimo2, conf.level=0.90) # Intervalo de confiança a 90% para o rendimento médio da Ethereum
#Limite superior = 475.0016 


#EXERCICIO 5
# criar a base de dados
cart<-read.csv2("cartoes1.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                sep=";", dec=",", header=T, encoding = "UTF-8")
cart
summary(cart)
# criar amostra das diferenças
cart$dif<-cart$Parte2_Vermelhos-cart$Parte1_Vermelhos
# Normalidade
shapiro.test(cart$dif) # teste de Shapiro-Wilk aos valores das diferenças
# Em caso de não rejeição
var.test(cart$Parte2_Vermelhos, cart$Parte1_Vermelhos) # teste F de igualdade de variâncias
t.test(cart$Parte2_Vermelhos, cart$Parte1_Vermelhos, var.equal=TRUE, conf.level=0.98) # teste t (variâncias iguais)
#Limite superior do intervalo a 98% = 0.56313994


#QUESTÕES EXTRA
#EXERCÍCIO 1
## Duas proporções em amostras emparelhadas
tabc <- as.table(rbind(c(50, 20), c(30, 50))) # tabela de contingência
dimnames(tabc)<-list(Antes = c("Aderente", "Não Aderente"),
                     Depois = c("Aderente", "Não Aderente"))
tabc
mcnemar.test(tabc)  # Teste de McNemar para igualdade de proporções 


#EXERCÍCIO 2
#ALINEA A
prop.test(c(100,50), c(250, 200), alternative = "greater")

#ALINEA B
prop.test(c(100, 50), c(250, 200), conf.level = 0.95)
ErroEstimativa <- abs(0.23987663-0.06012337)/2
ErroEstimativa

#ALINEA C
binom.test(100, 250, conf.level = 0.98)

#ALINEA D
# para p0 diferente de 0, correr a função
teste_diferenca_proporcoes <- function(x1, n1, x2, n2, p0, tipo_teste = "bilateral") {
  # Calcular proporções
  p1 <- x1 / n1
  p2 <- x2 / n2
  # Estatística z
  z <- ((p1 - p2) - p0) / sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))
  # Calcular valor p de acordo com o tipo de teste
  if (tipo_teste == "bilateral") {
    p_val <- 2 * (1 - pnorm(abs(z)))  # Duas caudas
  } else if (tipo_teste == "unilateral direito") {
    p_val <- 1 - pnorm(z)  # Cauda superior (H1: p1 - p2 > p0)
  } else if (tipo_teste == "unilateral esquerdo") {
    p_val <- pnorm(z)  # Cauda inferior (H1: p1 - p2 < p0)
  } else {
    stop("O tipo de teste deve ser 'bilateral', 'unilateral direito' ou 'unilateral esquerdo'.")
  }
  # resultados 
  list(
    p1 = p1,
    p2 = p2,
    z = z,
    p_val = p_val,
    tipo_teste = tipo_teste,
    p0=p0
  )
}
teste <- teste_diferenca_proporcoes(x1 = 100, x2 = 50,n1 = 250, n2 = 200, p0 = 0.1, "unilateral direito")
print(teste)
# Valor p = 0.1255185