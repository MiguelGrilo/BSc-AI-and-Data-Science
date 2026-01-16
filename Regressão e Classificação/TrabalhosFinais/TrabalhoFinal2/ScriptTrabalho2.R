#### REGRESSÃO E CLASSIFICAÇÃO ####
#### TRABALHO 2 ####
## Miguel Grilo    58387
## Jorge Couto     58656

rm(list=ls(all=TRUE))

###---------------###
#### EXERCÍCIO 1 ####
###---------------###
library(ggplot2)
library(rstatix)
# Cria uma semente para que a amostra possamos observar os mesmos dados
set.seed(0)
summary(diamonds)
# Subconjunto com 4 variáveis quantitativas e 1 qualitativa
dados <- diamonds[sample(nrow(diamonds), 1000), 
                  c("price", "carat", "depth", "table", "cut")]
summary(dados)

#   Variáveis Quantitativas
# price -> Preço do Diamante (a nossa variável resposta)
# carat -> Peso do Diamante
# depth -> Profundidade Relativa do Diamante (Medida que expressa a altura do 
#          diamante em relação à largura)
# table -> Proporção Entre o Diâmetro da "Mesa" do Diamante e a sua Largura
#   Variável Qualitativa
# cut   -> Qualidade do Corte do Diamante

# O QUE FAZER:
# Considerar price como variável resposta (i.e. tirar do modelo e colocar isso como a que queremos explicar)
# Categorizar a variável price (categorizar pelos quartis)

# Verificar se o dataset contêm dados omissos
sum(is.na(dados))
colSums(is.na(dados))
# O dataset não possui dados omissos

# Categorizar a variável price através dos quartis:
quintis <- quantile(dados$price, probs = c(0, 0.25, 0.5, 0.75, 1))

dados$price <- cut(dados$price, 
                    breaks = quintis, 
                    include.lowest = TRUE,
                    labels = c("Baixo", "Médio-Baixo", "Médio-Alto", "Alto"))

# Verificar
table(dados$price)
summary(dados)

# Tornar a variável cut, uma variável ordinal, em numérica:
dados$cut <- as.numeric(dados$cut)

# Para price não afetar a resposta, já que price seria a variável resposta,
# Devemos retirá-lo do conjunto de dados. Criemos, então, um novo conjunto
# De dados com todas as variáveis menos a variável categórica price.
dados1 <- dados[c("carat", "depth", "table", "cut")]
summary(dados1)

# Matriz de distâncias euclidianas
matriz.distancia <- dist(dados1, method="euclidean")
# Uma vez com a matriz de distâncias euclidianas, podemos usar a análise classificatória
# Comecemos pelo método simples
simples<-hclust(matriz.distancia, method="single")
simples$height

# Dendograma rápido
plot(simples)
# Pouco legível, mas precisamos apenas de ver um bom ponto de corte dos grupos
# Usemos o screeplot para saber um bom ponto de corte
nH <- length(simples$height)
plot(c(simples$height[nH:1],0), type="b", xaxt="n")
axis(1, at = seq(0, 200, by = 10))
abline(h = 1.1, col="red")
# Portanto, pelo screeplot, um bom ponto de corte seria 1.1
(membros_simples <- cutree(simples, h = 1.1)) # Corte a partir dos 1.1
table(membros_simples)
# Sugestão: cortar em menos grupos.
(membros_simples <- cutree(simples, k = 20))
table(membros_simples)
# Cortar em poucos grupos resulta apenas em um grupo geral e amostras vazias.

# Usemos, agora, o método completo
completo<-hclust(matriz.distancia, method="complete")
completo$height
# Dendograma do método completo
plot(completo)
# Começa a haver grande diferença a partir da altura igual a 6
# Usemos o screeplot para verificar se com o screeplot podemos tirar a mesma conclusão
nH2 <- length(completo$height)
plot(c(completo$height[nH2:1],0), type="b", xaxt="n")
abline(h = 4, col="red")
# Pelo screeplot, o nosso ponto de corte deveria ser muito anterior, apenas nos 2.5
(membros_completo <- cutree(completo, h = 4))
table(membros_completo)
# Novamente, temos vários grupos com poucas amostras
# Todavia, existe melhor divisão com o método completo do que com o método simples
# Novamente, podemos considerar unir grupos
# Estandartizamos, visto que pode ser que depth e table estejam a dominar
# Visto que depth e table apresentam um domínio maior comparados a carat e cut.

dadosZ<-within(dados,{
  caratZ<-scale(carat)
  depthZ<-scale(depth)
  tableZ<-scale(table)
  cutZ<-scale(cut)
}
)

head(dadosZ)
dadosZ1<-dadosZ[,-c(1:5)]
# dadosZ1 será usado para criar a matriz euclideana

# Euclideana
(matriz.distancia.e2 <- dist(dadosZ1, method="euclidean"))
resultado2s<-hclust(matriz.distancia.e2, method="single")
plot(resultado2s)
# Usemos o screeplot para conseguir perceber um ponto de corte
nHb <- length(resultado2s$height)
plot(c(resultado2s$height[nHb:1],0), type="b", xaxt="n")
abline(h = 0.6, col="red")
# 0.6 parece um bom ponto de corte pelo screeplot
(membros_simples2 <- cutree(resultado2s, h = 0.6))
table(membros_simples2)
# Com as variáveis estandartizadas nota-se maior divisão ainda
# Método completo:
resultado2c<-hclust(matriz.distancia.e2, method="complete")
plot(resultado2c)
# Usemos o screeplot para conseguir perceber um ponto de corte
nH2b <- length(resultado2c$height)
plot(c(resultado2c$height[nH2b:1],0), type="b", xaxt="n")
abline(h = 1.5, col="red")
# 1.5 Parece um bom ponto de corte pelo screeplot
(membros_completo2 <- cutree(resultado2c, h = 1.5))
table(membros_completo2)
# Maior divisão ainda, não parecem haver diferenças significativas
# Entre usar as variáveis estandartizadas ou não.

# Seguir pelo método Ward:
ward<-hclust(matriz.distancia, method="ward.D2")
ward$height
# Dendograma rápido
plot(ward)
# Usemos o screeplot para saber um bom ponto de corte
nH3 <- length(ward$height)
plot(c(ward$height[nH3:1],0), type="b", xaxt="n")
abline(h = 10, col="red")
# Portanto, pelo screeplot, um bom ponto de corte seria 10
# (Mais alto mas para termos menos grupos)
(membros_ward <- cutree(ward, h = 10)) # Corte a partir dos 10
table(membros_ward)
# Parece ser uma boa divisão de grupos! Usemos as variáveis estandartizadas:
resultado2w<-hclust(matriz.distancia.e2, method="ward.D2")
plot(resultado2w)
# Usar o screeplot
nH3b <- length(resultado2w$height)
plot(c(resultado2w$height[nH3b:1],0), type="b", xaxt="n")
abline(h = 8, col="red")
# h = 8 parece bom para que tenhamos menos grupos
(membros_ward2 <- cutree(resultado2w, h = 8))
table(membros_ward2)
# Com as variáveis estandartizadas parece haver melhor divisão!

# Tabela cruzada entre os grupos e a variável resposta
# Verificar se os grupos explicam bem o price
(tabela_grupos_price <- table(membros_ward2, dados$price))
# Criar um gráfico para analisar melhor:
# Criar um data frame com a resposta e o grupo
df_analise <- data.frame(dados)
df_analise$grupo <- as.factor(membros_ward2)

# Reobter o price no formato original (variável contínua)
set.seed(0) # Para garantir que voltamos a tirar o preço dos mesmos 1000 indivíduos
dadosprice <- diamonds[sample(nrow(diamonds), 1000),
                       c("price")]
df_analise$pricecont <- dadosprice$price
objeto <- games_howell_test(df_analise, pricecont~grupo)
View(objeto)
sort(by(df_analise$pricecont, df_analise$grupo, mean))
# Verificando a ordem das médias dos grupos e os níveis de significância
# Podemos verificar que grupos unir ao ver se são significativos pelo games_howell
# Com outros grupos vizinhos na ordenação de média.
# Com isso, chegamos a somente cinco clusters:
# 11, 10 e 1;
# 5, 8, 4, 6 e 12;
# 14, 3, 9 e 15;
# 7 e 13;
# 2

df_analise$grupo_unificado <- as.character(df_analise$grupo)
df_analise$grupo_unificado[df_analise$grupo %in% c(11, 10, 1)] <- "11_10_1"
df_analise$grupo_unificado[df_analise$grupo %in% c(5, 8, 4, 6, 12)] <- "5_8_4_6_12"
df_analise$grupo_unificado[df_analise$grupo %in% c(14, 3, 9, 15)] <- "14_3_9_15"
df_analise$grupo_unificado[df_analise$grupo %in% c(7, 13)] <- "7_13"
df_analise$grupo_unificado <- as.factor(df_analise$grupo_unificado)
table(df_analise$grupo_unificado)

###---------------###
#### EXERCÍCIO 2 ####
###---------------###
# Utilizar a Análise Discriminante nos grupos formados
# Dividir conjunto de dados: 75% para estimar função discriminante e 25% para testar
(nvalido <- dim(df_analise)[1])
linhas <- sample(1:2, nvalido,
                 replace = TRUE,
                 prob = c(0.75, 0.25))
dadosA <- df_analise[linhas==1,]  # dados para ajuste
dadosT <- df_analise[linhas==2,]  # dados para teste
count(dadosA)
count(dadosT)

summary(df_analise)
## selecionar variaveis discriminantes -----------------------------------------

cor(dadosA[,c(2, 3:5)])  # todas as correlacoes <0.8
# Ignoramos as variáveis price para ver que outras variáveis explicam os clusters
# Descontando o price por ter sido usado para formar os clusters (muito correlacionada)
# Criar subset dos dados para ajuste apenas com as variáveis a entrar no método stepwise
dadosAstep<- dadosA[ c(2:5, which(names(dadosA) == "grupo_unificado"))]
# metodo stepwise com criterio lambda de Wilks
library(klaR)
mod_step <- greedy.wilks(grupo_unificado ~ ., data = dadosAstep, niveau = 0.05)
mod_step  
# Todas as variáveis são escolhidas para explicar os grupos

## estimar funcao discriminante ------------------------------------------------
# nota: existem 4 funções
library(MASS)
fit <- lda(grupo_unificado ~ carat + depth + cut + table, data=dadosA, na.action="na.omit")
fit
# a variavel que mais discrimina em LD1 é carat e nas outras (LD2, LD3 e LD4) cut
# LD1 explica melhor a variância geral dos resultados.

## validar pressupostos --------------------------------------------------------

# 1. inexistencia de multicolinearidade
cor(dadosA[,c("carat", "depth", "cut", "table")])  
# todas as correlacoes <0.8

# 2. normalidade multivariada
variaveis <- c("carat", "depth", "cut", "table")
# via teste de Mardia
library(MVN)
mvn(dadosA[, variaveis], mvnTest="mardia")
# graves problemas de assimetria e achatamento
# carat com assimetria elevada positiva
# depth e cut com assimetria mediana negativa
# table com assimetria mediana positiva
# Não se pode assumir normalidade multivariada, também

# via QQplot
mvn(dadosA[, variaveis], multivariatePlot = "qq")
# grande desvio, não se pode considerar normal

# mas a AD e robusta à violacao dos pressupostos desde que:
# * dimensao da amostra menor > numero de variaveis discriminantes
table(dadosA$grupo_unificado)
table(dadosT$grupo_unificado)
# n menor = 41 > 4 variaveis discriminantes
# * medias dos grupos nao sao proporcionais às suas variancias
medias <- aggregate(dadosA[,variaveis], list(dadosA$grupo_unificado), mean)  # medias por grupo
desvpad <- aggregate(dadosA[,variaveis], list(dadosA$grupo_unificado), sd)  # medias por grupo
as.matrix(medias[,-1])/as.matrix(desvpad[,-1]^2)*100
# Existem problemas com o grupo 3 (Luxo) na variável cut
# O que faz sentido, porque todas as amostras do grupo Luxo têm o mesmo valor!
# Ou seja, não existe qualquer variabilidade das amostras: total separação dos dados
# O método multinomial será um método melhor que a análise discriminante
# Ainda assim, continuemos a seguir com a análise discriminante

## Avaliar funcao discriminante linear -----------------------------------------

# do ponto do vista do ajuste
previstos <- predict(fit)
library(caret)
confusionMatrix(previstos$class, dadosA$grupo_unificado)
# 74.6% classificacoes corretas > 44.4% ao acaso
# sensibilidade elevada nos grupos 14_3_9_15, 5_8_4_6_12 e 7_13
# sensibilidade baixa nos outros dois grupos
# especificidade alta em todos os grupos

library(klaR) 
partimat(grupo_unificado ~ carat + depth + cut + table, 
         data = dadosA, 
         method = "lda")
# nota: sao 6 graficos

# do ponto de vista da reacao a novas observacoes
previstosT <- predict(fit, newdata = subset(dadosT, select=-grupo_unificado))
confusionMatrix(previstosT$class, dadosT$grupo_unificado)
# 75.8% classificações corretas > 40.5% classificações à toa
# Modelo prediz melhor do que se fosse ao calhas (p > 0.001)
# Sensitividade alta para os grupos 14_3_9_15, 5_8_4_6_12 e 7_13
# Baixa para os outros dois
# Especificidade alta para todos os grupos
# Especificidade quase igual a 1 no grupo 2 pela falta de variação já antes notada

# Pelos pressupostos violados, não é necessário prosseguir para a AD quadrática



###---------------###
#### EXERCÍCIO 3 ####
###---------------###
library(car)
library(nnet)
library(dplyr)
library(ggplot2)
library(mlogit)
#library(VGAM)

# Caracterização dos Grupos
# Ver como price varia por grupo através de um gráfico de barras
ggplot(df_analise, aes(x = grupo_unificado, fill = price)) +
  geom_bar(position = "fill") +
  ylab("Proporção") +
  ggtitle("Distribuição da variável 'price' por grupo") +
  theme_minimal()

# Ver como carat varia por grupo
ggplot(df_analise, aes(x = grupo_unificado, y = carat, fill = grupo_unificado)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Distribuição de 'carat' por grupo")
# Ver como depth varia por grupo
ggplot(df_analise, aes(x = grupo_unificado, y = depth, fill = grupo_unificado)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Distribuição de 'depth' por grupo")
# Ver como table varia por grupo
ggplot(df_analise, aes(x = grupo_unificado, y = table, fill = grupo_unificado)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Distribuição de 'table' por grupo")
# Ver como cut varia por grupo
ggplot(df_analise, aes(x = grupo_unificado, y = cut, fill = grupo_unificado)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Distribuição de 'cut' por grupo")
summary(df_analise$grupo_unificado)
levels(df_analise$grupo_unificado) <- c("Equilibrados", "Intermédios-Altos", 
                                        "Baratos", "Intermédios-Baixos", "Luxo")
summary(df_analise$grupo_unificado)

# Equilibrados - Grupo com diamantes em todas as categorias de preço
# Carat baixo a intermédio, depth alto, table equilibrado e cut também

# Intermédios-Altos - Grupo com diamantes entre o preço alto e médio-alto
# Carat intermédio, depth equilibrado, table equilibrado e cut ótimo

# Baratos - Grupo com mais diamantes de preço baixo, mas alguns médio-baixo e poucos médio-alto
# Têm o carat e table mais baixos, a menor variedade em depth e o maior cut

# Intermédios-Baixos - Grupo com diamantes em todas as categorias de preço, mas com mais baixo e médio-baixo
# Segundo menor carat, depth altamente variável, table mediano e cut muito variável

# Luxo - Grupo com mais diamantes de preço alto, com quase todos em alto preço
# Têm o maior carat (embora com vários outliers), depth e table medianos e um cut médio-alto


# Análise preliminar
table(df_analise$grupo_unificado, df_analise$cut)
by(df_analise$carat, df_analise$grupo_unificado, summary)
by(df_analise$depth, df_analise$grupo_unificado, summary)
by(df_analise$table, df_analise$grupo_unificado, summary)

library(crosstable)
crosstable(df_analise,                     
           c(carat, depth, table, cut),
           by=grupo_unificado,              
           #total=TRUE,               
           test=TRUE,                 
           showNA='no',               
           percent_digits=1) %>%      
  as_flextable()                    
# Todas significativas


# Ajuste do Modelo
# Modelos univariados
df_analise$grupo_unificado <- relevel(df_analise$grupo_unificado, ref="Equilibrados")

fit_carat <- multinom(grupo_unificado ~ carat, data = df_analise)
summary(fit_carat)
z <- summary(fit_carat)$coefficients / summary(fit_carat)$standard.errors
p <- 2 * (1 - pnorm(abs(z)))
print(p)
# Todos os valores p < 0.20

fit_depth <- multinom(grupo_unificado ~ depth, data = df_analise)
summary(fit_carat)
z <- summary(fit_depth)$coefficients / summary(fit_depth)$standard.errors
p <- 2 * (1 - pnorm(abs(z)))
print(p)
# Todos os valores p < 0.20

fit_table <- multinom(grupo_unificado ~ table, data = df_analise)
summary(fit_table)
z <- summary(fit_table)$coefficients / summary(fit_table)$standard.errors
p <- 2 * (1 - pnorm(abs(z)))
print(p)
# Todos os valores p < 0.20

fit_cut <- multinom(grupo_unificado ~ cut, data = df_analise)
summary(fit_cut)
z <- summary(fit_cut)$coefficients / summary(fit_cut)$standard.errors
p <- 2 * (1 - pnorm(abs(z)))
print(p)
# Todos os valores p < 0.20

# Modelo multinomial múltiplo
fit_completo <- mlogit(grupo_unificado ~ 1 | carat + depth + table + cut,         # variavel y~1 | covariaveis
               reflevel = "Equilibrados",            # opcional: indicar qual a categoria y de referencia
               data = df_analise, shape="wide")
summary(fit_completo)
# McFadden R^2 = 0.59648 

# Remover variáveis não significativas 
# (remover por ordem decrescente de p-value do teste TRV no último modelo em estudo)
# A variável cut pode ser removida?
fit_cut <- mlogit(grupo_unificado ~ 1 | carat + depth + table,         # variavel y~1 | covariaveis
                       reflevel = "Equilibrados",            # opcional: indicar qual a categoria y de referencia
                       data = df_analise, shape="wide")
summary(fit_cut)
# McFadden R^2 = 0.50703 
library(lmtest)
lrtest(fit_completo, fit_cut)  
# Valor p < 0.0001
# Não remover a variável cut

# A variável depth pode ser removida?
fit_depth <- mlogit(grupo_unificado ~ 1 | carat + table + cut,         # variavel y~1 | covariaveis
                  reflevel = "Equilibrados",            # opcional: indicar qual a categoria y de referencia
                  data = df_analise, shape="wide")
summary(fit_depth)
# McFadden R^2 = 0.57475  
lrtest(fit_completo, fit_depth)  
# Valor p < 0.0001
# Não remover a variável depth

# A variável table pode ser removida?
fit_table <- mlogit(grupo_unificado ~ 1 | carat + depth + cut,         # variavel y~1 | covariaveis
                    reflevel = "Equilibrados",            # opcional: indicar qual a categoria y de referencia
                    data = df_analise, shape="wide")
summary(fit_table)
# McFadden R^2 = 0.52683   
lrtest(fit_completo, fit_table)  
# Valor p < 0.0001
# Não remover a variável table

# Interações
#fit_inter_1 <- mlogit(grupo_unificado ~ 1 | carat * cut + depth + table,
#                      reflevel = "Equilibrados",
#                      data = df_analise, shape = "wide")
#lrtest(fit_inter_1, fit_completo)
# Valor p < 0.0001
# Interação significativa
#fit_inter_2 <- mlogit(grupo_unificado ~ 1 | carat * depth + table + cut,
#                      reflevel = "Equilibrados",
#                      data = df_analise, shape = "wide")
#lrtest(fit_inter_2, fit_completo)
# Valor p < 0.0001
# Interação significativa
#fit_inter_3 <- mlogit(grupo_unificado ~ 1 | carat * table + depth + cut,
#                      reflevel = "Equilibrados",
#                      data = df_analise, shape = "wide")
#lrtest(fit_inter_3, fit_completo)
# Valor p < 0.0001
# Interação significativa
#fit_inter_4 <- mlogit(grupo_unificado ~ 1 | depth * cut + carat + table,
#                      reflevel = "Equilibrados",
#                      data = df_analise, shape = "wide")
# Apresenta erro, o que pode significar que existe pouca variação
#fit_inter_5 <- mlogit(grupo_unificado ~ 1 | depth * table + carat + cut,
#                      reflevel = "Equilibrados",
#                      data = df_analise, shape = "wide")
# Apresenta erro, o que pode significar que existe pouca variação
#fit_inter_6 <- mlogit(grupo_unificado ~ 1 | table * cut + carat + depth,
#                      reflevel = "Equilibrados",
#                      data = df_analise, shape = "wide")
# Apresenta erro, o que pode significar que existe pouca variação
#fit_inter_final <- mlogit(grupo_unificado ~ 1 | carat * cut + carat * depth + carat * table,
#                          reflevel = "Equilibrados",
#                          data = df_analise, shape = "wide")
#lrtest(fit_completo, fit_inter_final)  
# Valor p < 0.0001
# Modelo com interações é significativo
#AIC(fit_inter_final, fit_completo)
#                   df       AIC
# fit_inter_final   32   985.661
# fit_completo      20  1131.352
# O modelo com interações apresenta menor valor de AIC então é definitivamente o
# que melhor se ajusta
# No entanto as interações resultariam num modelo demasiado complexo então 
# deixamo-las de parte para permitir uma análise do estudo

# Modelo final
modelo_multinom <- multinom(grupo_unificado ~ carat + depth + table + cut, data = df_analise)
summary(modelo_multinom)
modelo_multinom


# Validação dos Pressupostos
# Multicolinearidade
vif(modelo_multinom)
# vif não é compativel com multinom ou mlogit então recorremos a glm
multicolinearidade <- glm(grupo_unificado ~ carat + depth + table + cut,  
                          data = df_analise, family = binomial(link = logit))
summary(multicolinearidade)
vif(multicolinearidade)
# carat     depth     table     cut 
# 1.045439  1.471997  1.478642  1.179918 
# Todos os vif são inferiores a 5 o que indica que não existe multicolinearidade
# significativa, ou seja, são estatisticamente independentes

# Linearidade
# Testado via método dos polinómios fracionários
library(mfp)
# mfp não suporta as interações então testamos sem elas
modelo_mfp <- mfp(grupo_unificado ~ fp(carat) + fp(depth) + fp(table) + cut,  # usar a função fp nas variáveis quantitativas
               data = df_analise, family = binomial(link = logit))
summary(modelo_mfp)
# As variáveis quantitativas não admitem linearidade, indicando que em todas as 
# variáveis a transformação aplicada é significativa e melhor ajustada para o 
# modelo

df_analise$carat2 <- df_analise$carat^2
df_analise$carat2_log <- df_analise$carat^2 * log(df_analise$carat)
df_analise$depth_scaled <- df_analise$depth / 100
df_analise$depth2 <- df_analise$depth_scaled^-2
df_analise$depth2_log <- df_analise$depth2 * log(df_analise$depth_scaled)
df_analise$table_scaled <- df_analise$table / 100
df_analise$table_inv <- df_analise$table_scaled^-1
df_analise$table_sqrt_inv <- df_analise$table_scaled^-0.5

transformacoes <- df_analise[, c("carat2", "carat2_log", "depth2", "depth2_log", "table_inv", "table_sqrt_inv")]
pairs(transformacoes)
cor(transformacoes)
# Existe correlação muito alta entre:
# carat2 e carat2_log
# depth2 e depth2_log
# table_inv e table_sqrt_inv
# Excluímos então as variáveis colineares

teste_mfp <- mlogit(grupo_unificado ~ 1 | carat2 + depth2 + table_inv + cut,
                  data = df_analise,
                  shape = "wide",
                  reflevel = "Equilibrados")
summary(modelo_multinom)
summary(teste_mfp)
AIC(modelo_multinom, teste_mfp)
#           df      AIC
# multinom  20 1131.452
# teste_mfp 20 1152.715
# Observamos então que apesar das sugestões de transformações, o modelo acaba
# por, mesmo assim, ter melhor performance sem a aplicação das transformações,
# apresentado maior McFadden R^2 e menor AIC no modelo original.


# Avaliação da qualidade do modelo e bondade do ajustamento
# Adequabilidade
summary(modelo_multinom)
# Residual Deviance = 1091.452 
# AIC = 1131.452 

# Bondade do ajustamento
# Teste de Hosmer e Lemeshow
library(generalhoslem)          # ativar pacote necessario
generalhoslem::logitgof(df_analise$grupo_unificado, fitted(modelo_multinom, outcome = FALSE))
generalhoslem::logitgof(df_analise$grupo_unificado, fitted(modelo_multinom, outcome = FALSE))$expected
# X-squared = 86.123
# Valor p < 0.0001
# Rejeitamos a hipótese de um bom ajuste com g=10
generalhoslem::logitgof(df_analise$grupo_unificado, fitted(modelo_multinom, outcome = FALSE), g=6)
generalhoslem::logitgof(df_analise$grupo_unificado, fitted(modelo_multinom, outcome = FALSE), g=6)$expected
# X-squared = 61.045
# Valor p < 0.0001
# Rejeitamos a hipótese de um bom ajuste com g=6

# R2 de Nagelkerke (e R2 de McFadden)
ll_completo <- logLik(modelo_multinom)
mod_nulo <- multinom(grupo_unificado ~ 1, data = df_analise)
ll_nulo <- logLik(mod_nulo)
n <- nrow(df_analise)
r2_mcfadden <- 1 - (as.numeric(ll_completo) / as.numeric(ll_nulo))
r2_nagelkerke <- r2_mcfadden / (1 - as.numeric(ll_completo) / n)
c(R2_McFadden = r2_mcfadden, R2_Nagelkerke = r2_nagelkerke)
# R2_McFadden = 0.60     
# R2_Nagelkerke = 0.39
# Embora valores não muita altos já indicam uma boa capacidade explicativa


# Análise dos Resíduos
# Teste de Cessie-van Houwelingen e coef Brier
library(rms)
mod_lrm <- lrm(grupo_unificado ~ carat + depth + table + cut, data=df_analise, x=TRUE, y=TRUE)
resid(mod_lrm, 'gof')
#                                     z           valor p
# y>=Intermédios-Altos           0.7108255    4.771923e-01
# y>=Baratos                  -137.6156197    0.000000e+00
# y>=Intermédios-Baixos         29.2599648    3.353656e-188
# y>=Luxo                        0.6375058    0.000000e+00
# Apenas os Intermédios-Altos apresentam um valor p acima de alfa, indicando um
# bom ajuste para este grupo
print(mod_lrm) 
# Brier= 0.215  # queremos que seja pequeno
# Idealmente o valor de Brier seria inferior a 0.2, está muito próximo mas
# poderia ser melhorado


# Capacidade Discriminativa
preditos <- predict(modelo_multinom)
head(preditos)  # probabilidade predita
# problemas com predict quando se usa o mlogit
probPred <- predict(modelo_multinom, type="probs")
head(probPred)  # probabilidade predita
grupo_unificadoPred <- predict(modelo_multinom, type="class")
head(grupo_unificadoPred)  # modalidade predita
confusionMatrix(df_analise$grupo_unificado, grupo_unificadoPred)
table(df_analise$grupo_unificado)
# Exatidão do modelo = 0.8 , ou seja, 80% o que é bastaste bom
# Valor Kappa = 0.706 o que indica uma boa concordância
#                         Class: Equilibrados   Class: Intermédios-Altos    Class: Baratos    Class: Intermédios-Baixos   Class: Luxo
#Sensitivity              0.5119                0.8069                      0.8919            0.8255                      0.8627
#Specificity              0.9017                0.9411                      0.9881            0.9132                      0.9863
#Pos Pred Value           0.3233                0.8662                      0.8571            0.8940                      0.7719
#Neg Pred Value           0.9527                0.9116                      0.9913            0.8551                      0.9926
#Prevalence               0.0840                0.3210                      0.0740            0.4700                      0.0510
#Detection Rate           0.0430                0.2590                      0.0660            0.3880                      0.0440
#Detection Prevalence     0.1330                0.2990                      0.0770            0.4340                      0.0570
#Balanced Accuracy        0.7068                0.8740                      0.9400            0.8694                      0.9245
# Análise da Sensibilidade e Especificidade
# Equilibrados  Sensibilidade = 51%
#               Especificidade = 90%
# Acertou apenas metade
# Intermédios-Altos   Sensibilidade = 81%
#                     Especificidade = 94%
# Boa sensibilidade e especificidade
# Baratos   Sensibilidade = 89%
#           Especificidade = 99%
# Excelente desempenho
# Intermédios-Baixo   Sensibilidade = 83%
#                     Especificidade = 91%
# Boa sensibilidade e especificidade
# Luxo    Sensibilidade = 86%
#         Especificidade = 99%
# Excelente desempenho


# Interpretação dos Coeficientes
coefficients(modelo_multinom)
summary(modelo_multinom)
# Estimativas pontuais
#                       (Intercept)   carat         depth         table           cut
# Intermédios-Altos     3.57295       5.761099      0.07614192    -0.31763009     1.5057258
# Baratos               21.74795      -18.372696    1.13500816    -4.27438024     31.0646687
# Intermédios-Baixos    31.87054      -3.972636     -0.39020858   -0.09948684     0.5113972
# Luxo                  -66.38876     13.792704     0.84259828    -0.16845355     1.4192948

# Carat
# Quanto maior o peso (carat), maior probabilidade de ser classificado como 
# Intermédios-Altos (+5.76) e Luxo (+13.79), em comparação com os "Equilibrados"
# e muito menor probabilidade de ser Barato (–18.37) ou Intermédios-Baixos (–3.97).
# O peso é um forte preditor do nível de preço, como seria de esperar. Diamantes 
# mais pesados tendem a ser mais caros.

# Depth
# Coeficiente positivo para Baratos (+1.135), Luxo (+0.8426) e 
# Intermédios-Altos (+0.076), e negativo para Intermédios-Baixos (–0.390).
# Um aumento em depth pode estar associado a preços mais baixos ou mais extremos.

# Table
# Coeficientes negativos em todos os grupos, Baratos (–4.27), 
# Intermédios-Altos (–0.32), Intermédios-Baixos (-0.10) e Luxo (-0.17).
# Diamantes com maior table tendem a ser menos valiosos, diminuindo a 
# probabilidade de categorias caras ou médias em relação aos Equilibrados.

# Cut
# Coeficientes positivos em todos os grupos, Intermédios-Altos (+1.51), 
# Baratos (+31.06), Intermédios-Baixos (+0.51) e Luxo (+1.42).
# Assim, cut tem também um impacto forte na influência do preço.

#Estimativas intervalares
coefs <- summary(modelo_multinom)$coefficients
stde <- summary(modelo_multinom)$standard.errors
lower <- coefs - 1.96 * stde
upper <- coefs + 1.96 * stde
lower
#                       (Intercept)   carat         depth         table         cut
# Intermédios-Altos     -2.756554     4.520574      -0.0388466    -0.4272852    1.2158718
# Baratos               21.428774     -23.418391    0.2794674     -5.1550344    29.4687647
# Intermédios-Baixos    21.530005     -4.874118     -0.5113694    -0.1967795    0.2875343
# Luxo                  -67.154519    11.443731     0.6684224     -0.3783161    0.8771187
upper
#                       (Intercept)   carat         depth         table         cut
# Intermédios-Altos     9.902454      7.001625      0.1911304     -0.20797494   1.7955798
# Baratos               22.067136     -13.327002    1.9905489     -3.39372604   32.6605728
# Intermédios-Baixos    42.211075     -3.071155     -0.2690477    -0.00219413   0.7352602
# Luxo                  -65.622996    16.141677     1.0167742     0.04140901    1.9614710

# Intermédios-Altos
# IC95(Intercepto)  = [-2.756, 9.902]
# IC95(carat)       = [4.521, 7.002]
# IC95(depth)       = [-0.039, 0.191]
# IC95(table)       = [-0.427, -0.208]
# IC95(cut)         = [1.216, 1.796]

# Baratos
# IC95(Intercepto)  = [21.429, 22.067]
# IC95(carat)       = [-23.418, -13.327]
# IC95(depth)       = [0.279, 1.991]
# IC95(table)       = [-5.155, -3.394]
# IC95(cut)         = [29.469, 32.661]

# Intermédios-Baixos
# IC95(Intercepto)  = [21.530, 42.211]
# IC95(carat)       = [-4.874, -3.071]
# IC95(depth)       = [-0.511, -0.269]
# IC95(table)       = [-0.197, -0.002]
# IC95(cut)         = [0.288, 0.735]

# Luxo
# IC95(Intercepto)  = [-67.155, -65.623]
# IC95(carat)       = [11.444, 16.142]
# IC95(depth)       = [0.668, 1.017]
# IC95(table)       = [-0.378, 0.041]
# IC95(cut)         = [0.877, 1.961]



###---------------###
#### EXERCÍCIO 4 ####
###---------------###
# Comparação entre Análise Discriminante e Regressão Multinomial

# A Análise Discriminante apresentou uma taxa de acerto de 74.6% na amostra de 
# treino e 75.8% na amostra de teste, ligeiramente inferior aos 80% da Regressão 
# Multinomial.
# No entanto, ao analisar a capacidade preditiva em detalhe, a Análise 
# Discriminante teve elevada sensibilidade, mas baixa especificidade. 
# Por outro lado, a Regressão Multinomial demonstrou elevada
# sensibilidade e especificidade para quase todos os grupos, 
# sendo especialmente precisa para os grupos "Baratos" e "Luxo", com 
# especificidades superiores a 98%.

# Em relação aos pressupostos, a Análise Discriminante violou de forma 
# significativa a normalidade multivariada com assimetrias e achatamentos 
# visíveis nos testes de Mardia e nos QQ-plots, além de existirem problemas de 
# homogeneidade de variâncias, especialmente no grupo "Luxo" que apresentava 
# zero variabilidade em algumas variáveis. Apesar da robustez da Análise 
# Discriminante frente a essas violações, esses fatores comprometem a 
# fiabilidade do modelo.

# A Regressão Multinomial respeitou melhor os pressupostos. O VIF mostrou 
# ausência de multicolinearidade entre as variáveis, e ainda que o teste de 
# linearidade com polinómios fracionários indicasse a necessidade de 
# transformações, o modelo original continuou a apresentar melhor desempenho em 
# termos de AIC e R². O modelo final da RM explicou bem a variabilidade dos 
# dados, com um R² de McFadden em torno de 0.60 e de Nagelkerke em 0.39, e um 
# valor Kappa de 0.706, evidenciando boa concordância entre classificações 
# observadas e previstas.

# A Regressão Multinomial também se destacou pela capacidade interpretativa, 
# onde os coeficientes obtidos permitiram analisar diretamente a influência de 
# cada variável em cada grupo. Por exemplo, "carat" foi um forte preditor 
# para o grupo "Luxo" (coef. +13.79) e para o grupo "Baratos" (coef. –18.37), 
# enquanto "cut" teve impacto positivo em todos os grupos.

# Embora a Análise Discriminante tenha sido competente em termos de exatidão, os 
# problemas com pressupostos e a baixa especificidade em novos dados indicam que 
# a Regressão Multinomial é mais adequada neste contexto. A Regressão 
# Multinomial demonstrou melhor ajuste global (menor AIC, melhor R-squared), 
# maior robustez estatística e maior capacidade discriminativa, sendo assim a 
# escolha mais recomendada para explicar e prever os grupos formados com base 
#nas variáveis carat, depth, table e cut.


###---------------###
#### EXERCÍCIO 5 ####
###---------------###
summary(df_analise)
individuo <- data.frame(carat = 0.75, depth = 61, table = 58, cut = 3) # cut=3 corresponde a Very Good

# Análise Discriminante
predict(fit, newdata = individuo)
#       11_10_1     14_3_9_15     2             5_8_4_6_12    7_13
# 1     0.2148581   0.08247297    0.01305493    0.689613      9.82074e-07
# Com base na função discriminante estimada, o indivíduo foi classificado no 
# grupo "5_8_4_6_12", que corresponde ao grupo unificado "Intermédios-Baixos", 
# com uma probabilidade predita de 68.96%. A segunda maior probabilidade (21.5%) 
# foi atribuída ao grupo "11_10_1" (Equilibrados), enquanto as restantes 
# categorias apresentaram probabilidades muito reduzidas.
# Isto indica uma elevada confiança da Análise Discriminante nesta classificação.

# Regressão Multinomial
predict(modelo_multinom, newdata = individuo, type = "probs")
# Equilibrados    Intermédios-Altos     Baratos         Intermédios-Baixos    Luxo 
# 2.777819e-01    7.080292e-02          6.010100e-35    6.514044e-01          1.076646e-05 
predict(modelo_multinom, newdata = individuo, type = "class")
# Com base no modelo de regressão multinomial ajustado, o mesmo indivíduo foi 
# classificado também no grupo "Intermédios-Baixos", com uma probabilidade de 
# 65.14%. O grupo "Equilibrados" foi o segundo mais provável, com 27.78%, 
# enquanto os restantes apresentaram probabilidades residuais ou praticamente 
# nulas.
# Assim, o modelo multinomial confirma a previsão da Análise Discriminante, 
# atribuindo este indivíduo a "Intermédios-Baixos".

# Conclui-se, portanto, que ambos os modelos convergem na atribuição do 
# indivíduo ao grupo "Intermédios-Baixos", com elevada confiança. Esta 
# concordância reforça a fiabilidade da classificação e está de acordo com a 
# caracterização prévia do grupo, onde se encontram diamantes com carat 
# intermédio, depth variável, table mediano e cut heterogéneo, tal como o perfil 
# do indivíduo em questão.