# ******************************************************************************
#                     Regressao e Classificacao 2024/25
#
# Exercicios: Análise classificatória
#
# ******************************************************************************


# ----------------------------- Ex. 4.4 ----------------------------------------

# conjunto de dados
(dados <- data.frame(x1 = c(7, 6, 3, 5, 4),
                     x2 = c(6, 8, 2, 6, 1)))

# alterar nome das linhas
rownames(dados) <- c("A", "B", "C", "D", "E")

# a) -----
# Quando queremos definir os grupos iniciais: criar matriz com os centros as coordenadas dos grupos
Medias <- aggregate(dados, 
                    list(c(1,1,2,2,2)), # grupo inicial a que foi associada cada linha
                    mean)               # medias por grupo  

agrupaKmeans <- kmeans(dados, 
                       centers=Medias[,-1], # centro dos grupos: pode ser data.frame ou matriz
                       nstart = 1)
# ou em alternativa
# converter centros dos grupos num objeto matriz
centros <- as.matrix(Medias[,-1],  ncol=2, byrow=T)
agrupaKmeans <- kmeans(dados, 
                       centers=centros,
                       nstart = 1)

# ver agrupamento final
agrupaKmeans$cluster  # agrupamento final

plot(dados, 
     col  = agrupaKmeans$cluster)  # a cor dos pontos varia com o grupo a que foi associado  
# adicionar o centro dos grupos
points(agrupaKmeans$centers, 
       col = c("green","blue"),   # cor
       pch = 7)                   # tipo de simbolo

# alternativa para visualizar os agrupamentos
library(factoextra)
fviz_cluster(agrupaKmeans, data = dados,
             ggtheme = theme_bw())


# b) -----
Medias <- aggregate(dados, 
                    list(c(1,2,2,2,1)), # grupo inicial a que foi associada cada linha
                    mean)               # medias por grupo  

agrupaKmeans <- kmeans(dados, 
                       centers=Medias[,-1], # centro dos grupos
                       nstart = 1)
agrupaKmeans$cluster  # agrupamento final


plot(dados, col  = agrupaKmeans$cluster)
# adicionar o centro dos grupos
points(agrupaKmeans$centers, 
       col = c("green","blue"),   # cor
       pch = 7)                   # tipo de simbolo

# ver agrupamentos
library(factoextra)
fviz_cluster(agrupaKmeans, data = dados,
             ggtheme = theme_bw())


# c) -----
agrupaKmeans <- kmeans(dados, 
                       centers=2,   # numero de grupos
                       nstart = 25)
agrupaKmeans$cluster  # agrupamento final

plot(dados, col  = agrupaKmeans$cluster)
# adicionar o centro dos grupos
points(agrupaKmeans$centers, 
       col = c("green","blue"),   # cor
       pch = 7)                   # tipo de simbolo

# ver agrupamentos
library(factoextra)
fviz_cluster(agrupaKmeans, data = dados,
             ggtheme = theme_bw())


# d) -----
# obter valor de R^2
# numerador da var de cada variavel
SQ <- function(x){ var(x)*(length(x)-1) }

(SQT <- SQ(dados$x1)+SQ(dados$x2))

g1 <- dados[c("A", "B", "D"),]
g2 <- dados[c("C", "E"),]
(SQDg1 <- SQ(g1$x1)+SQ(g1$x2))
(SQDg2 <- SQ(g2$x1)+SQ(g2$x2))

(R2 <- 1-(SQDg1+SQDg2)/SQT)
# Nota: Ha outras formas mais eficientes de obter o R2. 
# Com esta salienta-se o que esta a ser calculado