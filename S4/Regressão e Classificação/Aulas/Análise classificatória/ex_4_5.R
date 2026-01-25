# ******************************************************************************
#                     Regressao e Classificacao 2024/25
#
# Exercicios: Análise classificatória
#
# ******************************************************************************


# ----------------------------- Ex. 4.5 ----------------------------------------
# ----------------------------- Ex. 4.5 ----------------------------------------

# optativo: definir a diretoria atual como diretoria de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## a
# ler dados
library(readxl)
dados <- read_excel("Wescheler.xlsx") # le em formato tibble
head(dados)

# converter tibble em data.frame
dados <- data.frame(dados)
# alterar nome das linhas para o ID do individuo
rownames(dados) <- dados$Indivíduo

# remover coluna individuo da analise
dados <- subset(dados, select=-Indivíduo)


## a) -----
agrupaKmeans <- kmeans(dados, 
                       4, # grupos
                       nstart = 25)
agrupaKmeans
# R2 = 84.9%

# ver agrupamentos
(gKmeans <- fviz_cluster(agrupaKmeans, 
                         data = dados,
                         main="K-means, R2=84.9%",
                         ggtheme = theme_bw()))



## b) -----
# Funcao para obter R2 de uma solucao de agrupamento hierarquico
R2 <- function(dados, membros){  # input:
  # dados=dados usados no agrupamento, 
  # membros=indica o grupo a que pertence cada observacao
  # centrar variaveis
  dadosCentrados <- scale(dados, scale=F)
  # obter SQTotal = soma dos numeradores das variancias de cada variavel
  SQTotal <- sum(dadosCentrados^2)
  
  # obter as SQDentro dos grupos
  SQDentro <- sum(         # somar a SQ de todos os grupos
    # para cada grupo
    sapply(unique(membros),
           # obter o SQ dentro de cada grupo, i.e., o numerador da variancia de cada variavel
           function(k) {sum(scale(dados[membros == k, ], scale=F)^2)}  
    ))
  R2 <- 1 - (SQDentro / SQTotal)  # valor de R2
  return(list=c(SQTotal=SQTotal, SQDentro=SQDentro, R2_Perc=R2*100))
}


# matriz de distancias euclidiana
(matriz.distancias <- dist(dados, method = "euclidean"))

# 1) metodo da ligacao SIMPLES
agrupamento <- hclust(matriz.distancias, method = "single")
membros <- cutree(agrupamento, 5)           # solucao com 5 grupos
R2(dados, membros)   # R2=90.1%
(gSimples <- fviz_cluster(list(cluster=membros, data = dados),
                          main="SIMPLES, R2=90.1%", ggtheme = theme_bw()))
# ou em alternativa
#agrupamento <- hcut(dados, k = 5, hc_metric = "euclidean", 
#                    hc_method = "single",
#                    stand = F, graph=F)
#R2(dados, agrupamento$cluster)  # R2=90.1%
#(gSimples <- fviz_cluster(agrupamento, 
#                          main="SIMPLES, R2=90.1%", ggtheme = theme_bw()))


# 2) metodo da ligacao COMPLETA
agrupamento <- hclust(matriz.distancias, method = "complete")
membros <- cutree(agrupamento, 4) # solucao com 4 grupos
R2(dados, membros)  # R2=84.9%
(gCompleta <- fviz_cluster(list(cluster=membros, data = dados),
                           main="COMPLETA, R2=84.9%",
                           ggtheme = theme_bw()))

# 3) metodo da ligacao MEDIA
agrupamento <- hclust(matriz.distancias, method = "average")
membros <- cutree(agrupamento, 4) # solucao com 4 grupos
R2(dados, membros)  # R2=84.9%
(gMedia <- fviz_cluster(list(cluster=membros, data = dados),
                        main="MEDIA, R2=84.9%",
                        ggtheme = theme_bw()))

# 4) metodo da ligacao WARD
agrupamento <- hclust(matriz.distancias, method = "ward.D2")
membros <- cutree(agrupamento, 4) # solucao com 4 grupos
R2(dados, membros)  # R2=84.9%
(gWard <- fviz_cluster(list(cluster=membros, data = dados),
                       main="WARD, R2=84.9%",
                       ggtheme = theme_bw()))

# comparar as solucoes
library(gridExtra)
grid.arrange(gKmeans, gSimples, gCompleta, gMedia, gWard, ncol=3)



