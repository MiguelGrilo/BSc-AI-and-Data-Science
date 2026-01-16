# ******************************************************************************
#                     Regressao e Classificacao 2024/25
#
# Exercicios: Análise classificatória
#
# ******************************************************************************


# ----------------------------- Ex. 4.2 ----------------------------------------

# optativo: definir a diretoria atual como diretoria de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ler dados
library(readxl)
dados <- read_excel("Wescheler.xlsx") # le em formato tibble
head(dados)

# converter tibble em data.frame
dados <- data.frame(dados)
# alterar nome das linhas para o ID do individuo
rownames(dados) <- dados$Indivíduo

# remover coluna individuo da analise
dados <- dados[, -1]
# igual a 
#dados <- subset(dados, select=-Indivíduo)


# a) -----

# matriz de distancias euclidiana
(matriz.distancias <- dist(dados, method = "euclidean"))


# 1) metodo da ligacao SIMPLES
simples <- hclust(matriz.distancias, method = "single")
simples$height
# dendograma
plot(simples, 
     main = "Método da ligação SIMPLES", 
     sub = "Dendograma", xlab = "", ylab="Distância", hang=-1)
# Pela analise das distancias considerar 5 grupos ou 3 (muito forçado)...
# scree plot que pode ser util para decidir o numero de grupos: num. de grupos dado pelo ponto de inflexao
nH <- length(simples$height)
plot(c(simples$height[nH:1],0), type="b")
# 3 ou 7 grupos

# solucao com 3 grupos
plot(simples, 
     main = "Método da ligação SIMPLES", 
     sub = "Dendograma", xlab = "", ylab="Distância")
rect.hclust(simples,3)
(membros <- cutree(simples, 3))
table(membros)  # numero de membros por grupo
# G1: 3 elementos - I1 + I6 + I8
# G2: 5 elementos - I2 + I3 + I5 + I7 + I10
# G3: 2 elementos - I4 + I9

# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
# G1: < media em todas -> individuos em risco
# G2: > media a IVP
# G4: > media a ICV, IOP e IMT
# todas as variaveis diferenciam bem os grupos

library(cluster)
plot(silhouette(cutree(simples,3), matriz.distancias))
# observacoes com barra proximo de 1 estao bem agrupadas
# observacoes com barra proximo de 0 estao entre 2 grupos
# observacoes com barra negativa, ou estao no grupo errado ou sao outliers e podem ter que ser removidas
# conclusao: nada a observar

# solucao com 5 grupos
plot(simples, 
     main = "Método da ligação SIMPLES", 
     sub = "Dendograma", xlab = "", ylab="Distância")
rect.hclust(simples,5)
(membros <- cutree(simples, 5))
table(membros)  # numero de membros por grupo
# G1: 2 elementos - I1 + I8
# G2: 1 elemento  - I2
# G3: 4 elementos - I3 + I5 + I7 + I10
# G4: 2 elementos - I4 + I9
# G5: 1 elemento  - I6

# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
# G1: < media a ICV, IOP e IMT (tudo mt baixo) -> individuos em risco
# G2: > media a ICV e IOP e medias intermedias a IMT e IVP
# G3: medias intermedias a IMT, IOP e IMT e elevada a IVP 
# G4: > media a IMT, e medias elevadas a ICV e IOP
# G5: < media a IVP e media baixa a ICV e IOP -> possivel individuo em risco
# A escala ICV é a que menos diferencia os 5 grupos

plot(silhouette(cutree(simples,5), matriz.distancias))
# conclusao: nada a observar


# 2) metodo da ligacao COMPLETA
completa <- hclust(matriz.distancias, method = "complete")
completa$height
# dendograma
plot(completa, 
     main = "Método da ligação COMPLETA", 
     sub = "Dendograma", xlab = "", ylab="Distância")
# Pela analise das distancias considerar 4 grupos
# scree plot
nH <- length(completa$height)
plot(c(completa$height[nH:1],0), type="b")
# 2 ou 4 grupos

# solucao com 4 grupos
plot(completa, 
     main = "Método da ligação COMPLETA", 
     sub = "Dendograma", xlab = "", ylab="Distância")
rect.hclust(completa,4)
(membros <- cutree(completa, 4))
table(membros)  # numero de membros por grupo
# G1: 3 elementos - I1 + I6 + I8
# G2: 1 elemento  - I2
# G3: 4 elementos - I3 + I5 + I7 + I10
# G4: 2 elementos - I4 + I9

# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
# G1: < media a ICV, IOP e IMT (tudo mt baixo) -> individuos em risco
# G2: > media a ICV e IOP
# G3: medias intermedias a IMT eIOP e elevada a IVP 
# G4: > media a IMT e media elevada a ICV e IOP
# Em suma: IOP parece ser a escala que mais diferencia os 4 grupos.

plot(silhouette(cutree(completa,4), matriz.distancias))
# nada a observar


# 3) metodo da ligacao MEDIA
media <- hclust(matriz.distancias, method = "average")
media$height
# dendograma
plot(media, 
     main = "Método da ligação MÉDIA", 
     sub = "Dendograma", xlab = "", ylab="Distância")
# Pela analise das distancias considerar 4 grupos
# scree plot
nH <- length(media$height)
plot(c(media$height[nH:1],0), type="b")
# 4 grupos

# solucao com 4 grupos
plot(media, 
     main = "Método da ligação MÉDIA", 
     sub = "Dendograma", xlab = "", ylab="Distância")
rect.hclust(media,4)
(membros <- cutree(media, 4))
table(membros)  # numero de membros por grupo
# G1: 3 elementos - I1 + I6 + I8
# G2: 1 elemento  - I2
# G3: 4 elementos - I3 + I5 + I7 + I10
# G4: 2 elementos - I4 + I9

# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
# G1: < media a todas as vars -> individuos em risco
# G2: > media a ICV e IOP
# G3: > media IVP e medias intermedias às restantes
# G4: > media IMT, medias elevadas a ICV e IOP

plot(silhouette(cutree(media,4), matriz.distancias))
# nada a observar


# 4) metodo de WARD
ward <- hclust(matriz.distancias, method = "ward.D2")
ward$height
# dendograma
plot(ward, 
     main = "Método de WARD", 
     sub = "Dendograma", xlab = "", ylab="Distância")
# Pela analise das distancias considerar 3 ou 4 grupos
# scree plot
nH <- length(ward$height)
plot(c(ward$height[nH:1],0), type="b")
# 4 grupos

# solucao com 3 grupos
plot(media, 
     main = "Método da ligação MÉDIA", 
     sub = "Dendograma", xlab = "", ylab="Distância")
rect.hclust(ward,3)
(membros <- cutree(ward, 3))
table(membros)  # numero de membros por grupo
# G1: 3 elementos - I1 + I6 + I8
# G2: 3 elementos - I2 + I4 + I9
# G3: 4 elementos - I3 + I5 + I7 + I10

# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
# G1: < media a todas as vars -> individuos em risco
# G2: > media a ICV, IOP e IMT
# G3: > media IVP e medias intermedias às restantes

plot(silhouette(cutree(ward,3), matriz.distancias))
# individuo 2 nao parece fazer parte do grupo -> separar do grupo, i.e., formar 4 grupos
plot(silhouette(cutree(ward,4), matriz.distancias))
# sem problemas
plot(silhouette(cutree(ward,2), matriz.distancias)) 
# sem problemas
# optar por solucao com 2 ou 4 grupos

# solucao com 4 grupos
(membros <- cutree(ward, 4))
table(membros)  # numero de membros por grupo
# G1: 3 elementos - I1 + I6 + I8
# G2: 1 elemento  - I2
# G3: 4 elementos - I3 + I5 + I7 + I10
# G4: 2 elementos - I4 + I9

# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
# G1: < media a todas as vars -> individuos em risco
# G2: > media a ICV e IOP
# G3: > media IVP e medias intermedias às restantes
# G4: > media IMT, medias elevadas a ICV e IOP

# conclusao: ligacao simples apresenta solucao com mais grupos (6) ou menos (3), 
#            ao passo que os restantes metodos apresentam uma solucao com 4 grupos
#            e com a mesma composicao (i.e., ha concordancia entre os resultados).




# b) ----

# -- matriz de distancias de Manhattan
(matriz.distancias <- dist(dados, method = "manhattan"))

# 1) metodo da ligacao SIMPLES
simples <- hclust(matriz.distancias, method = "single")
simples$height
# dendograma
plot(simples, 
     main = "Método da ligação SIMPLES", 
     sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
# Pela analise das distancias considerar 7 grupos!!! 
# scree plot
nH <- length(simples$height)
plot(c(simples$height[nH:1],0), type="b")
# 2 ou 7 grupos

# solucao 2 grupos
plot(simples, 
     main = "Método da ligação SIMPLES", 
     sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
rect.hclust(simples,2)
(membros <- cutree(simples, 2))
# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
# IOP e a variavel que mais diferencia os grupos
plot(silhouette(cutree(simples,2), matriz.distancias))
# conclusao: nada a observar

# solucao 7 grupos
plot(simples, 
     main = "Método da ligação SIMPLES", 
     sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
rect.hclust(simples,7)
(membros <- cutree(simples, 7))
# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
# IOP e a variavel que mais diferencia os grupos
plot(silhouette(cutree(simples,7), matriz.distancias))
# conclusao: nada a observar


# 2) metodo da ligacao COMPLETA
completa <- hclust(matriz.distancias, method = "complete")
completa$height
# dendograma
plot(completa, 
     main = "Método da ligação COMPLETA", 
     sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
# Pela analise das distancias considerar 4 grupos
# scree plot
nH <- length(completa$height)
plot(c(completa$height[nH:1],0), type="b")
# 4 grupos

# solucao 4 grupos
plot(completa, 
     main = "Método da ligação COMPLETA", 
     sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
rect.hclust(completa,4)
(membros <- cutree(completa, 4))
# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
plot(silhouette(cutree(completa,4), matriz.distancias))
# nada a observar


# 3) metodo da ligacao MEDIA
media <- hclust(matriz.distancias, method = "average")
media$height
# dendograma
plot(media, 
     main = "Método da ligação MÉDIA", 
     sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
# Pela analise das distancias considerar 3 ou 4 grupos
# scree plot
nH <- length(media$height)
plot(c(media$height[nH:1],0), type="b")
# ... talvez 3 ou 4 grupos

# solucao 4 grupos
plot(media, 
     main = "Método da ligação MÉDIA", 
     sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
rect.hclust(media,4)
(membros <- cutree(media, 4))
# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
plot(silhouette(cutree(media,4), matriz.distancias))
# nada a observar


# 4) metodo de WARD
ward <- hclust(matriz.distancias, method = "ward.D2")
ward$height
# dendograma
plot(ward, 
     main = "Método de WARD", 
     sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
# Pela analise das distancias considerar 3 grupos
# scree plot
nH <- length(ward$height)
plot(c(ward$height[nH:1],0), type="b")
# 3 grupos

# solucao 3 grupos
plot(ward, 
     main = "Método de WARD", 
     sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
rect.hclust(ward,3)
(membros <- cutree(ward, 3))
# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
plot(silhouette(cutree(ward,3), matriz.distancias))
# individuo 2 nao parece fazer parte do grupo -> separar do grupo, i.e., formar 4 grupos
plot(silhouette(cutree(ward,2), matriz.distancias))
# ok
plot(silhouette(cutree(ward,4), matriz.distancias))
# ok
# a solucao com 2 grupos parece ser melhor...

(membros <- cutree(ward, 2))
aggregate(dados,list(membros),mean)
# G1: medias mais baixas em todas as variaveis
# G2: medias mais elevadas em todas as variaveis

(membros <- cutree(ward, 4))
aggregate(dados,list(membros),mean)
# G1: medias mais baixas em todas as variaveis
# G2: medias mais elevadas a ICV e IOP
# G3: media mais elevada a IVP
# G4: media mais elevada a IMT


# -- matriz de distancias de Minkowski 
(matriz.distancias <- dist(dados, method = "minkowski", p=3))

# 1) metodo da ligacao SIMPLES
simples <- hclust(matriz.distancias, method = "single")
simples$height
# dendograma
plot(simples, 
     main = "Método da ligação SIMPLES", 
     sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
# Pela analise das distancias considerar 3 ou 7 grupos!!! 
# scree plot
nH <- length(simples$height)
plot(c(simples$height[nH:1],0), type="b")
# 3 ou 7 grupos

# solucao 3 grupos
plot(simples, 
     main = "Método da ligação SIMPLES", 
     sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
rect.hclust(simples,3)
(membros <- cutree(simples, 3))
# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
# IOP e a variavel que mais diferencia os grupos
plot(silhouette(cutree(simples,3), matriz.distancias))
# conclusao: nada a observar


# 2) metodo da ligacao COMPLETA
completa <- hclust(matriz.distancias, method = "complete")
completa$height
# dendograma
plot(completa, 
     main = "Método da ligação COMPLETA", 
     sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
# Pela analise das distancias considerar 2 ou 4 grupos
# scree plot
nH <- length(completa$height)
plot(c(completa$height[nH:1],0), type="b")
# 2 ou 4 grupos

# solucao 2 grupos
plot(completa, 
     main = "Método da ligação COMPLETA", 
     sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
rect.hclust(completa,2)
(membros <- cutree(completa, 2))
# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
plot(silhouette(cutree(completa,2), matriz.distancias))
# nada a observar


# 3) metodo da ligacao MEDIA
media <- hclust(matriz.distancias, method = "average")
media$height
# dendograma
plot(media, 
     main = "Método da ligação MÉDIA", 
     sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
# Pela analise das distancias considerar 3 ou 4 grupos
# scree plot
nH <- length(media$height)
plot(c(media$height[nH:1],0), type="b")
# 4 grupos

# solucao 4 grupos
plot(media, 
     main = "Método da ligação MÉDIA", 
     sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
rect.hclust(media,4)
(membros <- cutree(media, 4))
# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
plot(silhouette(cutree(media,4), matriz.distancias))
# nada a observar


# 4) metodo de WARD
ward <- hclust(matriz.distancias, method = "ward.D2")
ward$height
# dendograma
plot(ward, 
     main = "Método de WARD", 
     sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
# Pela analise das distancias considerar 4 grupos
# scree plot
nH <- length(ward$height)
plot(c(ward$height[nH:1],0), type="b")
# 4 grupos

# solucao 4 grupos
plot(ward, 
     main = "Método de WARD", 
     sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
rect.hclust(ward,4)
(membros <- cutree(ward, 4))
# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
plot(silhouette(cutree(ward,4), matriz.distancias))
# ok




## PACOTES ALTERNATIVOS PARA ANALISE CLASSIFICATORIA HIERARQUICA -----
# exemplo com metodo simples e definindo 3 grupos
library(factoextra)
# Se soubermos quantos grupos queremos
agrupamento <- hcut(dados, 
                    k = 3,                   # numero de grupos
                    hc_method = "single",    # metodo de ligacao
                    hc_metric = "euclidean", # distancia a usar 
                    stand = F,               # se usa dados originais (=F) ou estandardizados (=T)
                    graph=F)                 # se mostra ou nao o dendograma
# grupo a que pertence cada observacao
agrupamento$cluster
# tamanho dos grupos
agrupamento$size
# dendograma
fviz_dend(agrupamento, 
          rect = TRUE) # se representa um rectangulo a delimitar os grupos
# grafico silhouette
fviz_silhouette(agrupamento)
# se quisermos ver a solucao em 2D com base nas componentes principais
fviz_cluster(agrupamento)


# outra alternativa
library(FactoMineR)
agrupamento <- HCPC(dados, 
                    nb.clust=3,           # numero de grupos
                    metric="euclidean",   # distancia a usar
                    method="single",      # metodo de ligacao
                    graph=T)              # mostra grafico de dispersao com grupos
fviz_dend(agrupamento, 
          rect = TRUE) # se representa um rectangulo a delimitar os grupos
fviz_cluster(agrupamento)

# sem definir o numero de grupos
agrupamento <- HCPC(dados, nb.clust=-1, metric="euclidean", method="single", graph=T)
fviz_dend(agrupamento, 
          rect = TRUE) # se representa um rectangulo a delimitar os grupos
fviz_cluster(agrupamento)

