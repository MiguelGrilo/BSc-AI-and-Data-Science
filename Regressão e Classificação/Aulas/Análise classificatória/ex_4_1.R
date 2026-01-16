# ******************************************************************************
#                     Regressao e Classificacao 2024/25
#
# Exercicios: Análise classificatória
#
# ******************************************************************************


# ----------------------------- Ex. 4.1 ----------------------------------------

# conjunto de dados
(dados <- data.frame(x1 = c(7, 6, 3, 5, 4),
                     x2 = c(6, 8, 2, 6, 1),
                     x3 = c(3, 4, 4, 2, 2)))

# alterar nome das linhas
rownames(dados) <- c("A", "B", "C", "D", "E")


# a) -----
# construir matriz de distancias (com base da distancia euclideana)
(matriz.distancias <- dist(dados, method = "euclidean"))


# b) i) -----
# usar metodo hierarquico da ligacao SIMPLES
simples <- hclust(matriz.distancias, method = "single")
# Opcional: Visualizar fusões
simples$height  # distancia a que foram realizadas as unioes

# funcao alternativa a hclust
simples <- agnes(matriz.distancias, method = "single")
simples$height  # distancia a que foram realizadas as unioes
# NOTA: ignorar o ultimo valor!!!

# se quiserem ver detalhe com distancias ao longo das etapas de aglomeracao
agnes(matriz.distancias, method = "single", trace.lev=3)



# b) ii) -----
# usar metodo hierarquico da ligacao COMPLETA
completa <- hclust(matriz.distancias, method = "complete")
# Opcional: Visualizar fusões
completa$height  # distancia a que foram realizadas as unioes



# b) iii) -----
# usar metodo hierarquico da ligacao MEDIA
media <- hclust(matriz.distancias, method = "average")
# Opcional: Visualizar fusões
media$height  # distancia a que foram realizadas as unioes



# c) -----
# ligacao SIMPLES
plot(simples)
# personalizar titulos
plot(simples, 
     main = "Método da ligação simples", 
     sub = "Dendograma", xlab = "", ylab="Distância")
# Pela analise das distancias considerar 2 grupos
# cutree: permite cortar a arvore tendo em conta o numero de grupos definidos
membros <- cutree(simples, 
                  2) # numero de grupos pretendidos
membros  # a que grupos pertencem
table(membros)  # numero de membros por grupo
# assinalar os grupos formados no dendograma (nota: o dendograma foi previamente construido)
rect.hclust(simples,2)  


# ligacao COMPLETA
plot(completa, 
     main = "Método da ligação completa", 
     sub = "Dendograma", xlab = "", ylab="Distância")
# Pela analise das distancias considerar 2 grupos
rect.hclust(completa,2)  


# ligacao MEDIA
plot(media, 
     main = "Método da ligação média", 
     sub = "Dendograma", xlab = "", ylab="Distância")
# Pela analise das distancias considerar 2 grupos
rect.hclust(media,2)  

# conclusao: qualquer que seja o metodo utilizado, os resultados sao coerentes:
#            considerar 2 grupos: (A, D, B) e (C, E)


## d) ver a)-c)


# e) -----
# usar metodo hierarquico do CENTROIDE
centroide <- hclust(matriz.distancias, method = "centroid")
# Opcional: Visualizar fusões
centroide$height  # distancia a que foram realizadas as unioes
# Nota: Estas distancias nao correspondem as obtidas manualmente 
#       porque a funcao hclust considera que a matriz de entrada 
#       = quadrado da distancia euclideana (i.e., sem raiz quadrada).
# Para obter os mesmos resultados dos que resultam de fazer os calculos manualmente:
centroide2 = hclust(matriz.distancias^2, method='centroid')
sqrt(centroide2$height)  # distancia a que foram realizadas as unioes

# dendograma
plot(centroide2, 
     main = "Método do centróide", 
     sub = "Dendograma", xlab = "", ylab="Distância")
# Pela analise das distancias considerar 2 grupos
rect.hclust(centroide2,2)


# usar metodo hierarquico de WARD
ward <- hclust(matriz.distancias, method = "ward.D2")
# Opcional: Visualizar fusões
ward$height  # distancia a que foram realizadas as unioes
# equivalente a (com detalhe sobre as distancias atualizadas em cada etapa)
agnes(matriz.distancias, method = "ward", trace.lev=3)

# dendograma
plot(ward, 
     main = "Método de WARD", 
     sub = "Dendograma", xlab = "", ylab="Distância")
# Pela analise das distancias considerar 2 grupos
rect.hclust(ward,2)