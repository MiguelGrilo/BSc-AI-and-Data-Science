# ******************************************************************************
#                     Regressao e Classificacao 2024/25
#
# Exercicios: Análise classificatória
#
# ******************************************************************************


# ----------------------------- Ex. 4.3 ----------------------------------------

# optativo: definir a diretoria atual como diretoria de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ler dados 
dados <- read.csv2("profissoes.csv")
head(dados)
# considerar profissao como nome das linhas
rownames(dados) <- dados$profissão

# remover colunas 1 e 2 do data.frame
dados <- dados[,-c(1:2)]
head(dados)
# equivalente a 
#dados <- subset(dados, select=-c(ID, profissão))

# a) 
# dados --> variaveis originais
# dadosE --> variaveis estandardizadas
dadosE <- within(dados,{
  prestigioZ <- scale(prestigio)
  suicídioZ <- scale(suicídio)
  rendimentoZ <- scale(rendimento)
  educaçãoZ <- scale(educação)
}
)
# reter apenas dados das variaveis estandardizadas
dadosE <- dadosE[,5:8]

# d(canalizadores,cozinheiros)
# usando dados originais
(distancia <- dist(dados[c("Canalizadores","Cozinheiros"),], , method = "euclidean"))
# usando dados estandardizados
(distanciaE <- dist(dadosE[c("Canalizadores","Cozinheiros"),], , method = "euclidean"))
# conclusao: os valores sai muito diferentes


# b) -----
(matriz.distancias <- dist(dados, method = "euclidean"))

agrupamento <- hclust(matriz.distancias, method = "complete")

# b) i) -----
plot(agrupamento,  horiz=T)
# alternativa: por vezes e util apresentar o dendograma na horizontal
par(mar=c(4,1,2,6))
plot(as.dendrogram(agrupamento),  horiz=T,
     xlab="distancia euclidiana",
     main="Método da MAIOR distancia")

# b) ii) -----
# pelo dendograma: 4 ou 8 grupos

# scree plot
par(mar=c(4,4,2,2))
nH <- length(agrupamento$height)
plot(c(agrupamento$height[nH:1],0), type="b", xaxt="n", horiz=T)
axis(1, at = seq(0, 30, by = 2))
# 4 ou 8 grupos

# solucao com 4 grupos
plot(agrupamento, 
     main = "Método da MAIOR distancia", 
     sub = "Dendograma", xlab = "", ylab="Distância euclidiana")
rect.hclust(agrupamento,4)
# ou
library(factoextra)
fviz_dend(agrupamento, k = 4)+
  labs(y="Distância euclidiana", title="Método da MAIOR distância")


# b) iii) -----
(membros <- cutree(agrupamento, 4))
table(membros)
# G1 (n=4):  Advogados, Autores, Engenheiros, Prof.Univers. 
# G2 (n=3):  Arquitectos, Padres, Químicos
# G3 (n=12): Barbeiros, Camionistas, Canalizadores, Carpinteiros, Criadas, Guardas, 
#            Mecânicos, Pintores, Polícias, Porteiro, Receptionistas, Vendedores
# G4 (n=10): Comerciantes, Contabilistas, Cozinheiros, Dentistas, Electricistas, 
#            Maquinistas, Marceneiros, Prof.Liceu, Seguros, Trab.Sociais 

# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dados,list(membros),mean)
# G1: 2a maior media a todas as variaveis
# G2: > media a todas as variaveis
# G3: < media em prestigio, rendimento, educacao 
# G4: < media em suicidio, e 2a menor media nas restantes variaveis
# rendimento e prestigio sao as variaveis que mais diferenciam os grupos
# G2: profissoes com elevado rendimento e prestigio
# G3: profissoes com baixo rendimento e prestigio
# ou graficamente
library(tidyr)
library(dplyr)
dataG <- aggregate(dados,list(membros),mean) %>%
  pivot_longer(cols = -Group.1,  # mantem a coluna Group.1 como fixa
               names_to = "Variavel", 
               values_to = "Media")
ggplot(dataG, aes(x = Variavel, y = Media, group = factor(Group.1), color = factor(Group.1))) +
  geom_line() +          
  geom_point(size = 3) + 
  labs(x = "", y = "Média", title = "Média das variáveis por grupo", color="Grupo")+
  theme_minimal()
# Em falta: que nome sugerem para cada grupo, com base nas suas características?
# (ou seja, no final devem nomear os grupos formados)

library(cluster)
plot(silhouette(cutree(agrupamento,4), matriz.distancias))
# individuos 2 (arquitectos) e 12 (dentistas) parecem nao fazer parte dos grupos -> separar do grupo, i.e., formar 4 grupos
plot(silhouette(cutree(agrupamento,3), matriz.distancias))
# problemas com 1 e 28
plot(silhouette(cutree(agrupamento,5), matriz.distancias))
# problemas com 2, 12 e 28
plot(silhouette(cutree(agrupamento, 6), matriz.distancias))
# problemas com 1, 12 e 28
# conclusao: existem sempre casos que parecem nao fazer parte -> outliers? experimentar remover e refazer agrupamentos
# (...)


# c) ----
(matriz.distanciasE <- dist(dadosE, method = "euclidean"))

agrupamentoE <- hclust(matriz.distanciasE, method = "complete")

# c) i) -----
plot(agrupamentoE, hang = - 1)

# c) ii) -----
# pelo dendograma: 5 grupos

# scree plot
nH <- length(agrupamentoE$height)
plot(c(agrupamentoE$height[nH:1],0), type="b", xaxt="n")
axis(1, at = seq(0, 30, by = 2))
# 5 grupos

# solucao com 5 grupos
plot(agrupamentoE, 
     main = "Método da MAIOR distancia (variaveis estandardizadas)", 
     sub = "Dendograma", xlab = "", ylab="Distância euclidiana")
rect.hclust(agrupamentoE, 5)
# ou
fviz_dend(agrupamentoE, k = 5)+
  labs(y="Distância euclidiana", title="Método da MAIOR distancia (variaveis estandardizadas)")

# c) iii) -----
(membrosE <- cutree(agrupamentoE, 5))
table(membrosE)
# G1 (n=5):  Advogados, Arquitectos, Autores, Padres, Químicos
# G2 (n=12): Barbeiros, Camionistas, Canalizadores, Carpinteiros, Cozinheiros, 
#            Criadas, Electricistas, Guardas, Marceneiros, Mecânicos, Porteiro, Vendedores 
# G3 (n=4):  Comerciantes, Pintores, Polícias, Receptionistas,
# G4 (n=5):  Contabilistas, Dentistas, Prof.Liceu, Prof.Univers., Trab.Sociais
# G5 (n=3): Engenheiros, Maquinistas, Seguros

# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dadosE,list(membrosE),mean)
# G1: todas as medias > 0 -> acima da media em todas as variaveis
# G2: todas as médias < 0 -> abaixo da média em todas as variaveis
# G3: tx de suicidio muito acima da media
# G4: semelhante a 1, mas com tx sucidio abaixo da media
# G5: educacao abaixo a media, mas tudo o resto acima embora proximo da media
# rendimento e prestigio sao as variaveis que mais diferenciam os grupos
# G2: profissoes com elevado rendimento e prestigio
# G3: profissoes com baixo rendimento e prestigio
# ou graficamente
dataG <- aggregate(dadosE,list(membrosE),mean) %>%
  pivot_longer(cols = -Group.1,  # mantem a coluna Group.1 como fixa
               names_to = "Variavel", 
               values_to = "Media")
ggplot(dataG, aes(x = Variavel, y = Media, group = factor(Group.1), color = factor(Group.1))) +
  geom_line() +          
  geom_point(size = 3) + 
  labs(x = "", y = "Média", title = "Média das variáveis por grupo", color="Grupo")+
  theme_minimal()

plot(silhouette(cutree(agrupamentoE, 5), matriz.distanciasE))
# 13 e 8 perto de 0
plot(silhouette(cutree(agrupamentoE, 4), matriz.distanciasE))
# tudo ok -> sugere que solucao com 4 grupos deve ser preferivel

# solucao com 4 grupos
plot(agrupamentoE, 
     main = "Método da MAIOR distancia (variaveis estandardizadas)", 
     sub = "Dendograma", xlab = "", ylab="Distância euclidiana")
rect.hclust(agrupamentoE, 4)
(membrosE <- cutree(agrupamentoE, 4))
table(membrosE)
# G1 (n=5):  Advogados, Arquitectos, Autores, Padres, Químicos
# G2 (n=12): Barbeiros, Camionistas, Canalizadores, Carpinteiros, Cozinheiros, 
#            Criadas, Electricistas, Guardas, Marceneiros, Mecânicos, Porteiro, Vendedores 
# G3 (n=7):  Comerciantes, Engenheiros, Maquinistas, Pintores, Polícias, Receptionistas, Seguros
# G4 (n=5):  Contabilistas, Dentistas, Prof.Liceu, Prof.Univers., Trab.Sociais

# obter caracteristicas dos grupos (media de cada variavel numerica)
aggregate(dadosE,list(membrosE),mean)
# graficamente
dataG <- aggregate(dadosE,list(membrosE),mean) %>%
  pivot_longer(cols = -Group.1,  # mantem a coluna Group.1 como fixa
               names_to = "Variavel", 
               values_to = "Media")
geom_line() +          
  geom_point(size = 3) + 
  labs(x = "", y = "Média", title = "Média das variáveis por grupo", color="Grupo")+
  theme_minimal()
# G1: todas as medias > 0 -> acima da media em todas as variaveis
# G2: todas as médias < 0 -> abaixo da média em todas as variaveis
# G3: tx de suicidio muito acima da media
# G4: semelhante a 1, mas com tx sucidio abaixo da media e rendimento inferior (mais proximo da media)
# ou seja:
# G1: profissoes com elevado rendimento, prestigio e educacao
# G2: profissoes com baixo rendimento, prestigio, educacao e suicidio
# G3: profissoes com baixo rendimento, prestigio e educacao, mas elevado sucicidio
# G4: profissoes com elevado prestigio e educacao, mas com rendimnto proximo da media e baixo suicidio

# Em falta: que nome sugerem para cada grupo, com base nas suas características?
# (ou seja, no final devem nomear os grupos formados)


# d) -----
# outra alternativa para visualizar as caracteristicas dos grupos por individuo
library(pheatmap)
pheatmap(t(dados), cutree_cols = 4)
# rendimento é a variavel que mais distingue os grupos
pheatmap(t(dadosE), cutree_cols = 4)
# suicidio e rendimento sao as variaveis que mais distinguem os grupos

# diferencas com 4 grupos: versao Estandardizada vs original:
# juntou (arquitetos, padres, quimicos) com (advogados, autores) 
# (cozinheiros, eletricistas, marceneiros)  mudaram para grupo (barbeiros, ...)
# juntou Comerciantes a (Pintores, Polícias, Receptionistas)
# juntou (Contabilistas, Dentistas) a (Prof.Liceu, Trab.Sociais) e Prof.Univers.
# juntou Engenheiros a (Maquinistas, Seguros)
