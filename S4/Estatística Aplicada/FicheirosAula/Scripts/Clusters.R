# ==========================================
# ANALISE DE CLUSTERS - PERFIL DE CONSUMIDORES
# ==========================================

# Pacotes Necessários
library(reshape2)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(pheatmap)
library(FactoMineR)
library(factoextra)
library(cluster)
library(openxlsx)

# ------------------------------------------
# Leitura da base e Exploração Inicial
# ------------------------------------------
dados <- read.csv2("Dados Clusters/Comprascl.csv", sep = ";", header = TRUE)
str(dados)
summary(dados)

# ------------------------------------------
# Normalização e Escolha do Nº de Clusters
# ------------------------------------------
dados_norm <- scale(dados[, 6:10])
wss <- vector()
for (k in 1:10) {
  wss[k] <- sum(kmeans(dados_norm, centers = k, nstart = 25)$withinss)
}
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Número de Clusters", ylab = "Soma dos Quadrados Intra-cluster")

# ------------------------------------------
# K-means com k = 3
# ------------------------------------------
set.seed(123)
kmeans_result <- kmeans(dados_norm, centers = 3, nstart = 25)
dados$cluster <- as.factor(kmeans_result$cluster)

# Renomear colunas para clareza
colnames(dados)[2:7] <- c("Comprar_é_engraçado", "Mau_para_o_orçamento", "Comprar_e_comer", 
                          "Comprar_bem", "Sem_importância", "Comparar_preços")

# ------------------------------------------
# Gráfico de Perfis dos Clusters
# ------------------------------------------
perfil_clusters <- aggregate(dados[, 2:7], by = list(Cluster = dados$cluster), mean)
perfil_melt <- melt(perfil_clusters, id.vars = "Cluster")

ggplot(perfil_melt, aes(x = variable, y = value, group = Cluster, color = Cluster)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Perfis de Clusters de Consumidores",
       x = "Atitude em relação às compras",
       y = "Média da Resposta (1 = Discordo, 7 = Concordo)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Cluster	Perfil	
# 1	Impulsivo e emocional	- Compra por prazer; não compara preços; baixa preocupação orçamental
# 2	Neutro racional	- Pouca emoção; atitude equilibrada; comportamento moderado
# 3	Eficiente e racional - Compra bem; compara preços; não vê compra como prazer

# ------------------------------------------
# Árvore de Decisão
# ------------------------------------------
modelo_arvore <- rpart(cluster ~ ., data = dados[, 2:8], method = "class",
                       cp = 0.001, minsplit = 2, parms = list(split = "gini"))
rpart.plot(modelo_arvore, type = 4, extra = 101,
           main = "Árvore de Decisão para Previsão de Cluster")

#  | Número grande no topo | **Cluster predito** naquele nó                     |
# | Linha com 3 números   | Distribuição real dos consumidores por cluster     |
#  | Percentagem           | Quantos consumidores chegam a esse nó (% do total) |

# A árvore mostra que apenas 2 variáveis são suficientes para distinguir os perfis com alta precisão.


# ------------------------------------------
# Mapa de Calor dos Perfis
# ------------------------------------------
matriz_cluster <- aggregate(dados[, 2:7], by = list(Cluster = dados$cluster), mean)
rownames(matriz_cluster) <- paste("Cluster", matriz_cluster$Cluster)
matriz_cluster <- as.matrix(matriz_cluster[, -1])
colnames(matriz_cluster) <- c("Comprar é engraçado", "Mau p/ orçamento", "Comprar e comer",
                              "Comprar bem", "Sem importância", "Comparar preços")

pheatmap(matriz_cluster,
         cluster_rows = FALSE, cluster_cols = FALSE,
         display_numbers = TRUE, number_format = "%.1f",
         fontsize = 12, fontsize_number = 10, angle_col = 45,
         color = colorRampPalette(c("white", "#0072B2"))(100),
         main = "Mapa de Calor dos Perfis Médios por Cluster")

# ------------------------------------------
# Gráfico de Pizza c om tamanhod e cada cluster
# ------------------------------------------
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
  labs(title = "Distribuição de Consumidores por Cluster") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# ------------------------------------------
# PCA para Visualizar Clusters
# ------------------------------------------
res.pca <- PCA(dados[, 2:7], scale.unit = TRUE, graph = FALSE)
fviz_pca_ind(res.pca,
             habillage = dados$cluster,
             palette = c("tomato", "limegreen", "royalblue"),
             addEllipses = TRUE, ellipse.type = "convex",
             repel = TRUE,
             title = "Visualização dos Clusters em Componentes Principais")

# ------------------------------------------
# Análise de Silhueta
# ------------------------------------------
sil <- silhouette(as.numeric(dados$cluster), dist(dados_norm))
plot(sil, col = c("tomato", "limegreen", "royalblue"), main = "Gráfico de Silhouette")

# Valores próximos de 1.0 indicam que o ponto está bem agrupado.
# Valores próximos de 0.0 indicam que ele está na fronteira entre clusters.
# Valores negativos sugerem que o ponto está mal agrupado (não é o caso aqui).
# Média geral da largura de silhouette:
# 0.7: excelente
# 0.5 – 0.7: boa
# 0.25 – 0.5: fraca a média
# < 0.25: fraca
# Cluster	Nº de Indivíduos	
# 1	8	0.53	Boa coesão e separação
# 2	6	0.56	Melhor performance global
# 3	6	0.49	Um pouco menos coeso e separado

# ------------------------------------------
# Tabela Final e Exportação
# ------------------------------------------
perfil_final <- aggregate(dados[, 2:7], by = list(Cluster = dados$cluster), FUN = mean)
tamanhos <- as.data.frame(table(dados$cluster))
colnames(tamanhos) <- c("Cluster", "Total")
perfil_final <- merge(perfil_final, tamanhos, by = "Cluster")

write.xlsx(list(
  "Dados_com_Clusters" = dados,
  "Perfis_por_Cluster" = perfil_final
), file = "Resultados_Clusters.xlsx")


# EXTRA: Calcular silhouette médio para k = 2 a 6
silhouette_medios <- numeric()
intervalo_k <- 2:6

for (k in intervalo_k) {
  set.seed(123)
  km <- kmeans(dados_norm, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(dados_norm))
  silhouette_medios[k - 1] <- mean(sil[, 3])
}

# Gráfico
plot(intervalo_k, silhouette_medios, type = "b", pch = 19, col = "darkorange",
     xlab = "Número de Clusters (k)", ylab = "Silhouette Médio",
     main = "Silhouette Médio para Diferentes Valores de k")
grid()


