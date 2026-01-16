
# -------------------------------------------------------------
# PCA APLICADA À INFLAÇÃO EM PORTUGAL (IHPC - EUROSTAT)
# -------------------------------------------------------------
# 1. Carregar pacotes necessários
library(eurostat)
library(tidyverse)
library(psych)
library(corrplot)

# 2. Obter os dados do Eurostat (se ainda não tiveres)
hicp_data <- get_eurostat("prc_hicp_manr", time_format = "date")

# 3. Definir novo conjunto de variáveis com melhor adequação (MSA)
categorias_otimizadas_2 <- c(
  "CP0111",  # Bread & cereals
  "CP0112",  # Meat
  "CP0113",  # Fish & seafood
  "CP031",   # Clothing & footwear
  "CP051",   # Rents
  "CP053",   # Maintenance & repair of dwelling
  "CP071",   # Transport services
  "CP091"    # Recreation & culture
)

# Nomes legíveis para gráficos
coicop_labels_refinado <- c(
  CP0111 = "Bread & cereals",
  CP0112 = "Meat",
  CP0113 = "Fish & seafood",
  CP031  = "Clothing & footwear",
  CP051  = "Rents",
  CP053  = "Household maintenance",
  CP071  = "Transport services",
  CP091  = "Recreation & culture"
)

# 4. Filtrar dados para Portugal e essas categorias
hicp_pt_refinado <- hicp_data %>%
  filter(geo == "PT", coicop %in% categorias_otimizadas_2)

# 5. Transformar para formato wide
hicp_wide_refinado <- hicp_pt_refinado %>%
  select(TIME_PERIOD, coicop, values) %>%
  pivot_wider(names_from = coicop, values_from = values) %>%
  drop_na()

# 6. Substituir nomes de colunas pelos nomes legíveis
colnames(hicp_wide_refinado)[-1] <- coicop_labels_refinado[colnames(hicp_wide_refinado)[-1]]

# 7. Guardar vetor de datas 
dates_vector_refinado <- hicp_wide_refinado$TIME_PERIOD

# 8. Remover coluna de datas para análise estatística
hicp_wide_refinado <- hicp_wide_refinado %>% select(-TIME_PERIOD)

hicp_df <- as.data.frame(hicp_wide_refinado)
write.csv(hicp_df, file ="dados.csv", row.names = FALSE)

# 9. Verificar matriz de correlação
cor_matrix <- cor(hicp_wide_refinado)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black")

# 10. Aplicar teste de Kaiser-Meyer-Olkin (KMO)
kmo_result <- KMO(hicp_wide_refinado)
print(kmo_result)

# Interpretação automática
if (kmo_result$MSA < 0.5) {
  message("KMO < 0.5: Os dados podem não ser adequados para PCA.")
} else if (kmo_result$MSA < 0.6) {
  message("KMO entre 0.5 e 0.6: PCA possível, mas não ideal.")
} else {
  message("KMO > 0.6: PCA adequada.")
}

library(FactoMineR)
##### Aplicar PCA (com escalamento) #####
pca_result <- PCA(hicp_wide_refinado, scale.unit = TRUE, ncp = 10, graph = FALSE)

library(factoextra)
# Scree plot (gráfico do cotovelo)
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 30))


# Biplot das variáveis (contribuições)
fviz_pca_var(pca_result, col.var = "contrib", repel = TRUE)

# Análise temporal dos componentes principais
componentes <- as.data.frame(pca_result$ind$coord)
componentes$time <- dates_vector_refinado 

ggplot(componentes, aes(x = time, y = Dim.1)) +
  geom_line() +
  labs(title = "Componente Principal 1 ao longo do tempo", x = "Ano", y = "Dimensão 1 (PCA)")

#VARIMAX (rotação ortogonal para interpretação)
# PCA com matriz de correlações
pca_psych <- principal(hicp_wide_refinado, nfactors = 3, rotate = "varimax", scores = TRUE)

# Matriz de cargas fatoriais após rotação
print(pca_psych$loadings)

# Heatmap opcional para mostrar cargas
library(reshape2)
loadings_df <- as.data.frame(unclass(pca_psych$loadings))
loadings_df$Categoria <- rownames(loadings_df)
loadings_melt <- melt(loadings_df, id.vars = "Categoria")

ggplot(loadings_melt, aes(x = variable, y = Categoria, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Carga Fatorial") +
  theme_minimal() +
  labs(title = "Cargas Fatoriais - Rotação VARIMAX", x = "Componente", y = "Categoria")


# Extrair scores dos indivíduos (linhas), ou seja, os valores das observações nas componentes
componentes_pca <- as.data.frame(pca_result$ind$coord)

# Adicionar coluna com as datas originais (assumindo que dates_vector foi previamente criado)
componentes_pca$time <- dates_vector

# Reordenar para ter a data como primeira coluna (opcional)
componentes_pca <- componentes_pca %>% select(time, everything())

# Gravar os scores num ficheiro CSV para posterior uso em modelos de regressão, machine learning, etc.
write.csv(componentes_pca, "scores_PCA_IHPC.csv", row.names = FALSE)



### Próximos passos possíveis ###

# Modelos de regressão:
# Usar os scores dos componentes para prever inflação global, categorias específicas ou indicadores externos (ex: juros, desemprego).

# Segmentação temporal com clustering:
# Agrupar períodos com perfis inflacionistas semelhantes (ex: K-means sobre Dim.1 e Dim.2).

# Análises comparativas:
# Fazer o mesmo para outros países e comparar perfis inflacionistas via PCA.

