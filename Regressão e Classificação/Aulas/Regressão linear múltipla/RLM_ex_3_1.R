# ******************************************************************************
#                     Regressao e Classificacao 2024/25
#
# Exercicios: Regressão linear multipla
#
# ******************************************************************************



# ----------------------------- Ex. 3.1 ----------------------------------------

# optativo: definir a diretoria atual como diretoria de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ler dados
dados <- read.csv2("vending.csv")
names(dados)

# a) ----
plot(dados[, -1])
# o tempo esta linearmente relacionado com caixas e distancia

# diagrama de dispersao 3D
scatter3d(tempo~caixas+distancia, data=dados)  # abre uma nova janela
scatter3d(tempo~caixas+distancia, data=dados, surface=F)  # sem plano

# alternativas

# diagrama de dispersao 2D com marcas a variar a cor por outra variavel
library(ggplot2)
gg <- ggplot(dados, aes(x = caixas, y = tempo, color = distancia)) +
  geom_point(size = 3) +
  labs(x = "Numero de caixas", y = "Tempo (em min)", color = "Distância (em m)") +
  theme_minimal()
print(gg)

# diagrama de dispersao 3D
library(plotly)
p <- plot_ly(dados, 
             x = ~caixas, y = ~tempo, z = ~distancia, 
             type = "scatter3d", 
             mode = "markers",
             marker = list(size = 3)) %>%
  layout(scene = list(xaxis = list(title = "Num. caixas"),
                      yaxis = list(title = "Tempo (em min.)"),
                      zaxis = list(title = "Distância (em m)")))
p

# diagrama de dispersao 3D
p1 <- plot_ly(dados, 
              x = ~caixas, y = ~tempo, z = ~distancia, 
              type = "scatter3d", 
              mode = "markers",
              marker = list(size = 3)) %>%
  layout(scene = list(xaxis = list(title = "Num. caixas"),
                      yaxis = list(title = "Tempo (em min.)", showbackground = TRUE),
                      zaxis = list(title = "Distância (em m)")))
p1


# b) ----
cor(dados[,-1])  # as variaveis estao todas muito correlacionadas com y e alguma correlacao entre x (mas sem preocupacao)

# ou via ggplot a) + b)
library(GGally)
ggpairs(dados %>% select(-amostra))  # excluir variavel amostra


# c)
modelo <- lm(tempo ~ caixas+distancia, data=dados)
modelo
vif(modelo)


# d) + e) ----
summary(modelo)


# f) ----
confint(modelo, level=.9)
# caso queiramos IC apenas para alguns parametros
confint(modelo, par=2:3, level=.9)


# g) ----
novosDados <- data.frame(caixas=c(3, 10, 20), distancia=c(100, 500, 1000))
predict(modelo, new=novosDados)  # previsao pontual

# h) i) ----
predict(modelo, new=novosDados, int="conf")  # previsao pontual e IC
# h) ii) ----
predict(modelo, new=novosDados, int="pred")



# i) i) ----
modelo1 <- lm(tempo ~ caixas, data=dados)
summary(modelo1)
# i) ii) ----
modelo2 <- lm(tempo ~ distancia, data=dados)
summary(modelo2)

# os coeficientes nao sao iguais aos do modelo de regressao linear multipla


# j) ----
# manualmente
(beta_caixas<- coef(modelo)["caixas"] * (sd(dados$caixas)/sd(dados$tempo)))
(beta_distancia <- coef(modelo)["distancia"] * (sd(dados$distancia)/sd(dados$tempo)))

# alternativa  
(modelo.beta <- lm(data.frame(scale(modelo$model))))  # betas estandardizados 
confint(modelo.beta)     # intervalos de confianca para betas estandardizados

# outra alternativa recorrendo a packages
library(parameters)
standardize_parameters(modelo)


