# ******************************************************************************
#                     Regressao e Classificacao 2024/25
#
# Exercicios: Regressão linear
#
# ******************************************************************************

# ----------------------------- Ex. 2.5 ----------------------------------------

# optativo: definir a diretoria atual como diretoria de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ler dados
library(readxl)
dados <- read_excel("vendas.xlsx")
names(dados)
# ha um problema com os nomes das variaveis: os caracteres nao estao a ser reconhecidos corretamente.
# alterar nome das colunas
names(dados) <- c("y", "x")


# a) ----
reta <- lm(y ~x, data=dados)
reta
#Coefficients:
#(Intercept)            x  
#       2813        -1578  
# Interpretacao:
#   b0: quando o preço é 0 euros, em média, vendem-se 2813 embalagens.
#   b1: por cada aumento de 1 euro no preço, em média, vendem-se menos 1578 embalagens.


# b) ----
library(ggplot2)
# por ex. usando o ggplot: neste caso nao e preciso ajustar previamente a reta 
ggplot(dados, aes(x=x, y=y))+
  geom_point()+
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Reta de regressão
  labs(title = "Medicamento genérico", 
       x = "Preço", 
       y = "N.º de embalagens vendidas") +
  theme_bw()


# c) ----
(residuos <- residuals(reta))
(residuosE <- rstandard(reta))
(residuosS <- rstudent(reta))


# d) ----
# vamos precisar do valor dos residuos (obtidos em c) e dos valores y ajustados 
estimados <- fitted(reta)    # valores y ajustados

# Pressupostos: 
# 1. linearidade entre x e y 
plot(estimados, residuos)  # grafico de dispersao (y^, residuos) 
# a nuvem deve ter padrao linear. nao deve ter padrao curvo.

# 2. independencia dos residuos
plot(residuos)  
# o grafico nao deve apresentar padroes sistematicos

# 3. residuos com media nula
mean(residuos)
plot(estimados, residuos)  # grafico de dispersao (y^, residuos)
# os pontos devem dispor-se em torno de uma reta horizontal y=0

# 4. homocedasticidade
plot(estimados, residuos)  # grafico de dispersao (y^, residuos) 
# se o grafico tiver um padrao tipo megafone (cone) pode indicar heterocedasticidade

# 5. normalidade dos residuos
library(car)
qqPlot(residuosE)
shapiro.test(residuosE)  # H0: residuos ~ Normal vs. H1: residuos nao~ Normal

# Analise de residuos
plot(reta, which = 5)  # Distância de Cook, Leverage e residuos
plot(reta, which = 4)  # Só distância de Cook

# Diagnostico simples: substitui os graficos anteriores 
par(mfrow = c(2, 2))  # Divide a janela do grafico em 2 linhas e 2 colunas
plot(reta)
par(mfrow = c(1, 1))  # repor definicoes iniciais da janela


# e) ----
cor(y, x)


# f) e g) ----
anova(reta)  # tabela anova e estimativa da variância
# valor 2644

# alternativa para estimativa da variância
summary(reta)
# no final: Residual standard error: 51.42 --> 51.42^2 = 2644


# h) ----
confint(reta, level=.99)

# alternativa: implementando o IC
alfa <- 0.01
n <- dim(dados)[1]                # dimensao do conjunto de dados
b0 <- coef(reta)["(Intercept)"]   # b0
SEb0 <- summary(reta)$coefficients["(Intercept)", "Std. Error"]  # sigma(b0)
# IC
IC <- b0 + qt(c(alfa/2, 1-alfa/2), n-2)*SEb0
round(IC, 3)


# i) ----
confint(reta)

# alternativa: implementando o IC
alfa <- 0.05
n <- dim(dados)[1]      # dimensao do conjunto de dados
b1 <- coef(reta)["x"]   # b1
SEb1 <- summary(reta)$coefficients["x", "Std. Error"]  # sigma(b1)
# IC
IC <- b1 + qt(c(alfa/2, 1-alfa/2), n-2)*SEb1
round(IC, 3)


# j) k) ----
summary(reta)

# alternativa j) implementando o teste:
# H0: beta0=0 vs. H1: beta0!=0 (teste bilateral)
n <- dim(dados)[1]  # dimensao do conjunto de dados
beta0.0 <- 0                      # valor de beta1 em H0
b0 <- coef(reta)["(Intercept)"]   # b0
SEb0 <- summary(reta)$coefficients["(Intercept)", "Std. Error"]  # sigma(b0)
# valor observado para a estatistica de teste
ETobs <- (b0 - beta0.0) / SEb0
# p-value bilateral
valorp <- 2*(1-pt(abs(ETobs), n-2))
print(paste("ETobs =", round(ETobs, 3), "p-value =",round(valorp, 3)))

# alternativa k) implementando o teste:
# H0: beta1=0 vs. H1: beta1!=0  (teste bilateral)
n <- dim(dados)[1]  # dimensao do conjunto de dados
beta1.0 <- 0            # valor de beta1 em H0
b1 <- coef(reta)["x"]   # b1
SEb1 <- summary(reta)$coefficients["x", "Std. Error"]  # sigma(b1)
# valor observado para a estatistica de teste
ETobs <- (b1 - beta1.0) / SEb1
# p-value bilateral
valorp <- 2*(1-pt(abs(ETobs), n-2))
print(paste("ETobs =", round(ETobs, 3), "p-value =",round(valorp, 3)))


# l) ----
summary(reta)$r.sq


# m) ----
novosDados <- data.frame(x=1.23)
predict(reta, new=novosDados)  # previsao pontual
predict(reta, new=novosDados, int="conf")  # previsao pontual e IC


# n) ----
predict(reta, new=novosDados, int="pred")


# extra) IC a 99% para o número esperado de embalagens vendidas quando o preço de cada embalagem é 1,1 ou 1,3 euros. 
novosDados <- data.frame(x=c(1.1, 1.3))
predict(reta, new=novosDados, int="conf", level=0.99)

