# ******************************************************************************
#                     Regressao e Classificacao 2024/25
#
# Exercicios: Análise discriminante
#
# ******************************************************************************


# ----------------------------- Ex. 5.1 ----------------------------------------

pop1 <- data.frame(g=1, x1=c(-2,0,-1), x2=c(5,3,1))
pop2 <- data.frame(g=2, x1=c(0,2,1), x2=c(6,4,2))
pop3 <- data.frame(g=3, x1=c(1,0,-1), x2=c(-2,0,-4))
dados <- rbind(pop1, pop2, pop3)

# constantes ou vetores que vao fazer falta
# dimensao dos grupos
(ni <- table(dados$g))     # dimensao de cada grupo
grupos <- unique(dados$g)  # grupos
n <- dim(dados)[1]         # numero total de observacoes
p <- dim(dados)[2]-1       # numero de vars independentes
g <- length(grupos)        # total de grupos


# a) -----
# medias por grupos
with(dados, by(cbind(x1, x2), g, colMeans))


# b) -----
# matrizes variancias e covariancias por grupo
with(dados, by(cbind(x1, x2), g, cov))

# mas como obter varPooled
VV <- matrix(0, ncol=p, nrow=p)   # iniciar matriz Spooled
for (i in grupos){
  dadosg <- subset(dados, g==i, select=-g)
  covg <- cov(dadosg)
  VV <- VV + cov(dadosg)*(ni[g]-1)/(n-g)
}
# ver matriz Spooled
(Spooled <- VV)


# c) -----

# obter Matrizes W e B
# media geral
media_geral <- colMeans(dados[,-1])
# iniciar matrizes
W <- matrix(0, ncol=p, nrow=p)   # matriz W
B <- matrix(0, ncol=p, nrow=p)   # matriz B

for (i in grupos){
  dadosg <- subset(dados, g==i, select=-g)
  mediasg <- colMeans(dadosg)
  W <- W + cov(dadosg) * (nrow(dadosg) - 1)
  difmedias <- matrix(mediasg - media_geral, ncol=1)
  B <- B + nrow(dadosg) * (difmedias %*% t(difmedias))
}
# ver matrizes W e B
W
B

# alternativa para obte W
# W <- (n-g)*Spooled


# d) -----
# Inversa de W
Winv <- solve(W)
Winv

# W^{-1} * B
Winv_B <- Winv %*% B
Winv_B

# Valores proprios reais
eigen_result <- eigen(Winv_B)
eigen_result$values

# vetores proprios normalizados
eigenvectors <- eigen_result$vectors  # Matriz cujas colunas são os vetores próprios
eigenvectors