#5
  (dados <- read.table("clipboard", header=T))

## a)
colMeans(dados[, -1])

with(dados, by(cbind(V1,V2), Grupo, colMeans))

## b)
with(dados, by(cbind(V1,V2), Grupo, cov))

p <- dim(dados)[2] - 1
n <- dim(dados)[1]
grupos <- unique(dados$Grupo)
g <- length(grupos)
(Sp <- matrix(NA, ncol=p, nrow=p))

for (i in grupos){
  dadosg <- subset(dados, Grupo==i, select=-Grupo)
  covg <- cov(dadosg)
  Sp <- Sp + (dim(dadosg)[1] - 1) * covg
}
Sp <- Sp / (n-g)
Sp #Spooled

# c) ---
B <- matrix(0, ncol=p, nrow=p)
W <- matrix(0, ncol=p, nrow=p)

for (i in grupos){
  dadosg <- subset(dados, Grupo==i, select=-Grupo)
  mediag <- colMeans(dadosg)
W <- W + cov(dadosg)*(nrow(dadosg)-1)
difmedias <- matrix(mediasg-mediaglobal, ncol=1)
  B <- B + nrow(dadosg) * (difmedias %*% difmedias)
}
B
W

