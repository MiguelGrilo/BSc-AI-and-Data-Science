## Teste de hipóteses e intervalo de confiança para a Proporção
binom.test(97,111, conf.level = 0.95) # IC para a proporção (também faz teste bilateral)
binom.test(97,111, p = 0.8, alternative="greater")
binom.test(97,111, p = 0.9, alternative="less")
## Teste de hipóteses e intervalo de confiança para a diferença de Proporções em amostras independentes
prop.test(c(97, 80),c(111, 121), conf.level = 0.95) # IC para a diferença de proporções (também faz teste bilateral)
prop.test(c(97, 80),c(111, 121), alternative="greater") # H0:p1-p2≤0 vs H1: p1-p2>0
# para p0 diferente de 0, correr a função
teste_diferenca_proporcoes <- function(x1, n1, x2, n2, p0, tipo_teste = "bilateral") {
  # Calcular proporções
  p1 <- x1 / n1
  p2 <- x2 / n2
  # Estatística z
  z <- ((p1 - p2) - p0) / sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))
  # Calcular valor p de acordo com o tipo de teste
  if (tipo_teste == "bilateral") {
    p_val <- 2 * (1 - pnorm(abs(z)))  # Duas caudas
  } else if (tipo_teste == "unilateral direito") {
    p_val <- 1 - pnorm(z)  # Cauda superior (H1: p1 - p2 > p0)
  } else if (tipo_teste == "unilateral esquerdo") {
    p_val <- pnorm(z)  # Cauda inferior (H1: p1 - p2 < p0)
  } else {
    stop("O tipo de teste deve ser 'bilateral', 'unilateral direito' ou 'unilateral esquerdo'.")
  }
  # resultados 
  list(
    p1 = p1,
    p2 = p2,
    z = z,
    p_val = p_val,
    tipo_teste = tipo_teste,
    p0=p0
  )
}
teste <- teste_diferenca_proporcoes(x1 = 97, x2 = 80,n1 = 111, n2 = 121, p0 = 0.1, "unilateral direito")
print(teste)
## Duas proporções em amostras emparelhadas
tabc <- as.table(rbind(c(30, 40), c(12, 18))) # tabela de contingência
dimnames(tabc)<-list(Antes = c("Apoiante", "Não apoiante"),
                     Depois = c("Apoiante", "Não apoiante"))
tabc
mcnemar.test(tabc)  # Teste de McNemar para igualdade de proporções 