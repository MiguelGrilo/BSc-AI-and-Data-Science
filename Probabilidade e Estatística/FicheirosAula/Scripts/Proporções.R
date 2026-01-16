## Teste de hipóteses e intervalo de confiança para a Proporção
binom.test(80,150, conf.level = 0.90) # IC para a proporção (também faz teste bilateral)

## Teste de hipóteses e intervalo de confiança para a diferença de Proporções em amostras independentes
prop.test(c(180, 80),c(300, 150), conf.level = 0.90) # IC para a diferença de proporções (também faz teste bilateral)

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
teste <- teste_diferenca_proporcoes(x1 = 180, x2 = 80,n1 = 300, n2 = 150, p0 = 0.05, "unilateral direito")
print(teste)


## Duas proporções em amostras emparelhadas
tabc <- as.table(rbind(c(30, 20), c(10, 40))) # tabela de contingência
#Inserir linha a linha (ordem)
dimnames(tabc)<-list(Antes = c(Antes = "Preferência A", "Preferência B"),
                     Depois = c(Depois = "Preferência A", "Preferência B"))
tabc
mcnemar.test(tabc)  # Teste de McNemar para igualdade de proporções 
