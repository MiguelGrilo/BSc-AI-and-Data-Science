# ******************************************************************************
#                     Regressao e Classificacao 2024/25
#
# Exercicios: Regressao Multinomial (e logistica)
#
# ******************************************************************************


# ------------------------------- Ex. 6.1 --------------------------------------

# criar tabela com dados
tabela <- matrix(c(4229, 1381, 1046, 599), ncol=2, byrow=T)
rownames(tabela) <- c("Sem vitimas", "Com vitimas")
colnames(tabela) <- c("Dentro localidade", "Fora localidade")
tabela

## a) ----- 
# Teste do qui-quadrado de independência
# H0: X e Y independentes vs H1: Há relação entre X e Y
chisq.test(tabela)
# p-value < 0.001 -> rej H0 
# Aos niveis usuais de significância, há evidência estatística para afirmar que 
# há relação entre a gravidade do acidente e o local onde este ocorre
chisq.test(tabela)$expected  # freq. esperadas
chisq.test(tabela)$stdres    # residuos estandardizados

# se as amostras forem pequenas ou os pressupostos forem violados, usar teste de Fisher
fisher.test(tabela)
# p-value < 0.001 -> rej H0 -> (igual conclusão)
# devolve OR = 1.75 = (a/c)/(b/d) = (tabela[1,1]/tabela[2,1]) / (tabela[1,2]/tabela[2,2])


## b) ---- 

## --- chances = odds
(chances <- tabela[2,]/tabela[1,])

## Em alternativa podemos inverter estes valores, o que torna a interpretação mais "simpática" (pois a chance anterior <1):
(chances.inv <- tabela[1,]/tabela[2,]) # = 1/chances


(chances2 <- tabela[,2]/tabela[,1])
## Ou invertendo
(chances2.inv <- tabela[,1]/tabela[,2]) # = 1/chances2


# --- razao de chances = odd ratios
(OR <- chances[1]/chances[2])
# Em alternativa podemos inverter estes valores, o que torna a interpretação mais "simpática" (pois o OR anterior <1):
1/OR

(OR2 <- chances2[1]/chances2[2])
# ou invertendo
1/OR2


# --- extra: alternativa que devolve resultado do teste Fisher, um OR e respectivo IC
(odds.ratio(tabela))
