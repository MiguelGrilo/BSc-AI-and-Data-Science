# leitura da base de dados
satisfacao <- read.csv2("Satisfacao.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                     sep=";", dec=",", header=T, encoding = "UTF-8")
summary(satisfacao) # Síntese dos dados
names(satisfacao) # designações das variáveis

### Contrastes actuais ###
contrasts(satisfacao$Politica.Salarial)
# Definição dos contrastes #
(contrastmatrix <- cbind(c(1, 1, -3, 1), c(2, -1, 0, -1), c(0, 1, 0, -1)))
# Verificação da ortogonalidade #
round(crossprod(contrastmatrix), 2)
# Análise de variância #
(contrasts(satisfacao$Politica.Salarial)<-contrastmatrix)
summary(aov(Satisfacao~Politica.Salarial, satisfacao), split=list(Politica.Salarial=list("Fixo VS. Variável"=1, "Bónus VS. Não Bónus"=2, "Tipo de Bónus"=3)))

# Obter médias estimadas
mod <- aov(Satisfacao~Politica.Salarial, data=satisfacao)
library(emmeans)
medias_est <- emmeans(mod, ~Politica.Salarial)
contrastes <- list(
  "Fixo VS. Variável" = c(1/3, 1/3, -1, 1/3),
  "Bónus VS. Não Bónus" = c(1, -1/2, 0, -1/2),
  "Variável VS. Lucros" = c(0, 1, 0, -1)
  )
resultados_contrastes <- contrast(medias_est, method=contrastes)
summary(resultados_contrastes, infer=TRUE)
