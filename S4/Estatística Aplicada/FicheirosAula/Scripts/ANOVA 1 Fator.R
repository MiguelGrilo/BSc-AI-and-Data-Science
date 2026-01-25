# leitura da base de dados
bovinos <- read.csv2("bovinos.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                   sep=";", dec=",", header=T, encoding = "UTF-8")
summary(bovinos) # Síntese dos dados
names(bovinos) # designações das variáveis

# Calcular as médias, quartis e desvios padrão de T3 para cada Raça #
with (bovinos, tapply(T3,  Raça,  mean))
with (bovinos, tapply (T3,  Raça,  sd))
with (bovinos, tapply(T3, Raça,  quantile , p=(c(0.25, 0.5, 0.75)), na.rm=TRUE))

# Ver representações gráficas no final do script

mod  <-  aov(T3 ~  Raça,  data=bovinos) # modelo com T3 variável contínua e Raça variável categórica 
rs<-rstandard(mod) # resíduos standardizados do modelo
# Normalidade
library(car)
qqPlot(rs) # gráfico quantil-quantil aos resíduos
shapiro.test(rs) # teste de Shapiro-Wilk aos resíduos
# Em caso de rejeição
library(moments)
# Se ambos não se rejeitarem então podemos considerar que seja normal
agostino.test(rs) # teste de Agostino aos resíduos
anscombe.test (rs) # teste de Anscombe-Glynn aos resíduos
#Homogeneidade de Variâncias
plot(mod$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")
abline(h=0)
leveneTest(bovinos$T3~bovinos$Raça) # teste de Levene

####(Caso passe na normal)####

summary (mod) # resultados da ANOVA

# Tamanho do efeito
library(effectsize)
options(es.use_symbols = TRUE)
(om<-omega_squared(mod))# percentagem da variabilidade da resposta explicada pelo efeito do tratamento. Importância do fator em explicar a variabilidade da resposta.
# 0.01 - Efeito pequeno; 0.06 - Efeito médio; 0.14 - Efeito grande
cohens_f(mod, method="omega") 
# 0.1 - Efeito pequeno; 0.25 - Efeito médio; 0.40 - Efeito grande

# Comparações múltiplas
(extuk  <-  TukeyHSD(mod)) # comparações das médias 2 a 2 aplicando o Teste de Tukey
plot(extuk) # representação gráfica dos intervalos de confiança simultâneos
library(asbio)
(du<-pairw.anova(bovinos$T3, bovinos$Raça,method="tukey")) # Tukey~Kramer
(du<-pairw.anova(bovinos$T3, bovinos$Raça,method="dunnett", control="Alentejana")) # Dunnett
(sc<-pairw.anova(bovinos$T3,factor(bovinos$Raça),method="scheffe")) # Scheffé
(sl<-pairw.anova(bovinos$T3,factor(bovinos$Raça),method="lsd")) # LSD
plot(du, 2)

# Alternativas
library (DescTools)
ScheffeTest (mod)
library(multcomp)
dunnett_test <- glht(mod, linfct = mcp(Raça = "Dunnett"))
summary(dunnett_test)
# definir manualmente o grupo de controlo dados$grupo <- relevel(dados$grupo, ref = "Controlo")  

### para obter intervalos de confinaça para a média de cada tratamento
library(emmeans)
emmeans(mod, ~ Raça)


####(Caso não passe na normal)####

kruskal.test(T3 ~ Raça, data=bovinos) # teste de Kruskal-Wallis com T3 variável contínua e Raça variável categórica
library(dunn.test)
dunn.test(bovinos$T3, bovinos$Raça, method="holm") # Teste de Dunn para comparação dos ranks 2 a 2

####(Caso não passe na homocedasticidade)####

oneway.test(T3 ~ Raça, data=bovinos) # Welch
library(rstatix)
games_howell_test (bovinos, T3~Raça) # teste de Games-Howell com T3 variável contínua e Raça variável categórica

# Teste de comparação múltipla de Tukey com estimativas robustas das variâncias: método max-t #
library (multcomp)
library(sandwich)
mcomp <- glht(aov(T3 ~  Raça,  data=bovinos), mcp(Raça="Tukey"), vcov=vcovHC)
# O argumento vcov = vcovHC especifica o uso da estimativa HC3, consistente no caso de heterocedasticidade.
summary(mcomp)
confint (mcomp)
plot(mcomp)

library(coin)
oneway_test(T3 ~ Raça, data = bovinos, distribution = "approximate")

# ...................... #
#      Contrastes       #
# ......................#

### Contrastes actuais ###
contrasts(bovinos$Raça)
# Definição dos contrastes #
(contrastmatrix <- cbind(c(1, -1, 0, 0), c(0, 0, 1, -1), c(-1, -1, 1, 1)))
# Verificação da ortogonalidade #
round(crossprod(contrastmatrix), 2)
# Análise de variância #
(contrasts(bovinos$Raça)<-contrastmatrix)
summary(aov(T3~Raça, bovinos), split=list(Raça=list("A vs F"=1, "L vs M"=2, "A, F vs L, M"=3)))


# ...................... #
# Representações gráficas #
# ......................#

# Strip Chart #
library(ggplot2)
library(dplyr)
sc2<-ggplot(bovinos, aes(x = Raça, y = T3)) +  # variáveis a representar no gráfico
  geom_point(color = '#810000', size = 2.5) +   # cor e tamanho dos pontos
  stat_summary(fun = median, geom = 'point', size = 5, shape = 18, colour="steelblue")+ # tamanho, marca e cor da mediana
  scale_y_continuous(expand = c(0,0), limit = c(1, 3.5), breaks=seq(1,3.5,0.25))+ #formtação do eixo das abcissas
  xlab("Raça") + # título do eixo das abcissas
  ylab("Triiodotironina (T3)") + # título do eixo das ordenadas
  coord_flip()+ #rodar o gráfico
  theme(text = element_text(size=12, face="bold", colour="black"), # tamanho, fonte e cor do texto
        axis.text.x = element_text(size = 12), # tamanho da fonte no eixo das abcissas
        axis.text.y = element_text(size = 12), # tamanho da fonte no eixo das ordenadas
        panel.border = element_blank(), # sem linhas de painel
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) # cor do eixo
sc2
ggsave("sc2.png", width = 20, height = 15, units = "cm") # caso pretenda gravar a figura num dado formato

# Boxplot (apenas no caso em que as amostras tiverem uma dimensão de pelo menos 15 elementos) #
library (ggplot2)
bp1<-ggplot(bovinos, aes(x=Raça, y=T3)) + 
  geom_boxplot(fill="#810000", color="steelblue")+ #cores
  xlab("Raça")+
  coord_flip() + # caixas na horizontal
  scale_y_continuous(expand = c(0,0), limits=c(1, 3.5), breaks=seq(1,3.5,0.25),
                     name="Triiodotironina (T3)")+ #formtação do eixo das abcissas
  theme(text = element_text(size=16, face="bold", colour="black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.y=element_blank())
bp1
ggsave("bp1.png", width = 25, height = 15, units = "cm")

### visualizar intervalos de confiança para a média de cada tratamento
library(emmeans)
medias_ic_df <-as.data.frame(emmeans(mod, ~ Raça))
ggplot(medias_ic_df, aes(x = Raça, y = emmean)) +
  geom_point(size = 3, color = "blue") +  
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, color = "blue") +  # Barras de erro
  labs(title = "Intervalos de Confiança das Médias",
       x = "Grupo", y = "Média Estimada") +
  theme_minimal()  

### construir e representar intervalos de confiança por boostrap para a mediana
library(boot)

# Criar uma função para calcular a mediana (essa função será usada no bootstrap)
mediana_boot <- function(dados, indices) {
  return(median(dados[indices]))  # Calcula a mediana dos dados reamostrados
}

# Aplicar bootstrap para cada grupo
boot_A <- boot(bovinos$T3[bovinos$Raça == "Alentejana"], statistic = mediana_boot, R = 1000)
boot_F <- boot(bovinos$T3[bovinos$Raça == "Frisia"], statistic = mediana_boot, R = 1000)
boot_L <- boot(bovinos$T3[bovinos$Raça == "Limousine"], statistic = mediana_boot, R = 1000)
boot_M <- boot(bovinos$T3[bovinos$Raça == "Mertolenga"], statistic = mediana_boot, R = 1000)

# Calcular intervalos de confiança com bootstrap
IC_A <- boot.ci(boot_A, type = "perc")$percent[4:5]  # IC de 95% para Alentejana
IC_F <- boot.ci(boot_F, type = "perc")$percent[4:5]  # IC de 95% para Frisia
IC_L <- boot.ci(boot_L, type = "perc")$percent[4:5]  # IC de 95% para Limousine
IC_M <- boot.ci(boot_M, type = "perc")$percent[4:5]  # IC de 95% para Mertolenga

# Criar tabela com os resultados
medianas_ic_boot <- data.frame(
  Grupo = c("A", "F", "L", "M"),
  Mediana = c(median(bovinos$T3[bovinos$Raça == "Alentejana"]),
              median(bovinos$T3[bovinos$Raça == "Frisia"]),
              median(bovinos$T3[bovinos$Raça == "Limousine"]),
              median(bovinos$T3[bovinos$Raça == "Mertolenga"])),
  IC_Lower = c(IC_A[1], IC_F[1], IC_L[1], IC_M[1]),
  IC_Upper = c(IC_A[2], IC_F[2], IC_L[2], IC_M[2])
)
print(medianas_ic_boot)

ggplot(medianas_ic_boot, aes(x = Grupo, y = Mediana)) +
  geom_point(size = 3, color = "red") +  # Ponto para a mediana
  geom_errorbar(aes(ymin = IC_Lower, ymax = IC_Upper), width = 0.2, color = "red") + 
  labs(title = "Intervalos de Confiança para as Medianas (Bootstrap)",
       x = "Grupo", y = "Mediana Estimada") +
  theme_minimal()
