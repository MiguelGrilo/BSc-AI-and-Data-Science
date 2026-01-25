casas <- read.csv2("Trabalho_I_2324/casas.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                   sep=";", dec=",", header=T, encoding = "UTF-8")
summary(casas) # Síntese dos dados
names(casas) # designações das variáveis

casas$Dia_semana=factor(casas$Dia_semana, 
                        levels=c("Domingo", "Segunda-feira", "Terça-feira", 
                                 "Quarta-feira", "Quinta-feira", "Sexta-feira",
                                 "Sábado"), ordered=TRUE)

# Calcular as médias, quartis e desvios padrão de Dias para cada Dia_semana #
with (casas, tapply(Dias,  Dia_semana,  mean))
with (casas, tapply (Dias,  Dia_semana,  sd))
with (casas, tapply(Dias, Dia_semana,  quantile , p=(c(0.25, 0.5, 0.75)), na.rm=TRUE))

# Ver representações gráficas no final do script

mod  <-  aov(Dias ~  Dia_semana,  data=casas) # modelo com Dias variável contínua e Dia_semana variável categórica 
rs<-rstandard(mod) # resíduos standardizados do modelo
# Normalidade
library(car)
qqPlot(rs) # gráfico quantil-quantil aos resíduos
shapiro.test(rs) # teste de Shapiro-Wilk aos resíduos

#Homogeneidade de Variâncias
plot(mod$fit,  rs,  xlab="valores  ajustados",  ylab="resíduos")
abline(h=0)
leveneTest(casas$Dias~casas$Dia_semana) # teste de Levene

summary (mod) # resultados da ANOVA

# Tamanho do efeito
library(effectsize)
options(es.use_symbols = TRUE)
(om<-omega_squared(mod)) # percentagem da variabilidade da resposta explicada pelo efeito do tratamento. Importância do fator em explicar a variabilidade da resposta.
cohens_f(mod, method="omega") 

# Comparações múltiplas
(extuk  <-  TukeyHSD(mod)) # comparações das médias 2 a 2 aplicando o Teste de Tukey
plot(extuk) # representação gráfica dos intervalos de confiança simultâneos

# Boxplot (apenas no caso em que as amostras tiverem uma dimensão de pelo menos 15 elementos) #
library (ggplot2)
bp1<-ggplot(casas, aes(x=Dia_semana, y=Dias)) + 
  geom_boxplot(fill="#810000", color="steelblue")+ #cores
  xlab("Dia da semana")+
  coord_flip() + # caixas na horizontal
  scale_y_continuous(expand = c(0,0), limits=c(5, 155), breaks=seq(10,150,20),
                     name="Número de dias")+ #formtação do eixo das abcissas
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
medias_ic_df <-as.data.frame(emmeans(mod, ~ Dia_semana))
ggplot(medias_ic_df, aes(x = Dia_semana, y = emmean)) +
  geom_point(size = 3, color = "blue") +  
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, color = "blue") +  # Barras de erro
  labs(title = "Intervalos de Confiança das Médias",
       x = "Grupo", y = "Média Estimada") +
  theme_minimal()  


##### Extra #####
### Falha apenas a normalidade
kruskal.test(Dias ~ Dia_semana, data=casas) # teste de Kruskal-Wallis com Dias variável contínua e Dia_semana variável categórica
library(dunn.test)
dunn.test(casas$Dias, casas$Dia_semana, method="holm") # Teste de Dunn para comparação dos ranks 2 a 2


### Falha apenas a homocedasticidade
oneway.test(Dias ~ Dia_semana, data=casas) # Welch
# Teste de comparação múltipla de Tukey com estimativas robustas das variâncias: método max-t #
library (multcomp)
library(sandwich)
mcomp <- glht(aov(Dias ~  Dia_semana,  data=casas), mcp(Dia_semana="Tukey"), vcov=vcovHC)
# O argumento vcov = vcovHC especifica o uso da estimativa HC3, consistente no caso de heterocedasticidade.
summary(mcomp)
confint (mcomp)
plot(mcomp)


### Falha a normalidade e a homocedasticidade
print(games_howell_test (casas, Dias~Dia_semana), n=25) # teste de Games-Howell com Dias variável contínua e Dia_semana variável categórica


### 
### construir e representar intervalos de confiança por boostrap para a mediana
library(boot)

# Criar uma função para calcular a mediana (essa função será usada no bootstrap)
mediana_boot <- function(dados, indices) {
  return(median(dados[indices]))  # Calcula a mediana dos dados reamostrados
}

# Aplicar bootstrap para cada grupo
boot_1 <- boot(casas$Dias[casas$Dia_semana == "Domingo"], statistic = mediana_boot, R = 1000)
boot_2 <- boot(casas$Dias[casas$Dia_semana == "Segunda-feira"], statistic = mediana_boot, R = 1000)
boot_3 <- boot(casas$Dias[casas$Dia_semana == "Terça-feira"], statistic = mediana_boot, R = 1000)
boot_4 <- boot(casas$Dias[casas$Dia_semana == "Quarta-feira"], statistic = mediana_boot, R = 1000)
boot_5 <- boot(casas$Dias[casas$Dia_semana == "Quinta-feira"], statistic = mediana_boot, R = 1000)
boot_6 <- boot(casas$Dias[casas$Dia_semana == "Sexta-feira"], statistic = mediana_boot, R = 1000)
boot_7 <- boot(casas$Dias[casas$Dia_semana == "Sábado"], statistic = mediana_boot, R = 1000)

# Calcular intervalos de confiança com bootstrap
IC_1 <- boot.ci(boot_1, type = "perc")$percent[4:5]  # IC de 95% para Domingo
IC_2 <- boot.ci(boot_2, type = "perc")$percent[4:5]  # IC de 95% para Segunda
IC_3 <- boot.ci(boot_3, type = "perc")$percent[4:5]  # IC de 95% para Terça
IC_4 <- boot.ci(boot_4, type = "perc")$percent[4:5]  # IC de 95% para Quarta
IC_5 <- boot.ci(boot_5, type = "perc")$percent[4:5]  # IC de 95% para Quinta
IC_6 <- boot.ci(boot_6, type = "perc")$percent[4:5]  # IC de 95% para Sexta
IC_7 <- boot.ci(boot_7, type = "perc")$percent[4:5]  # IC de 95% para Sábado

# Criar tabela com os resultados
medianas_ic_boot <- data.frame(
  Grupo = c("1", "2", "3", "4", "5", "6", "7"),
  Mediana = c(median(casas$Dias[casas$Dia_semana == "Domingo"]),
              median(casas$Dias[casas$Dia_semana == "Segunda-feira"]),
              median(casas$Dias[casas$Dia_semana == "Terça-feira"]),
              median(casas$Dias[casas$Dia_semana == "Quarta-feira"]),
              median(casas$Dias[casas$Dia_semana == "Quinta-feira"]),
              median(casas$Dias[casas$Dia_semana == "Sexta-feira"]),
              median(casas$Dias[casas$Dia_semana == "Sábado"])),
  IC_Lower = c(IC_1[1], IC_2[1], IC_3[1], IC_4[1], IC_5[1]), IC_6[1]), IC_7[1]),
  IC_Upper = c(IC_1[2], IC_2[2], IC_3[2], IC_4[2], IC_5[2]), IC_6[2]), IC_7[2]),
)
print(medianas_ic_boot)

ggplot(medianas_ic_boot, aes(x = Grupo, y = Mediana)) +
  geom_point(size = 3, color = "red") +  # Ponto para a mediana
  geom_errorbar(aes(ymin = IC_Lower, ymax = IC_Upper), width = 0.2, color = "red") + 
  labs(title = "Intervalos de Confiança para as Medianas (Bootstrap)",
       x = "Grupo", y = "Mediana Estimada") +
  theme_minimal()
