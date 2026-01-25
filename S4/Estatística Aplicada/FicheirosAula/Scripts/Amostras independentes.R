bancos <- read.csv2("Bancos.csv", stringsAsFactors = T, 
                    na.strings = c("NULL","", "NA"), 
                    sep=";", dec=",", header=T, 
                    encoding = "UTF-8")
summary(bancos)
with (bancos, tapply(JUR,  CONC,  mean))
with (bancos, tapply (JUR,  CONC,  sd))
with (bancos, tapply(JUR, CONC,  quantile , p=(c(0.25, 0.5, 0.75)), na.rm=TRUE))

# Obter as duas amostras a testar
concsim<-subset(bancos, CONC=="Sim")
concnao<-subset(bancos, CONC=="Não")

concsim
concnao

# Normalidade
shapiro.test(concsim$JUR) # teste de Shapiro-Wilk aos valores da amostra 
shapiro.test(concnao$JUR) # teste de Shapiro-Wilk aos valores da amostra 

# Igualdade de Variâncias
var.test(concsim$JUR, concnao$JUR) # teste F de igualdade de variâncias

t.test(concsim$JUR, concnao$JUR, var.equal=TRUE, mu=0, alternative="less")

with (bancos, tapply(NEG,  CONC,  mean))
with (bancos, tapply (NEG,  CONC,  sd))
with (bancos, tapply(NEG, CONC,  quantile , p=(c(0.25, 0.5, 0.75)), na.rm=TRUE))

# Obter as duas amostras a testar
concsim<-subset(bancos, CONC=="Sim")
concnao<-subset(bancos, CONC=="Não")

concsim
concnao

# Normalidade
shapiro.test(concsim$NEG) # teste de Shapiro-Wilk aos valores da amostra 
shapiro.test(concnao$NEG) # teste de Shapiro-Wilk aos valores da amostra 

# Igualdade de Variâncias
var.test(concsim$NEG, concnao$NEG) # teste F de igualdade de variâncias

t.test(concsim$NEG, concnao$NEG, var.equal=TRUE, mu=0, alternative="less")

######## Wilcoxon-Mann_Whitney ########
library(coin)
wilcox_test(NEG ~ CONC, data = bancos, mu=0, alternative="greater", distribution = "exact")





# ...................... #
# Representações gráficas #
# ......................#

# Strip Chart #
library(ggplot2)
library(dplyr)
sc2<-ggplot(bancos, aes(x = CONC, y = JUR)) +  # variáveis a representar no gráfico
  geom_point(color = 'grey', size = 2.5) +   # cor e tamanho dos pontos
  stat_summary(fun = median, geom = 'point', size = 5, shape = 18, colour="#810000")+ # tamanho, marca e cor da mediana
  scale_y_continuous(expand = c(0,0), limit = c(8, 18), breaks=seq(8,18,1))+ #formtação do eixo das abcissas
  xlab("Concorrência") + # título do eixo das abcissas
  ylab("Taxa de Juro (%)") + # título do eixo das ordenadas
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
bp1<-ggplot(bancos, aes(x = CONC, y = JUR)) + 
  geom_boxplot(fill="#810000", color="steelblue")+ #cores
  xlab("Concorrência")+
  coord_flip() + # caixas na horizontal
  scale_y_continuous(expand = c(0,0), limits=c(8, 18), breaks=seq(8,18,1),
                     name="Taxa de Juro (%)")+ #formtação do eixo das abcissas
  theme(text = element_text(size=16, face="bold", colour="black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.y=element_blank())
bp1
ggsave("bp1.png", width = 25, height = 15, units = "cm")