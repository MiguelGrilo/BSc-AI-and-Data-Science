# leitura da base de dados
vendas <- read.csv2("vendas.csv", stringsAsFactors = T, 
                     na.strings = c("NULL","", "NA"), 
                   sep=";", dec=",", header=T, 
                   encoding = "UTF-8")
summary(vendas) # Síntese dos dados
names(vendas) # designações das variáveis

# Calcular as médias, quartis e desvios padrão de uni_vendidas para cada Preco #
with (vendas, tapply(uni_vendidas,  Preco,  mean))
with (vendas, tapply (uni_vendidas,  Preco,  sd))
with (vendas, tapply(uni_vendidas, Preco,  quantile , p=(c(0.25, 0.5, 0.75)), na.rm=TRUE))

# Obter as duas amostras a testar
P10<-subset(vendas, Preco=="10 € ")
P9<-subset(vendas, Preco=="9 € ")

# Normalidade
shapiro.test(P10$uni_vendidas) # teste de Shapiro-Wilk aos valores da amostra da uni_vendidas da P11
shapiro.test(P9$uni_vendidas) # teste de Shapiro-Wilk aos valores da amostra da uni_vendidas da P9

# Igualdade de Variâncias
var.test(P10$uni_vendidas, P9$uni_vendidas)

# Teste t
t.test(P10$uni_vendidas, P9$uni_vendidas, var.equal=TRUE) # teste t (idem ao anterior)

######## Wilcoxon-Mann_Whitney ########
library(exactRankTests)
# teste exato para comparação das distribuições quanto à localização
wilcox.exact(P10$uni_vendidas, P9$uni_vendidas, mu=0, alternative="less", exact=TRUE) # H0:h1-h2≥m vs H1:h1-h2<m

library(coin)
wilcox_test(uni_vendidas ~ Preco, data = vendas, mu=0, distribution = "exact")

# Representação Gráfica #
library (ggplot2)
bp1<-ggplot(vendas, aes(x = Preco, y = uni_vendidas)) + 
  geom_boxplot(fill="#810000", color="steelblue")+ #cores
  xlab("Preço")+
  coord_flip() + # caixas na horizontal
  scale_y_continuous(expand = c(0,0), limits=c(90, 212), breaks=seq(90,210,10),
                     name="Unidades vendidas")+ #formtação do eixo das abcissas
  theme(text = element_text(size=16, face="bold", colour="black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.y=element_blank())
bp1
ggsave("bp1.png", width = 25, height = 15, units = "cm")