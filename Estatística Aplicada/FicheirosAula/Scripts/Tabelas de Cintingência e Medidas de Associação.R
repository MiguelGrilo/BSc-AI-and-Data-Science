# Pacotes necessários
library(rcompanion)
library(irr)
library(correlation)
library(forcats)
library(moments)


# Limpar memória #
rm(list=ls(all=TRUE))
AF <- read.csv2("AF.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                sep=";",dec=".", header=T, encoding = "UTF-8")
names(AF)
summary(AF)
Indices <- read.csv2("Indices.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                sep=";",dec=",", header=T, encoding = "UTF-8")
names(Indices)
summary(Indices)

# ............................#
# Tabelas de contingência 2X2 #
# ............................#

(tc<-table(AF$sexo, AF$praticacaminhadas))
round(prop.table(tc, 1),2) # proporções por linha
round(prop.table(tc, 2),2) # proporções por coluna
(tcq<-chisq.test(tc)) # teste qui-quadrado
tcq$expected # valores esperados sob a hipótese de independência 
(tf<-fisher.test(tc)) # teste exato de Fisher (inclui Odds Ratio e respetivo intervalo de confiança)
1/c(tf$estimate, tf$conf.int) # inversão da categoria de referência 

# exemplo de caso de um estudo de coorte #
tc <- as.table(rbind(c(10845, 189), c(10933, 104))) # valores da tabela de contingência
dimnames(tc) <- list(Exposição = c("Placebo","Aspirina"), # atribuição dos labels às linhas e colunas
                     Estado = c("Sem IM","Com IM"))
tc
library(epitools) 
riskratio (tc, conf.level = 0.90) ## Cálculo do risco relativo ##
riskratio (tc, conf.level = 0.90, rev="r") ## Cálculo do risco relativo invertendo as linhas ##

# ............................#
# Tabelas de contingência nxk #
# ............................#

(tc<-table(AF$instrucao, AF$praticacaminhadas)) # Tabela de contigência
(tcq<-chisq.test(tc)) # teste qui-quadrado
tcq$expected # valores esperados sob a hipótese de independência
tcq$stdres # Residuos standardizados

cramerV(tc, ci=TRUE) # V de Cramer e intervalo de confiança

# ...........................#
# coeficientes de correlação #
# ...........................#

# duas variáveis contínuas #
shapiro.test(Indices$CPI)
shapiro.test(Indices$HI)
cor_test(Indices, "CPI", "HI", ci=.95, method="pearson") # correlação de Pearson 

shapiro.test(Indices$PIB_per_capita)
agostino.test(Indices$PIB_per_capita)
cor_test(Indices, "CPI", "PIB_per_capita", ci=.95, method="spearman") # correlação de Spearman
cor_test(Indices, "CPI", "PIB_per_capita", ci=.95, method="kendal") # correlação de kendall

cor_test(AF, "instrucao", "tempodepratica", ci=.95, method="gamma") # correlação de Goodman-Kruskal

cor_test(AF, "idade", "praticacaminhadas", ci=.95, method="pearson") # Coeficiente de correlação bisserial por pontos

Indices$PIB<-ifelse(Indices$PIB_per_capita<=median(Indices$PIB_per_capita), "Abaixo da mediana", "Acima da mediana")
Indices$PIB<-factor(Indices$PIB)
summary(Indices$PIB)
cor_test(Indices, "CPI", "PIB", ci=.95, method="biserial") # Coeficiente de correlação bisserial

Indices$HI2<-ifelse(Indices$HI<=mean(Indices$HI), "Abaixo da média", "Acima da média")
Indices$HI2<-factor(Indices$HI2)
summary(Indices$HI2)

Indices$PIBn<-as.numeric(Indices$PIB)-1
Indices$HI2n<-as.numeric(Indices$HI2)-1
cor_test(Indices, "PIBn", "HI2n", ci=.95, method="tetrachoric") # Coeficiente de correlação tetracórico


#............. ...........#
# Medidas de Concordância #
# ........................#

# Exemplo em que 6 provadores ordenam 4 vinhos #
datac<-data.frame(
  vinhos=c("E", "D", "A", "S"),
  Provador1=c(3, 2, 1, 4),
  Provador2=c(3, 2, 1, 4),
  Provador3=c(3, 2, 1, 4),
  Provador4=c(4, 2, 1, 3),
  Provador5=c(3, 2, 1, 4),
  Provador6=c(4, 1, 2, 3)
)
datac
kendall(datac[,2:7], correct=TRUE) # Coeficiente de concordância Kendall W 

# Exemplo diagnósticos feitos por dois médicos a 10 pacientes #
medico1 <- c("D1", "D2", "D3", "D1", "D2", "D3", "D1", "D1", "D2", "D3")
medico2 <- c("D1", "D3", "D3" ,"D1", "D2", "D3", "D2", "D1", "D2", "D1") 
datak <- data.frame(medico1,medico2)
datak
kappa2(datak) # coeficiente K de Cohen 


#................................#
#     Associação condiconal.     #
# .............................. #

(tcc<-with(AF,table(sexo, praticacaminhadas, tempodepratica)))
mantelhaen.test (tcc, correct=FALSE) ## teste de Mantel-Haenszel à independência condicional
library(DescTools)
BreslowDayTest (tcc)  ## teste de Breslow-Day à homogeneidade dos odds ratio


#.......................................#
#     Agumas representações gráficas    #
# ......................................#

# Mosaico #
library(ggplot2)
library(ggmosaic)
library(RColorBrewer)
library(dplyr)
p<-ggplot(data =AF ) +
  geom_mosaic(aes(x = product(sexo, Modalidades), fill = sexo)) +
  geom_mosaic_text(aes(x = product(sexo, Modalidades),
                       label = after_stat(.wt)), colour="transparent")+
  theme_minimal() +
  theme(
    text = element_text(size=14, face="bold", colour="black"),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "none")+
  xlab("")+ylab("")
df2 <- ggplot_build(p)$data[[1]]
df2$pr <- round(100*df2$.wt/sum(df2$.wt), 1) # percentagens com uma decimal
df2$lab <- paste0(df2$pr, "% (", df2$.wt, ")") # juntar valores absolutos aos labels
mp<-p + geom_text(data = ggplot_build(p)$data[[1]] %>%
                    group_by(x__fill__sexo) %>%
                    mutate(pct = .wt/sum(.wt)*100),
                  aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label=paste0(round(pct, 1), "% (", df2$.wt, ")")), size=5, fontface = "bold")+
  labs(title = "Praticantes por modalidade em função do sexo")+
  scale_fill_manual(values = brewer.pal(4, "Dark2"))
mp # mosaico com frequências condiconais ao sexo
# ggsave("mplm.png", width = 25, height = 18, units = "cm") para gravar em png

p<-ggplot(data =AF ) +
  geom_mosaic(aes(x = product(Modalidades, sexo), fill = Modalidades)) +
  geom_mosaic_text(aes(x = product(Modalidades, sexo),
                       label = after_stat(.wt)), colour="transparent")+
  theme_minimal() +
  theme(
    text = element_text(size=14, face="bold", colour="black"),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "none")+
  xlab("")+ylab("")
df2 <- ggplot_build(p)$data[[1]]
df2$pr <- round(100*df2$.wt/sum(df2$.wt), 1) # percentagens com uma decimal
df2$lab <- paste0(df2$pr, "% (", df2$.wt, ")") # juntar valores absolutos aos labels
mp<-p + geom_text(data = ggplot_build(p)$data[[1]] %>%
                    group_by(x__fill__Modalidades) %>%
                    mutate(pct = .wt/sum(.wt)*100),
                  aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label=paste0(round(pct, 1), "% (", df2$.wt, ")")), size=5, fontface = "bold")+
  labs(title = "Praticantes por sexo em função da modalidade")+
  scale_fill_manual(values = brewer.pal(4, "Dark2"))
mp # mosaico com frequências condiconais à modalidade


# Correleograma #
Indices1<-Indices[, 2:7]
library(ggcorrplot)
mcor <- round(cor(Indices1,                             # coef. Spearman
                  method="spearman",
                  use = "pairwise.complete.obs"),1)
library(corrplot)
pcor <- cor_pmat(Indices1,                       # p-values do coef. Spearman
                 method="spearman",
                 use = "pairwise.complete.obs")
c1<-ggcorrplot(mcor,
               type = "upper",
               outline.color = "white",
               colors = c("steelblue", "white", "darkred"),
               lab_size = 4,
               p.mat = pcor,
               hc.order = TRUE,
               insig = "blank",
               lab = TRUE,
               digits = 1,
               lab_col = "white",
               ggtheme=theme_classic,
               show.legend = FALSE
)
c1
# ggsave("c2.png", width = 17, height = 13, units = "cm")




exp(0.10796*5)
