#### TPC2 ####
#### Miguel Grilo   58387
#### Jorge Couto    58656

#### FICHEIRO CARROS.CSV

# Criar a base de dados
carros<-read.csv2("Carros.csv", stringsAsFactors = T, 
                na.strings = c("NULL","", "NA"), 
                sep=";", dec=",", header=T, 
                encoding = "UTF-8")
carros
summary(carros)
carros$dif<-carros$Novo-carros$Antigo

# Normalidade
shapiro.test(carros$dif) # teste de Shapiro-Wilk aos valores das diferenças
# p-value = 0.7558 > 0.05(alfa)
# Logo, admitimos a normalidade
t.test(carros$Novo, carros$Antigo, mu=0, paired=TRUE, alternative="greater") 
# H0:μ(Novo) - μ(Antigo) ≤ 0 vs H1:μ(Novo) - μ(Antigo) > 0
# p-value = 1.048e-06
# Ou seja
# p-value < 0.001
# Logo, rejeitamos H0 e admitimos H1, ou seja,  μ(Novo) - μ(Antigo) > 0 <=>
#                                           <=> μ(Novo) > μ(Antigo)
# Há então evidência estatística para concluir que os carros novos têm, 
# em média, um consumo de combustível superior aos antigos.


#### FICHEIRO DIABETES.CSV

library(rstatix)
library(tidyverse)
library(ggpubr)
library(emmeans) 
library(afex)
library(car)
library(PMCMRplus)
library(psych)
library(coin)
library(nlme)
library(lme4)
library(lmerTest)
library(ARTool)

diabetes <- read.csv2 ("Diabetes.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                      dec=",", header=T, sep=";", encoding = "UTF-8") 
summary(diabetes)
# Paciente foi assumido com o formato errado então recorremos a 
# factor() para fazer essa correção
diabetes$Paciente <- factor(diabetes$Paciente)
summary(diabetes)

# Representação Gráfica
library (ggplot2)
bp1<-ggplot(diabetes, aes(x=Medição, y=HbA1c)) + 
  geom_boxplot()+
  xlab("Medição")+
  coord_flip() + # caixas na horizontal
  scale_fill_manual(values = "chocolate")+ #cores das caixas
  theme(text = element_text(size=12, face="bold", colour="black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.y=element_blank())+
  labs(color = "Medição")
bp1
# Deteção de outliers
outlier<-diabetes %>%
  group_by(Medição) %>%
  identify_outliers(HbA1c)
data.frame(outlier)
# <0 rows> (or 0-length row.names)
# Concluímos que não foram detetados outliers
# Normalidade
diabetes %>% 
  group_by(Medição) %>%
  shapiro_test(HbA1c)
ggqqplot(diabetes, "HbA1c", facet.by = "Medição")
# Valor p = 0.620 > 0.05
# Valor p = 0.924 > 0.05
# Valor p = 0.911 > 0.05
# Admitimos então a normalidade
# Homocedasticidade
leveneTest(HbA1c~Medição, data=diabetes)
# Valor p = 0.4053 > 0.05
# Admitimos então a homocedasticidade
# Modelo com Teste à Esfericidade
mod <-aov_car(HbA1c~Medição +Error(Paciente/Medição),data=diabetes) 
summary(mod)
# Valor p = 0.0026879 < 0.05
# Logo não admitimos a esfericidade
# Observando agora os valores p dos métodos Greenhouse-Geisser e Huynh-Feldt
# no modelo corriggido verificamos que estes têm um valor < 0.001
# Logo, a variável Medição continua a ser significativa após a correção
# Comparações múltiplas
mm<-emmeans(mod, "Medição", data=diabetes) 
pairs(mm, adjust="tukey")
# Todos os contrastes apresentam um valor p < 0.0001 logo concluímos que 
# todas as diferenças são significativas
# Observando agora as estimativas percebemos que:
# μ(T0) > μ(T1) pois a estimativa apresenta valor positivo
# μ(T0) > μ(T2) pois a estimativa apresenta valor positivo
# μ(T1) > μ(T2) pois a estimativa apresenta valor positivo
# Concluímos por fim que μ(T0) > μ(T1) > μ(T2)
# Ou seja, houve decréscimo nos níveis de HbA1c ao longo das Medições,
# o que é um desfecho positivo para os Pacientes
# Por fim, com base nas conclusões apresentadas anteriormente,
# ampliaria o programa a mais pacientes.