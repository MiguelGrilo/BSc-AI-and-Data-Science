# 6.1
# H0: A gravidade não está relacionada (é independente) do local
# H1: A gravidade está relacionada com o local
tabela <- matrix(c(4229, 1046, 1381, 599), ncol=2, nrow=2)
rownames(tabela) <- c("Sem vítimas", "Com vítimas")
colnames(tabela) <- c("Dentro localidade", "Fora localidade")
tabela

chisq.test(tabela)

chisq.test(tabela)$stdres

fisher.test(tabela)

(599/1381)/(1046/4229)

(4229/1381)/(1046/599)

dados <- read.csv2("AFsub.csv", fileEncoding = "utf-8", stringsAsFactors = T, na.strings="")

head(dados)
str(dados)

summary(dados)

library(summarytools)
view(dfSummary(dados))

dados <- na.omit(dados)

dados$caminhada <- factor(ifelse(dados$modalidade=="Cminhada", "Sim", "Não"))
table(dados$modalidade, dados$caminhada)

library(crosstable)
crosstable(dados,
           c(idade, sexo, instrucao),
           by=caminhada,
           #total=TRUE,
           test=TRUE,
           showNA='no',
           percent_digits=1) %>%
  as_flextable()

mod <- glm(caminhada~idade+sexo+instrucao,data=dados,
           family=binomial(link=logit))
summary(mod)


mod1 <- glm(caminhada~idade+sexo,data=dados,
           family=binomial(link=logit))
summary(mod1)

anova(mod, mod1, test="Chisq")

drop1(mod, test="Chisq")

summary(mod1)
drop1(mod1, test="Chisq")

exp(mod1$coef)
(exp(mod1$coef)-1)*100
1/exp(mod1$coef)
confint(mod1)
exp(confint(mod1))
