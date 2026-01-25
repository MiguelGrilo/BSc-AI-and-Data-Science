# ******************************************************************************
#                     Regressao e Classificacao 2024/25
#
# Exercicios: Regressão linear
#
# ******************************************************************************

# ----------------------------- Ex. 2.8 ----------------------------------------

library(MASS)

dados <- Animals
str(dados)

# a) ---- 
library(ggplot2)
ggplot(dados, aes(x=body, y=brain))+
  geom_point()+theme_classic()
# problemas: nao linear e ha pontos que se destacam (outliers e claramente influentes)


# b) ----
with(dados, cor(brain, body))
# -0.005341163: relação linear quase inexistente


# c) ----
# i)
(g1 <- ggplot(dados, aes(x=body, y=log(brain)))+
   geom_point())
# nao linear e com pontos influentes
# ii)
(g2 <- ggplot(dados, aes(x=log(body), y=brain))+
    geom_point())
# nao linear e com pontos influentes

# iii)
(g3 <- ggplot(dados, aes(x=log(body), y=log(brain)))+
    geom_point())
# linear, mas ha outliers que podem ser influentes

# para visualizar os graficos lado a lado 
library(gridExtra)
grid.arrange(g1, g2, g3, ncol=3)


# d) -----
# a relação base é da forma da função potencia:
# brain = a0 * body^a1


# e) -----
# correlação
with(dados, cor(log(body), log(brain)))  
# r=0.7794935 -> correlação liner positiva forte entre log(body) e log(brain)

# coeficiente de determinação = correlação^2
with(dados, cor(log(body), log(brain)))^2  
# ou
reta <-lm(log(brain)~log(body), data=dados)
summary(reta)$r.sq
# 0.6076101 -> 60.76% da variabilidade de log(brain) é explicada pela relação linear com log(body)
# existem 3 pontos que estão a degradar a qualidade do ajustamento 


# f) ----- 
reta <-lm(log(brain)~log(body), data=dados)
reta
# Coefficients:
#(Intercept)    log(body)  
#      2.555        0.496 
# ou
coefficients(reta)
# modelo ajustado (variaveis logaritmizadas): log(brain)^ = 2.555 + 0.496 * log(body)
# b0: (neste caso nao tem significado...) um animal com body a pesar 0 kg (!!) tem, em média, 2.555 log(brain)
# b1: por cada aumento unitário em log(body) há um aumento, em média, de 0,496 em log(brain)

exp(coefficients(reta)[1])  # SO SE EXPONENCIA O 1º COEFICIENTE
# modelo ajustado (variaveis originais): brain = 12.87 + body^0.496
# b0: (neste caso nao tem significado...) um animal com body a pesar 0 kg (!!) tem, em média, 12.87 g de brain
# b1: por cada aumento de 1% no peso do body há um aumento, em média, 0.496% no peso do brain


# g) ----- 
with(dados, plot(log(body), log(brain)))  
identify(log(dados$body), log(dados$brain), tolerance=0.5)  
# temos que indicar as coordenadas pela sequencia (x, y) e aumentar o valor de tolerance! 
# identifica os casos: 6, 16, 26


# h) ----- 
# modelo sem as 3 observacoes
modelo2 <- lm(log(brain)~log(body),data=dados[-c(6,16,26),])
# igual a 
modelo2 <-update(reta, data=dados[-c(6,16,26),])
summary(modelo2)$r.sq
# 0.9216991  -> aumentou bastante!!


# i) ----- 
with(dados, plot(log(body), log(brain)))  
abline(reta, col="pink")        # modelo inicial com todas as observacoes
abline(modelo2, col="green")    # modelo sem as 3 observações
# tanto a ordenada na origem como o declive sofreram uma alteração substancial

# extra: comparar analise de residuos
par(mfrow=c(2,2))
plot(reta)  
# 1o grafico: problemas
#   Ha 3 observacoes que se destacam por terem residuo elevado: "Dipliodocus", "Triceratops", "Brachiosaurus" 
#   apresenta padrao
# 2o grafico: problemas
#   2 afetam a normalidade: "Triceratops", "Brachiosaurus"
# 3o grafico: nada a salientar
# 4o grafico: problemas
#    1 ("Brachiosaurus") tem distancia de Cook 0.5 e leverage > 2*2/n = 0.14 (sendo n=28), logo é ponto influente

n <- 28
dfbeta(reta)  
abs(dfbeta(reta)) > 2/sqrt(n)
# ok: todos |dfitts| < 2/raizq(n) = 0.378

dffits(reta)  
abs(dffits(reta))>2*sqrt(3/(n-3))
# "Dipliodocus", "Triceratops", "Brachiosaurus" com |dfitts| >  2*sqrt(3/(n-3)) = 0.693, logo sao pontos influentes
# conclusao: os 3 pontos identificados sao influentes


plot(modelo2)
# 1o grafico: ok. 
#   nao ha residuos elevados, apesar de destacar 3 pontos
#   pontos dentro de uma banda horizontal em torno de zero
# 2o grafico: algum afastamento na cauda superior mas nada de preocupante
# 3o grafico: nada a salientar
# 4o grafico: problemas
#    2 pontos com leverage > 2*2/n (com n=25), mas nao e nenhum dos destacados anteriormente

n<- 25
dfbeta(modelo2)  
abs(dfbeta(modelo2)) > 2/sqrt(n)
# ok: todos |dfitts| < 2/raizq(n) = 0.4

dffits(modelo2)  # ok: todos |dfitts| <  2*sqrt(3/(n-3)) = 0.734
abs(dffits(modelo2))>2*sqrt(3/(n-3))
# conclusao: tudo ok :-)

