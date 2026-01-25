#### REGRESSÃO E CLASSIFICAÇÃO
#### TRABALHO 1
## Miguel Grilo    58387
## Jorge Couto     58656

rm(list=ls(all=TRUE))

#1
library(ggplot2)
# Cria uma semente para que a amostra possa ser reproduzida sempre de forma igual
set.seed(0)
summary(diamonds)
# Subconjunto com 4 variáveis quantitativas e 1 qualitativa
dados <- diamonds[sample(nrow(diamonds), 1000), 
                  c("price", "carat", "depth", "table", "cut")]
summary(dados)
#   Variáveis Quantitativas
# price -> Preço do Diamante, a nossa variável resposta
# carat -> Peso do Diamante
# depth -> Profundidade Relativa do Diamante (Medida que expressa a altura do 
#          diamante em relação à largura)
# table -> Proporção Entre o Diâmetro da "Mesa" do Diamante e a sua Largura
#   Variável Qualitativa
# cut   -> Qualidade do Corte do Diamante

#2
sum(is.na(dados))
colSums(is.na(dados))
# O conjunto de dados diamonds não contém dados omissos.

#3
pairs(dados[, c("price", "carat", "depth", "table")],
      main = "Scatterplots entre variáveis quantitativas")
#ou
library(GGally)
ggpairs(dados[, c("price", "carat", "depth", "table")])

#       Price / Carat
#   r = 0.921   ***
# Existe uma relação positiva muito alta entre carat e price e é 
# estatisticamente significativa
# À medida que o peso do diamante aumenta, o preço também tende a aumentar, no
# entanto aparenta um comportamento não linear dada a curvatura apresentada
# no gráfico

#       Price / Depth
#   r = 0.044
# A relação entre depth e price parece baixa ou inexistente
# O coeficiente de correlação é próximo de zero o que sugere que a depth 
# tem um impacto insignificante sobre o price, não sendo estatisticamente
# significativa

#       Price / Table
#   r = 0.116   ***
# A relação entre table e price é baixa no entanto é estatisticamente
# significativa

#       Carat / Depth
#   r = 0.059
# A relação entre carat e depth parece baixa ou inexistente e não é
# estatisticamente significativa

#       Carat / Table
#   r = 0.167   ***
# A relação entre carat e table é baixa no entanto é estatisticamente 
# significativa sugerindo que diamantes com maior peso têm uma mesa um pouco 
# maior

#       Depth / Table
#   r = -0.358  ***
# A relação entre table e depth é moderada e negativa, sendo 
# estatisticamente significativa, o que indica que em diamantes com table 
# maior, a depth tende a ser menor. 

ggplot(dados, aes(x = cut, y = price)) +
  geom_boxplot(fill = "green") +
  labs(title = "Distribuição do price por cut", x = "cut", y = "price")

#       Price / Cut
# Após observação podemos concluir que:
# A mediana do price tende a aumentar desde Fair até Premium
# Os cut Very Good, Premium e Ideal apresentam os maiores valores de price com
# muitos possíveis outliers associados
# Mesmo sendo Ideal a melhor classificação para cut, a mediana do price nesta
# categoria não é a mais elevada

#4
library(dplyr)
cor(dados %>% select(where(is.numeric)))
# Embora já analisado anteriormente visto que também recorremos a ggpairs() ...
# Correlação alta entre price e carat com r = 0.92085249    
# Correlação baixa entre price e depth com r = 0.04365188   
# Correlação baixa entre price e table com r = 0.1163144 
# Correlação baixa entre carat e depth com r = 0.05886091    
# Correlação baixa entre carat e table com r = 0.1669732 
# Correlação moderada entre depth e table com r = -0.3575062  

(anova_cut <- aov(price ~ cut, data = dados))
summary(anova_cut)
# Após análise da ANOVA verificamos que cut tem um efeito estatisticamente 
# significativo sobre price com valor p = 0.000162 

#5
(mod_carat <- lm(price ~ carat, data = dados))
summary(mod_carat)
# Se carat=0 então o valor estimado de price será -2135.56, o que não faria 
# sentido, sendo um valor monetário negativo
# Para cada aumento de uma unidade em carat, o price aumenta em média 7539.38     
# Valor p < 2e-16 logo é estatisticamente significativo
# R² = 0.848 logo este modelo explica 84.8% da variabilidade no price
#     price_estimado = -2135.56 + 7539.38 * carat
(mod_depth <- lm(price ~ depth, data = dados))
summary(mod_depth)
# Para cada aumento de uma unidade de depth, o price aumenta em média 118.49      
# Valor p = 0.525 logo não é estatisticamente significativo
# R² = 0.001905 logo este modelo explica 0.1905% da variabilidade no price
#     price_estimado = -3364.91 + 118.49 * depth

(mod_table <- lm(price ~ table, data = dados))
summary(mod_table)
# Para cada aumento de uma unidade de table, o price aumenta em média 200.75      
# Valor p = 0.015120 logo é estatisticamente significativo
# R² = 0.01353 logo este modelo explica 1.353% da variabilidade no price
#     price_estimado = -7598.01 + 200.75 * table


x <- factor( c(1,2,"a") , ordered = TRUE )
x
#[1] 1 2 a
#Levels: 1 < 2 < a

x <- factor( x , ordered = FALSE )
x


dados$cut <- factor(dados$cut, ordered = FALSE)
str(dados$cut)
(mod_cut <- lm(price ~ cut, data = dados))
summary(mod_cut)
is.ordered(dados$cut)
# Valor p = 1.39e-08 logo é estatisticamente significativo
# R² = 0.0223 logo este modelo explica 2.23% da variabilidade no price

#6
#   Modelo price ~ carat
# R² = 0.848 logo este modelo explica 84.8% da variabilidade no price
#   Modelo price ~ depth
# R² = 0.001905 logo este modelo explica 0.1905% da variabilidade no price
#   Modelo price ~ table
# R² = 0.01353 logo este modelo explica 1.353% da variabilidade no price
#   Modelo price ~ cut
# R² = 0.0223 logo este modelo explica 2.23% da variabilidade no price

# O modelo price ~ carat tem o maior coeficiente de determinação com
# R² = 0.848, indicando que a variável carat é a que melhor explica a 
# variação do price dos diamantes. Os modelos com depth, table e cut têm valores de 
# R² muito baixos, indicando um baixo poder explicativo.

#7
mod_log <- lm(price ~ log(carat), data = dados)
summary(mod_log)
# R-squared:  0.738

mod_pow <- lm(log(price) ~ log(carat), data = dados)
summary(mod_pow)
# R-squared:  0.9345

mod_carat_poly2 <- lm(price ~ poly(carat, 2), data = dados)
summary(mod_carat_poly2)
# R-squared:  0.848

mod_carat_poly3 <- lm(price ~ poly(carat, 3), data = dados)
summary(mod_carat_poly3)
# R-squared:  0.8698

# A melhor relação entre price e carat é dada pelo modelo onde tanto o
# price quanto o carat são transformados em logaritmos, que apresenta o maior 
# R-squared (0.9345).

#8
mod0 <- lm(price ~ 1, data = dados)

(mod_forward <- step(mod0, scope=~carat + depth + table + cut, direction = "forward"))
summary(mod_forward)

mod_completo <- lm(price ~ carat + depth + table + cut, data = dados)

(mod_backward <- step(mod_completo, direction = "backward"))
summary(mod_backward)

(mod_stepwise <- step(mod0, scope = ~carat + depth + table + cut, direction = "both"))
summary(mod_stepwise)

#9
#Usando as três direções diferentes o modelo sempre resultou no mesmo
#O modelo mais adequado é aquele que usa apenas as variáveis carat e cut
#As variáveis depth e table não são significativas o suficiente para que sejam incluídas no modelo

#10
anova(mod_stepwise)
summary(mod_stepwise)
#F-statística significativa → o modelo é globalmente significativo
#R² ajustado elevado → boa 

#11
par(mfrow = c(2,2))
plot(mod_stepwise)
#Resíduos vs ajustados: Padrão de abertura, não se pode verificar a homocedasticidade
#Q-Q plot: grande curvatura no início e fim da linha, parece não ser normal
#Scale-Location: padrão crescente (heterocedasticidade)
#Resíduos: alguns pontos fora do padrão mas nenhum ponto fora da distância de cook
hist(rstandard(mod_stepwise))
#Podemos confirmar, pelo histograma, que não é normal
#Tem comportamento parabólico mas com uma altura demasiado elevada no centro

#12
plot(mod_stepwise, which=4) 
#Não existem pontos influentes pela distância de cook, mas verifiquemos quais pontos são superiores a 4/length
cooksd <- cooks.distance(mod_stepwise)
which(cooksd > (4/length(cooksd)))
#Alguns valores são atípicos
#Podemos confirmar, então, que existem valores atípicos

#13a
#Verificar quais variáveis devemos transformar
residuos<-rstandard(mod_stepwise)
plot(dados$carat, residuos)
plot(dados$cut, residuos)
#Carat está a fazer padrão de abertura, devemos transformar essa variável
mod_transf1 <- lm(price ~ I(sqrt(carat)) + cut, data=dados)
plot(mod_transf1)
#Não parece ter corrigido nenhum dos problemas, tentemos outra transformação
mod_transf2 <- lm(price ~ log(carat) + cut, data=dados)
plot(mod_transf2)
#Talvez se tornarmos carat em um polinómio de grau 2 funcione
mod_transf3 <- lm(price ~ poly(carat, 2) + cut, data=dados)
plot(mod_transf3)
#E se invertermos carat?
mod_transf4 <- lm(price ~ I(1/carat) + price, data=dados)
plot(mod_transf4)

#Sugestão: transformar a variável resposta
mod_transf5 <- lm(log(price) ~ carat + price, data=dados)
plot(mod_transf5)
#Tentar transformar carat com log(price) como variável resposta
mod_transf6 <- lm(log(price) ~ I(sqrt(carat)) + cut, data=dados)
plot(mod_transf6)
mod_transf7 <- lm(log(price) ~ log(carat) + cut, data=dados)
plot(mod_transf7)
hist(rstandard(mod_transf7))
#Resíduos vs ajustados: não existe padrão de afunilamento
#Q-Q plot: amostras seguem uma linha reta, parece haver normalidade
#Scale-Location: Não existe padrão
#Resíduos: Nenhum ponto de fora da distância de Cook, temos os pressupostos validados
AIC(mod_stepwise, mod_transf1, mod_transf2, mod_transf3, mod_transf4, mod_transf5,
    mod_transf6, mod_transf7)
#O AIC também indica que esta é a nossa melhor transformação até então.
#Testemos, então, a adição de interação entre carat e cut.

#13b
mod_transf8 <- lm(log(price) ~ log(carat) * cut, data=dados)
plot(mod_transf8)
hist(rstandard(mod_transf8))
#Os pressupostos, mantêm-se validados
#Os resíduos estão mais próximos da distância de cook e alguns pontos no Q-Q plot estão mais afastados da reta
#Portanto, é melhor se não incluirmos interação

#14
mod_final <- lm(log(price) ~ log(carat) + cut, data = dados)
summary(mod_final)
plot(mod_final)
hist(rstandard(mod_final))
confint(mod_final)
#Interpretação:
#O modelo log-log permite interpretar coeficientes como elasticidades.
#Carat é o que mais influencia o preço, apesar do cut também influenciar.
#A 95% de confiança, o preço (em logaritmo) de um diamante varia entre 8.387 e 8.439
#Por cada unidade de peso extra (em logaritmo), o preço varia entre 1.659 e 1.714
#Cada tipo de corte diferente afeta o valor de modo diferente