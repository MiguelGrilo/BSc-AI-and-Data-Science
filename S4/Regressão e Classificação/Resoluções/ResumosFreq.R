##### ESTUDO FREQUÊNCIA #####
### Miguel Grilo    58387

#### CORRELAÇÃO ####
### EXERCÍCIO 1.1 ###

tempos <- read.csv2("Correlação/tempos.csv")
str(tempos)
head(tempos)
summary(tempos)



  ## ALÍNEA A ##
  # Através da análise gráfica, parece-lhe existir relação linear entre as duas 
  # variáveis?

  plot(tempos$y ~ tempos$x)
  # ou
  plot(tempos$x, tempos$y)
  # Dado o baixo número de observações é complicado admitir uma relação linear, 
  # no entanto com as observações apresentadas parece suscetível à existência de 
  # uma relação linear.



  ## ALINEA B ##
  # Calcule e interprete o valor do coeficiente de correlação linear de Pearson.

  cor(tempos$x, tempos$y)
  # ou
  cor.test(tempos$x, tempos$y)
  # Coeficiente de correlação linear de Pearson = 0.4433094
  # Dado o valor do coeficiente ser aproximadamente 0.44 admitimos que apresenta 
  # uma correlação moderada e sendo o valor positivo podemos ainda admitir que
  # em média, X varia diretamente com Y.


  #   |r|           Interpretação
  #   [0; 0.2[      Não existe correlação ou é desprezável
  #   [0.2; 0.7[    Correlação Moderada
  #   [0.7; 0.9[    Correlação Forte
  #   [0.9; 1]      Correlação Muito Forte



### EXERCÍCIO 1.2 ###

ml <- read.csv2("Correlação/ML.csv")
str(ml)
head(ml)
summary(ml)



  ## ALINEA A ##
  # Considera que o tempo de execução está relacionado com o número de 
  # parâmetros? Justifique com auxílio de um gráfico.

  plot(ml$NumParametros ~ ml$Tempo)
  # A partir somente da visualização gráfica podemos concluir que existe uma 
  # relação linear entre o Tempo e o Número de Parâmetros dado o declive
  # aproximadamente linear formado pelas observações.



  ## ALINEA B ##
  # Calcule a covariância entre o tempo de execução e o número de parâmetros.

  cov(ml$Tempo, ml$NumParametros)
  # Covariância = 369.8659
  # Como a covariância é positiva, concluímos que existe relação linear positiva
  # entre as duas variáveis.



  ## ALINEA C ##
  # Calcule o coeficiente de correlação de Pearson entre o tempo de execução e o 
  # número de parâmetros.

  cor(ml$Tempo, ml$NumParametros)
  # Coeficiente de correlação de Pearson = 0.993836
  # Dado o valor do coeficiente ser aproximadamente 0.99 admitimos a existência 
  # de uma correlação muito forte e ainda sendo este valor tão próximo de 1, 
  # admitimos também há uma relação linear positiva quase perfeita.



  ## ALINEA D ##
  # Repita as alíneas anteriores, considerando o tempo em minutos. Pronuncie-se 
  # sobre as diferenças obtidas. 

  ml$TempoEmMinutos <- ml$Tempo/60

      # ALINEA A #

      plot(ml$NumParametros ~ ml$TempoEmMinutos)
      # Continuamos a admitir uma relação linear entre TempoEmMinutos e o Número 
      # de Parâmetros, apenas foi alterada a escala do Tempo.

    
      # ALINEA B #
    
      cov(ml$TempoEmMinutos, ml$NumParametros)
      # Covariância = 6.164432
      # A covariância continua positiva, logo concluimos que existe relação 
      # linear positiva entre as duas variáveis, no entanto o valor é diferente 
      # do apresentado anteriormente, correspondendo ao (valor antigo)/60. 
      # Ou seja: 369.8659/60 = 6.164432
    
    
      # ALINEA C #
    
      cor(ml$TempoEmMinutos, ml$NumParametros)
      # Coeficiente de correlação de Pearson = 0.993836
      # O valor do coeficiente não se alterou, o que indica que o coeficiente de 
      # correlação não foi afetado pela mudança para o Tempo em minutos.



### EXERCÍCIO 1.3 ###

library(readxl)
rdh2324 <- read_excel("Correlação/RDH2324.xlsx")
str(rdh2324)
head(rdh2324)
summary(rdh2324)



  ## ALINEA A ##
  # Quais os índices que lhe parecem estar mais correlacionados? Confirme o seu 
  # palpite calculando os respetivos coeficientes de correlação.
  
  library(dplyr)
  cor(rdh2324 %>% select(where(is.numeric)))
  #           IDH         IDHAjD      IDG         IDiG
  # IDHAjD    0.9250235
  # IDG       -0.3901594  -0.3088538
  # IDiG      -0.7762524  -0.6696901  0.1672332
  # IDHAjPP   0.5251351   0.4595531   -0.3998648  -0.3807619
  
  # Assim, os indices que estão mais correlacionados são:
  #   - IDH e IDHAjD
  #   - IDH e IDiG
  # Pois são os que apresentam maior valor de módulo nos coeficientes de 
  # correlação de Pearson, no entanto, IDH e IDHAjD apresentam valor positivo,
  # o que indica a existência de uma relação linear positiva, enquanto IDH e
  # IDiG apresentam valor negativo, o que indica uma relação linear negativa.
  
  
  
  ## ALINEA B ##
  # Se recorresse ao coeficiente de Spearman em vez do coeficiente de Pearson, 
  # parece-lhe que a avaliação da associação entre as variáveis se iria alterar 
  # substancialmente? Justifique. 
  
  library(dplyr)
  cor(rdh2324 %>% select(where(is.numeric)), method="spearman")
  #           IDH         IDHAjD      IDG         IDiG
  # IDHAjD    0.8711797   
  # IDG       -0.4649136  -0.3415865
  # IDiG      -0.8056830  -0.6725718  0.2692132
  # IDHAjPP   0.5746178   0.4007949   -0.4561860  -0.4804403
  
  # Assim, agora a partir do método de Spearman, os indices que estão mais
  # correlacionados continuam a ser:
  #   - IDH e IDHAjD
  #   - IDH e IDiG
  # Portanto, a avaliação da associação entre as variáveis não se iria alterar 
  # substancialmente.


  
  
  
#### REGRESSÃO LINEAR SIMPLES ####
### EXERCÍCIO 2.1 ###
  
N_remates <- c(19, 15, 12, 16, 11, 28, 22, 8, 16)
N_cruzamentos <- c(45, 25, 22, 32, 28, 56, 38, 10, 32)
# Número de remates depende do número de cruzamentos efetuados por jogo, logo
# o número de remates é a variável resposta (y) e o número de cruzamentos é a
# variável explicativa (x).
remates <- data.frame(N_cruzamentos, N_remates)
# Inserido N_cruzamentos primeiro para ser assumido como x no data.frame e 
# depois N_remates para ser assumido como y.
  
  

  ## ALÍNEA A ##
  # Através da análise gráfica, considera que existe relação linear entre o 
  # número de remates à e o número de cruzamentos? Justifique.

  plot(remates)
  # ou
  plot(remates$N_cruzamentos, remates$N_remates)
  # Analisando visualmente o gráfico entre N_remates e N_cruzamentos observamos
  # as observações apresentam um padrão aproximadamente linear, então podemos
  # considerar a existência de uma relação linear.
  
  
  
  ## ALÍNEA B ##
  # Com base na nuvem de pontos, sugira um valor para o coeficiente de 
  # correlação linear entre o número de remates à e o número de cruzamentos. 
  # Confirme o seu palpite calculando o valor do coeficiente e comente o valor 
  # obtido.
  
  cor(remates$N_remates, remates$N_cruzamentos)
  # Coeficiente de correlação linear de Pearson = 0.9346111
  # Foi então obtido um valor bastante alto, aproximadamente 0.93, o que indica
  # uma correlação muito forte entre as variáveis N_remates e N_cruzamentos.
  
  
  
  ## ALÍNEA C ##
  # Qual a equação da reta de regressão ajustada pelo método dos mínimos 
  # quadrados?
  
  (reta <- lm(N_remates ~ N_cruzamentos, remates))
  # ŷ = valores estimados/ajustados de y a partir da reta
  # ŷ = b0 + b1 * x
  # Ou seja,
  # ŷ = 2.7725 + 0.4238 * x
  # Quando não existem cruzamentos em um jogo, são esperados que aconteçam, 
  # em média, 2.7725 remates.
  # A cada cruzamento efetuado, o número de remates esperado aumenta em 
  # aproximadamente 0.4238.
  
  
  
  ## ALÍNEA D ##
  # Interprete os coeficientes de regressão estimados.
  
  # Quando não existem cruzamentos em um jogo, são esperados que aconteçam, 
  # em média, 2.7725 remates.
  # A cada cruzamento efetuado, o número de remates esperado aumenta em 
  # aproximadamente 0.4238.
  
  
  
  ## ALÍNEA E ##
  # Represente a reta de regressão ajustada em cima da nuvem de pontos e comente.
  
  plot(remates$N_cruzamentos, remates$N_remates)
  abline(reta)
  # Verificamos que a reta apresenta um bom ajuste preditivo.
  
  
  
  ## ALÍNEA F ##
  # Calcule a Soma dos Quadrados Totais (𝑆𝑄𝑇), a partir do cálculo da 
  # variância amostral de 𝑦.
  
  dim(remates)
  n <- dim(remates)[1]
  # s² = SOMATÓRIO( (yi - ỹ)²) / (n-1)
  (SQT <- var(remates$N_remates)*(n-1))
  # SQT = 294
  
  
  
  ## ALÍNEA G ##
  # Calcule a Soma dos Quadrados da Regressão (𝑆𝑄𝑅), a partir do cálculo da 
  # variância amostral dos valores estimados para 𝑦.
  
  estimados <- fitted(reta)
  (SQR <- var(estimados)*(n-1))
  # SQR = 256.8084
  
  
  
  ## ALÍNEA H ##
  # Calcule a Soma dos Quadrados dos Resíduos (𝑆𝑄𝑅𝐸), a partir do cálculo 
  # da variância amostral dos resíduos. 
  
  # Resíduos = e
  # ei = yi - ŷi
  residuos <- residuals(reta)
  (SQRE <- var(residuos)*(n-1))
  # SQRE = 37.19161
  
  
  
  ## ALÍNEA I ##
  # Verifique numericamente a relação: 𝑆𝑄𝑇 = 𝑆𝑄𝑅 + 𝑆𝑄𝑅𝐸.
  
  # SQT = SQR + SQRE
  SQR + SQRE
  # = 294
  SQT
  # = 294
  # Então, foi provada a expressão mencionada no enunciado.
  
  
  
  ## ALÍNEA J ##
  # Analise a qualidade estatística do modelo estimado.
  
  (R_Squared <- SQR/SQT)
  # R-Squared = 0.8734979
  
  #ou
  summary(reta)$r.squared
  # R-Squared = 0.8734979
  # Assim, com valor de R-Squared de aproximadamente 0.87, admitimos que o 
  # modelo apresenta um bom ajuste, explicando aproximadamente 87% da 
  # variabilidade
  
  
  #   R-Squared       Interpretação
  #   [0; 0.2[        Ajuste Muito Fraco
  #   [0.2; 0.5[      Ajuste Fraco
  #   [0.5; 0.7[      Ajuste Aceitável
  #   [0.7; 0.9[      Ajuste Bom
  #   [0.9; 1]        Ajuste Excelente
  
  
  
### EXERCÍCIO 2.5 ###
  
  library(readxl)
  vendas <- read_excel("Regressão linear simples/vendas.xlsx")
  
  
  
  ## ALÍNEA A ##
  # Estime a reta de regressão linear, pelo método dos mínimos quadrados. 
  # Interprete os coeficientes de regressão.
  
  names(vendas)
  names(vendas) <- c("y", "x")
  (reta <- lm(y ~ x, vendas))
  # ŷ = valores estimados/ajustados de y a partir da reta
  # ŷ = b0 + b1 * x
  # Ou seja,
  # ŷ = 2813 - 1578 * x
  # Quando o preço do medicamento é 0, são esperadas, em média, 2813 vendas do 
  # medicamento.
  # A cada euro aumentado no preço do medicamento, é esperado que o número de 
  # vendas diminua em 1578 unidades.
  
  
  
  ## ALÍNEA B ##
  # Represente graficamente a nuvem de pontos e a reta ajustada.

  plot(vendas$x,vendas$y)
  abline(reta)
  
  
  
  ## ALÍNEA C ##
  # Obtenha os resíduos de estimação.
  
  residuals(reta)
  rstudent(reta)
  rstandard(reta)  
  
  
  
  ## ALÍNEA D ##
  # Valide os pressupostos subjacentes ao modelo de regressão linear.
  
  # Pressupostos sobre os Resíduos:
  # - Linearidade entre x e y;
  # - Independência dos Resíduos;
  # - Resíduos com Média Nula;
  # - Homocedasticidade dos Resíduos;
  # - Normalidade dos Resíduos.
  
  residuos <- residuals(reta)
  residuos_standard <- rstandard(reta)
  estimados <- fitted(reta)
  
  # Linearidade entre x e y
  plot(residuos ~ estimados)
  # A nuvem de pontos não apresenta qualquer curva, logo admitimos linearidade e 
  # verificamos o pressuposto.
  
  # Independência dos Resíduos
  plot(residuos)
  # O gráfico não apresenta qualquer padrão, estando os pontos distribuídos 
  # aleatoriamente, então admitimos a independência e verificamos o pressuposto.
  
  # Resíduos com Média Nula
  mean(residuos)
  # Média = 8.147418e-16  Aproximadamente 0
  # Assim, a média dos resíduos é aproximadamente 0 e verificamos o pressuposto.
  
  # Homocedasticidade dos Resíduos
  plot(residuos ~ estimados)
  # O gráfico não apresenta afunilamento na nuvem de pontos por isso verificamos
  # o pressuposto da Homocedasticidade.
  
  # Normalidade dos Resíduos
  qqPlot(residuos_standard)
  # Não existem outliers significativos fora das bandas de confiança por isso 
  # podemos admitir a normalidade dos resíduos e verificar o pressuposto pela 
  # análise gráfica.
  shapiro.test(residuos_standard)
  # H0: Resíduos são Normais.
  # H1: Resíduos não são Normais.

  # Valor p = 0.538 > alfa
  # Logo, admitimos H0, admitimos que os Resíduos são Normais e verificamos o
  # pressuposto.
  
  # Analise de residuos
  plot(reta, which = 5)  # Distância de Cook, Leverage e residuos
  plot(reta, which = 4)  # Só distância de Cook
  # Não apresentando um valor alarmante para a distância de Cook, estando todos
  # abaixo de 0.35, admitimos que não há outliers preocupantes para a reta.
  
  
  
  ## ALÍNEA E ##
  # Determine o coeficiente de correlação e interprete o valor obtido.
  
  cor(vendas$x, vendas$y)
  # Coeficiente de correlação de Pearson = -0.9598046
  # O coeficiente é negativo o que indica uma relação linear negativa e o seu 
  # valor em módulo é aproximadamente 0.96, o que indica uma correlação
  # muito forte.
  
  
  
  ## ALÍNEA F ##
  # Estime a variância do erro do modelo.
  
  (n <- dim(vendas)[1])
  (SQRE <- var(residuos)*(n-1))
  # SQRE = 26437.23
  (RQME <- SQRE/(n-2))
  # RQME = 2643.723
  
  # ou
  summary(reta)
  (variancia_erro <- 51.42^2)
  
  # Assim, a variância do erro é aproximadamente 2644.
  
  
  
  ## ALÍNEA G ##
  # Construa a tabela ANOVA.
  anova(reta)
  
  
  
  ## ALÍNEA H ##
  # Construa um intervalo de confiança a 99% para β0.
  
  confint(reta, level=0.99)
  # IC99% (β0) = [2257.671; 3368.969]
  
  
  
  ## ALÍNEA I ##
  # Complete: “Com 95% de confiança o verdadeiro valor de β1 situa-se 
  # entre ... e ...”.
  
  confint(reta)
  # IC95% (β1) = [-1902.628; -1252.534]
  # Com 95% de confiança o verdadeiro valor de β1 situa-se entre -1902.628 
  # e -1252.534 .
  
  
  
  ## ALÍNEA J ##
  # A partir de que nível de significância é rejeitada a hipótese do coeficiente 
  # β0 ser nulo?
  
  summary(reta)
  # H0: β0 = 0.
  # H1: β0 /= 0.
  
  # Valor p (β0) = 1.83e-08
  # Assim, a hipótese do coeficiente β0 ser nulo é rejeitada para quaisquer 
  # níveis de significância superiores a 1.83e-08 .
  
  
  
  ## ALÍNEA K ##
  # Ensaie a hipótese de que o preço não influência linearmente o número de 
  # embalagens vendidas (considere alfa = 1%).
  
  summary(reta)
  # H0: β1 = 0.
  # H1: β1 /= 0.
  
  # Valor p (β1) = 7.72e-07 < 0.01 = alfa
  # Logo, rejeitamos H0, a hipótese de que β1 é nulo, e admitimos que o preço 
  # influencia linearmente o número de embalagens vendidas.
  
  
  
  ## ALÍNEA L ##
  # Determine e interprete o coeficiente de determinação.
  
  summary(reta)$r.squared
  # R-Squared = 0.9212248
  # O valor de R-Squared é aproximadamente 0.92 o que indica um ajuste 
  # excelente da reta.
  
  
  
  ## ALÍNEA M ##
  # Estime o número esperado de embalagens vendidas quando o preço de cada 
  # embalagem é 1,23 euros. Construa um intervalo de confiança a 95% para esse 
  # valor esperado.
  
  predict(reta, new = data.frame(x=1.23), int="conf", level = 0.95)
  # fit      lwr      upr
  # 872.8953 838.1771 907.6135
  
  # ŷ (1.23) = 872.8953   Aproximadamente 873.
  # IC95% = [838.1771; 907.61350]
  
  # Quando cada embalagem é 1.23 euros, esperamos vender aproximadamente 
  # 873 embalagens com um intervalo de confiança situado entre 838.1771 
  # e 907.6135 .
  
  
  
  ## ALÍNEA N ##
  # Construa um intervalo de predição (95%) associado ao número de embalagens 
  # quando o preço é 1,23 euros. Compare com o intervalo de confiança obtido na 
  # alínea anterior e comente.
  
  predict(reta, new = data.frame(x=1.23), int="pred", level = 0.95)
  # fit      lwr      upr
  # 872.8953 753.1857 992.6049
  
  # ŷ (1.23) = 872.8953   Aproximadamente 873.
  # IC95% = [753.1857; 992.6049]
  
  # Quando cada embalagem é 1.23 euros, esperamos vender aproximadamente 
  # 873 embalagens com um intervalo de predição situado entre 753.1857 
  # e 992.6049 .
  
  # O intervalo de predição é mais amplo porque incorpora tanto a incerteza na 
  # estimativa da média como a variabilidade natural dos dados.
  
  
  
### EXERCÍCIO 2.8 ###
  
  library(MASS)
  Animais <- Animals
  names(Animais)
  
  
  
  ## ALÍNEA A ##
  # Desenhe a nuvem de pontos e comente.
  
  plot(Animais$brain ~ Animais$body)
  # O gráfico não é linear e temos várias outliers significantes que 
  # certamente serão influentes.
  
  
  
  ## ALÍNEA B ##
  # Calcule o coeficiente de correlação correspondente e comente.
  
  cor(Animais$body, Animais$brain)
  # Coeficiente de correlação de Pearson = -0.005341163
  # O coeficiente é negativo logo representa uma regressão linear negativa e o
  # seu valor em módulo é aproximadamente 0.005, indicando uma correlação não 
  # existente ou desprezável.
  
  
  
  ## ALÍNEA C ##
  # Construa nuvens de pontos com as seguintes transformações de uma ou ambas as
  # variáveis:
  
      # PONTO I) #
      # ln(y) vs. x;
  
      plot(log(Animais$brain) ~ Animais$body)
      # Esta transformação continua não linear e com pontos influentes.
  
  
      # PONTO II) #
      # y vs. ln(x)
  
      plot(Animais$brain ~ log(Animais$body))
      # Esta transformação continua não linear e com pontos influentes.
      
  
      # PONTO III) #
      #ln(y) vs. ln(x)
      
      plot(log(Animais$brain) ~ log(Animais$body))
      # Encontramos então uma transformação que é linear porém ainda com alguns 
      # pontos possivelmente influentes.
      
      
      
  ## ALÍNEA D ##
  # Considere uma relação linear entre ln(y) e ln(x). Explicite a relação de 
  # base correspondente entre as variáveis originais (não logaritmizadas). 
  # Comente.
      
  # brain = a * body^(b)
  # Sendo a = e^(a)
  # Esta é uma relação potencial, ou seja, o peso do cérebro é proporcional a 
  # uma potência do peso do corpo.
  # Se b = 1 o cérebro cresce proporcionalmente ao corpo.
  # se b < 1 o crescimento do cérebro é mais lento que do corpo.
  # Se b > 1 o crescimento do cérebro é mais rápido que do corpo.
      
  
      
  ## ALÍNEA E ##    
  # Nas próximas alíneas, considere sempre os dados logaritmizados, i.e, ln(y) 
  # e ln(x).
  
      # PONTO I) #
      # Calcule os coeficientes de correlação e de determinação associados à 
      # relação entre ln(y) e ln(x). Interprete os valores obtidos. Como se 
      # explica que o Coeficiente de Determinação não seja particularmente 
      # elevado, sendo evidente a partir da nuvem de pontos que existe uma boa 
      # relação linear entre log-peso do corpo e log-peso do cérebro para a 
      # generalidade das espécies?
      
      cor(log(Animais$body), log(Animais$brain))
      # Coeficiente de Correlação de Pearson = 0.7794935
      # O coeficiente é positivo o que indica uma regressão linear positiva e o
      # seu valor em módulo é aproximadamente 0.78 o que indica uma correlação
      # forte entre as variáveis.
      
      (cor(log(Animais$body), log(Animais$brain)))^2
      # R-Squared = 0.6076101
      # Assim, com os dados logaritmizados, obtemos um R-Squared aceitável de 
      # aproximadamente 0.61, ou seja, aproximadamente 61% da variabilidade é
      # explicada.
      
      # Obtemos um R-Squared relativamente baixo apesar de uma visualização
      # gráfica quase linear pois existem 3 pontos influentes que podem estar 
      # a degradar a qualidade do ajuste.
    
      
      
  ## ALÍNEA F ##
  # Ajuste a reta de regressão de log-peso do cérebro sobre log-peso do corpo 
  # (utilizando a totalidade das observações). Interprete o valor obtido para o 
  # declive da reta, do ponto de vista biológico, quer na relação entre 
  # variáveis logaritmizadas, quer na relação entre as variáveis originais (não 
  # logaritmizadas). 
  
  (reta <- lm(log(brain) ~ log(body), data = Animais))
  # ŷ = 2.555 + 0.496 * log(body)
  # Neste cenário não faz sentido considerar body = 0, ou seja, um animal cujo 
  # peso é igual a 0 Kg.
  # Cada unidade aumentada em log(body), irá aumentar, em média, 0.496 em 
  # log(brain).
  
  # Agora nas variáveis originais:
  # ŷ = e^(2.555) + body^(0.496)
  exp(2.555) # = 12.8713
  # Assim, ŷ = 12.87 + body^(0.496)
  # Mais uma vez, neste cenário não sentido considerar body = 0, ou seja, um
  # animal cujo peso é igual a 0 Kg.
  # Cada aumento de 1% no peso do corpo leva a um aumento de 0.496% do peso do
  # cérebro do animal.
  
  
  
  ## ALÍNEA G ##
  # Considere a nuvem de pontos das variáveis logaritmizadas. Com auxílio da 
  # função identify do R, identifique os três pontos que se destacam na parte 
  # inferior direita da nuvem.
  
  plot(log(Animais$brain) ~ log(Animais$body))
  identify(log(Animais$body), log(Animais$brain), tolerance = 1)
  # Assim, conseguimos identificar os pontos 6, 16 e 26 como sendo os outliers
  # observados anteriormente.
  
  
  
  ## ALÍNEA H ##
  # Ajuste a reta de regressão de log-peso do cérebro sobre log-peso do corpo 
  # considerando os dados (logaritmizados) sem as 3 observações identificadas 
  # em g). Obtenha o valor do coeficiente de determinação.
  
  (reta2 <- lm(log(brain) ~ log(body), data = Animais[-c(6, 16, 26),]))
  # ŷ = 2.1504 + 0.7523 * log(body)
  
  # Agora nas variáveis originais:
  # ŷ = e^(2.1504) + body^(0.7523)
  exp(2.1504) # = 8.588293
  # Assim, ŷ = 8.588293 + body^(0.7523)
  
  summary(reta2)
  # R-Squared = 0.9217
  # Assim, o ajuste melhorou bastante, apresentado agora valor de R-Squared 
  # excelente de aproximadamente 0.92, ou seja, explica aproximadamente 92% da 
  # variabilidade.
  
  
  
  ## ALÍNEA I ##
  # Represente na nuvem de pontos das variáveis logaritmizadas os modelos 
  # obtidos em f) e h). Comente, sem esquecer de comentar a diferença 
  # considerável nos coeficientes de determinação.
  
  plot(log(Animais$brain) ~ log(Animais$body))
  abline(reta, col="blue")
  abline(reta2, col="green")
  # Assim, observamos imediatamente a diferença de declives das duas retas, uma 
  # com os outliers incluídos (reta azul) e outra sem eles (reta verde), estando 
  # esta muito melhor ajustada á nuvem de pontos, daí aumentar bastante o 
  # R-Squared quando comparado com a reta que inclui os outliers.
  
  
  
  
  
#### REGRESSÃO LINEAR MÚLTIPLA ####
### EXERCÍCIO 3.1 ###
  
vending <- read.csv2("Regressão linear múltipla/vending.csv", stringsAsFactors = T, na.strings = c("NULL","", "NA"), 
                        sep=";", dec=",", header=T, encoding = "UTF-8")
summary(vending)



  ## ALÍNEA A ##
  # Desenhe as nuvens de pontos para cada par das 3 variáveis observadas. 
  # Comente.
  
  plot(tempo ~ caixas, data = vending)
  plot(tempo ~ distancia, data = vending)
  plot(caixas ~ distancia, data = vending)
  
  # ou
  plot(vending[,-1])
  # Após análise gráfica, verificamos que todos os pares de variáveis apresentam
  # uma relação aproximadamente linear, mais importante, que o tempo (y) está
  # linearmente relacionado com caixas (x0) e distancia (x1).
  
  
  
  ## ALÍNEA B ##
  # Calcule a matriz de correlações entre todos os pares de variáveis 
  # observadas. Comente.
  
  cor(vending[,-1])
  #             tempo       caixas
  # caixas      0.9219760
  # distancia   0.8755203   0.692529
  
  # Agora por observação da matriz de correlações podemos retirar as seguintes
  # conclusões:
  # - Existe correlação muito forte e positiva (0.92) entre tempo e caixas;
  # - Existe correlação forte e positiva (0.88) entre tempo e distância;
  # - Existe correlação moderada e positiva (0.69) entre caixas e distância.
  
  
  
  ## ALÍENA C ##
  # Ajuste o modelo de regressão linear múltipla resultante de modelar o tempo 
  # com base nos dois preditores disponíveis. Interprete os coeficientes de 
  # regressão.
  
  (modelo <- lm(tempo ~ caixas + distancia, data = vending))
  # ŷ = tempo estimado
  # ŷ = 3.1442 + 1.4217*caixas + 0.0197*distancia
  # Quando foram reabastecidas 0 caixas e a distância percorrida a pé pelo 
  # motorista foi 0 metros, espera-se que o tempo na prestação do serviço
  # seja aproximadamente 3.1442 minutos.
  # A cada caixa de produtos reabastecida, espera-se que aumente o tempo na 
  # prestação do serviço em aproximadamente 1.4217 minutos.
  # A cada metro de distância percorrida pelo motorista a pé, espera-se que
  # aumente o tempo na prestação do serviço em aproximadamente 0.0197 minutos.
  
  
  
  ## ALÍNEA D ##
  # Teste, ao nível de significância de 5%, a hipótese de que os coeficientes 
  # de regressão das variáveis explicativas são conjuntamente nulos.
  
  summary(modelo)
  # H0: β1 = β2 = 0.
  # H1: Pelo menos um dos coeficientes /= 0.
  
  # E.T. F = MQR/MQRE ~ F p; n-(p+1)
  
  # Valor p (F-statistic) = 7.181e-16 < 0.001
  # Logo, rejeitamos H0 e admitimos que pelo menos um dos coeficientes é 
  # diferente de 0.
  
  
  
  ## ALÍNEA E ##
  # Para cada variável explicativa, teste, ao nível de significância de 5%, a 
  # hipótese de que o respetivo coeficiente de regressão é nulo.
  
  summary(modelo)
  # H0: β1 = 0.
  # H1: β1 /= 0.
  
  # Valor p (β1) = 1.18e-09 < 0.001
  # Logo, rejeitamos H0 e admitimos que o coeficiente de caixas é diferente 
  # de 0 e contribui significativamente para explicar tempo.
  
  # H0: β2 = 0.
  # H1: β2 /= 0.
  
  # Valor p (β2) = 1.62e-07 < 0.001
  # Logo, rejeitamos H0 e admitimos que o coeficiente de distancia é diferente 
  # de 0 e contribui significativamente para explicar tempo.
  
  
  
  ## ALÍNEA F ##
  # Construa intervalos de confiança a 90% para cada um dos coeficientes de 
  # regressão.
  
  confint(modelo, level = 0.9)
  #                      5 %          95 %
  # (Intercept)   1.15721505    5.13119203
  # caixas        1.17793648    1.66551700
  # distancia     0.01520498    0.02420441
  
  # IC90% (β0) = [1.157; 5.131]
  # IC90% (β1) = [1.178; 1.666]
  # IC90% (β0) = [0.015; 0.024]
  
  
  
  ## ALÍNEA G ##
  # Qual o valor estimado para o tempo necessário na prestação de serviço à 
  # máquina nas seguintes situações:
  #         Número de caixas          3     10      20
  #         Distância (em metros)   100    500    1000
  
  predict(modelo, new = data.frame(caixas = c(3, 10, 20), distancia = c(100, 500, 1000)), level = 0.95)
  # ŷ (3, 100) = 9.379853 
  # Para um número de caixas igual a 3 e distância (em metros) igual 100, 
  # espera-se que o tempo na prestação do serviço seja aproximadamente 
  # 9.38 minutos.

  # ŷ (10, 500) = 27.213818  
  # Para um número de caixas igual a 10 e distância (em metros) igual 500, 
  # espera-se que o tempo na prestação do serviço seja aproximadamente 
  # 27.21 minutos.
  
  # ŷ (20, 1000) = 51.283433 
  # Para um número de caixas igual a 20 e distância (em metros) igual 1000, 
  # espera-se que o tempo na prestação do serviço seja aproximadamente 
  # 51.28 minutos.
  
  
  
  ## ALÍNEA H ##
  # Com base em cada um dos pares de observações anteriores, obtenha:
  
      # PONTO I) #
      # Um intervalo de confiança (95%) para o valor esperado para o tempo 
      # necessário na prestação de serviço associado a esses valores das 
      # variáveis preditoras.
      
      predict(modelo, new = data.frame(caixas = c(3, 10, 20), distancia = c(100, 500, 1000)), int = "conf", level = 0.95)
      # fit       lwr       upr
      # 9.379853  7.448819  11.31089
      # 27.213818 25.822374 28.60526
      # 51.283433 48.445076 54.12179
      
      # ŷ (3, 100) = 9.379853
      # IC95% (ŷ (3, 100)) = [7.449; 11.311]
      
      # ŷ (10, 500) = 27.213818
      # IC95% (ŷ (10, 500)) = [25.822; 28.605]
      
      # ŷ (20, 1000) = 51.283433
      # IC95% (ŷ (20, 1000)) = [48.445; 54.122]
      
      
      # PONTO II) #
      # Um intervalo de predição (95%) para o tempo necessário na prestação de 
      # serviço individual.
      predict(modelo, new = data.frame(caixas = c(3, 10, 20), distancia = c(100, 500, 1000)), int = "pred", level = 0.95)
      # fit       lwr       upr
      # 9.379853  2.38454   16.37517
      # 27.213818 20.34784  34.07979
      # 51.283433 43.98537  58.58150
      
      # ŷ (3, 100) = 9.379853
      # IC95% (ŷ (3, 100)) = [2.385; 16.375]
      
      # ŷ (10, 500) = 27.213818
      # IC95% (ŷ (10, 500)) = [20.348; 34.080]
      
      # ŷ (20, 1000) = 51.283433
      # IC95% (ŷ (20, 1000)) = [43.985; 58.582]
      
      
      
  ## ALÍNEA I ##
  # Ajuste os modelos de regressão lineares simples para modelar o tempo com 
  # base no número de caixas, e o tempo com base na distância. Compare os 
  # coeficientes destes modelos com os obtidos com o modelo de regressão linear
  # múltipla.
      
  # ŷ = 3.1442 + 1.4217*caixas + 0.0197*distancia
  # R-Squared = 0.958
      
  (modelo1 <- lm(tempo ~ caixas, data = vending))
  # ŷ = 4.462 + 2.161*caixas
  summary(modelo1)
  # R-Squared = 0.85
      
  (modelo2 <- lm(tempo ~ distancia, data = vending))
  # ŷ = 8.73543 + 0.03788*distancia
  summary(modelo2)
  # R-Squared = 0.7665
      
  # Verificamos que em ambos os modelos simples o coeficiente é maior pois com a
  # falta de explicabilidade, existindo apenas uma variável explicativa, o 
  # modelo tenta compensar ajustando os coeficientes do intercepto e da variável
  # explicativa.
  # Verificamos também que os modelos de regressão lineares simples o valor de 
  # R-Squared é inferior, o que seria o esperado, no entanto, caso isso não se
  # verificasse, o modelo final deveria ser repensado.
  
  
  
  ## ALÍNEA J ##
  # No modelo de regressão múltipla ajustado em c), qual a variável com maior 
  # contribuição relativa para o modelo?

  (modelo.beta <- lm(data.frame(scale(modelo$model))))  # Betas Estandardizados 
  # ŷ = -2.743e-16 + 6.066e-01*caixas + 4.555e-01*distancia
  
  # Assim, a variável caixas tem o maior coeficiente padronizado (0.6066), o 
  # que indica que é a variável com maior contribuição relativa para explicar a 
  # variável resposta tempo, em comparação com distancia (0.4555).
  
  confint(modelo.beta)
  #                    2.5 %      97.5 %
  # (Intercept)  -0.08878727  0.08878727
  # caixas        0.48093782  0.73216980
  # distancia     0.32984823  0.58108021
  
  # IC95% (Intercepto) = [-0.089; 0.089]
  # IC95% (caixas) = [0.481; 0.732]
  # IC95% (distanica) = [0.330; 0.581]

  
  
### EXERCÍCIO 3.2 ###

library(datarium)
mrk <- marketing
summary(mrk)



  ## ALÍNEA A ##
  # Ajuste o modelo de regressão linear considerando apenas os efeitos 
  # principais.

  (modelo <- lm(sales ~ youtube + facebook + newspaper, data = mrk))
  # ŷ = 3.526667 + 0.045765*youtube + 0.188530*facebook - 0.001037*newspaper
  # Quando o montante gasto nos 3 medias é igual a 0, espera-se o montante de 
  # vendas seja aproximadamente 3.527 .
  # Cada unidade gasta em anúncios no Youtube aumenta em aproximadamente 0.046 o
  # montante das vendas.
  # Cada unidade gasta em anúncios no Facebook aumenta em aproximadamente 0.189
  # o montante das vendas.
  # Cada unidade gasta em anúncios nos Newspaper diminui em aproximadamente 
  # 0.001 o montante das vendas.



  ## ALÍNEA B ##
  # Teste a significância do modelo geral.

  summary(modelo)
  # H0: β1 = β2 = β3 = 0.
  # H1: Pelo menos um dos coeficientes /= 0.

  # Valor p (F-statistic) < 2.2e-16 < 0.001
  # Logo, rejeitamos H0 e admitimos que pelo menos um dos coeficientes é 
  # diferente de 0.
  
  
  
  ## ALÍNEA C ##
  # Teste a significância de cada um dos coeficientes.
  
  summary(modelo)
  # H0: β1 = 0.
  # H1: β1 /= 0.
  
  # Valor p (β1) < 2e-16 < 0.001
  # Logo, rejeitamos H0 e admitimos que o coeficiente do youtube é diferente 
  # de 0 e contribui significativamente para explicar sales.
  
  # H0: β2 = 0.
  # H1: β2 /= 0.
  
  # Valor p (β2) < 2e-16 < 0.001
  # Logo, rejeitamos H0 e admitimos que o coeficiente do facebook é diferente 
  # de 0 e contribui significativamente para explicar sales.
  
  # H0: β3 = 0.
  # H1: β3 /= 0.
  
  # Valor p (β3) = 0.86 > alfa
  # Logo, não rejeitamos H0 e admitimos que o coeficiente dos newspaper
  # são iguais a 0 e não contribuem significativamente para explicar sales, 
  # então deve ser removido do modelo.
  
  
  
  ## ALÍNEA D ##
  # Com base nos resultados anteriores, ajuste o modelo que lhe parecer mais 
  # adequado.
  
  (modelo <- update(modelo, ~.-newspaper))
  # ŷ = 3.50532 + 0.04575*youtube + 0.18799*facebook
  # Quando o montante gasto nos 2 medias é igual a 0, espera-se o montante de 
  # vendas seja aproximadamente 3.505 .
  # Cada unidade gasta em anúncios no Youtube aumenta em aproximadamente 0.046 o
  # montante das vendas.
  # Cada unidade gasta em anúncios no Facebook aumenta em aproximadamente 0.188
  # o montante das vendas.
  
  
  
  ## ALÍNEA E ##
  # Verifique se obteria um modelo diferente, caso tivesse usado os métodos 
  # sequenciais: forward, backward e stepwise.
  
  # Forward:
  (mod0 <- lm(sales ~ 1, data = mrk))
  # ŷ = 16.83
  
  add1(mod0, scope=~youtube + facebook + newspaper, test="F")
  #             AIC     F value   Pr(>F)  
  # youtube     547.44  312.145   < 2.2e-16
  # facebook    656.03  98.422    < 2.2e-16
  # newspaper   726.02  10.887    0.001148
  
  # Começamos então por adicionar a mais significativa, neste caso o youtube, 
  # tendo o menor AIC e maior F value, aqui não podemos recorrer apenas ao
  # valor p pois o youtube e o facebook têm o mesmo valor.
  (mod0 <- update(mod0, ~.+youtube))
  # ŷ = 8.43911 + 0.04754*youtube
  
  add1(mod0, scope=~.+facebook + newspaper, test="F")
  #             AIC     F value   Pr(>F)    
  # facebook    283.75  546.74    < 2.2e-16 ***
  # newspaper   531.13  18.89     2.217e-05 ***
  
  # Adicionamos agora facebook pois é a mais significativa, com menor valor p, 
  # menor AIC e maior valor F.
  (mod0 <- update(mod0, ~.+facebook))
  # ŷ = 3.50532 + 0.04575*youtube + 0.18799*facebook
  
  add1(mod0, scope=~.+newspaper, test="F")
  #             AIC     F value   Pr(>F)           
  # newspaper   285.71  0.0312    0.8599
  
  # Assim, newspaper não é significativo, então não será adicionado ao modelo e
  # ficamos então com o seguinte modelo final pela estratégia forward:
  # ŷ = 3.50532 + 0.04575*youtube + 0.18799*facebook
  
  
  # Backward:
  (mod0 <- lm(sales ~ youtube + facebook + newspaper, data = mrk))
  # ŷ = 3.526667 + 0.045765*youtube + 0.188530*facebook - 0.001037*newspaper
  
  drop1(mod0, test="F")
  #             AIC     F value   Pr(>F)    
  # youtube     657.83  1076.4058 <2e-16 ***
  # facebook    531.13  479.3252  <2e-16 ***
  # newspaper   283.75  0.0312    0.8599
  
  # Começamos então por remover newspaper, sendo esta não significativa.
  (mod0 <- update(mod0, ~.-newspaper))
  # ŷ = 3.50532 + 0.04575*youtube + 0.18799*facebook
  
  drop1(mod0, test="F")
  #             AIC     F value   Pr(>F)    
  # youtube     656.03  1082.98   < 2.2e-16 ***
  # facebook    547.44  546.74    < 2.2e-16 ***
  
  # Assim, todas as variáveis explicativas são significativas, então ficamos com
  # o seguinte modelo final pela estratégia backward:
  # ŷ = 3.50532 + 0.04575*youtube + 0.18799*facebook
  
  
  # Stepwise:
  # Poucas variáveis para testar com estes dados.
  
  # 1. Modelo Nulo
  # 2. Incluir as 2 primeiras variaveis pelo método forward
  # 3. Usar método backward para avaliar a exclusão de alguma variável
  # 4. Usar método forward para avaliar a adição de alguma variável
  # 5. Repetir os passos 3 e 4 até não incluir nem excluir variáveis.
  
  
  
  ## ALÍNEA F ##
  # Considere o modelo ajustado em ALÍNEA D.
  
      # PONTO I) #
      # Obtenha intervalos de confiança a 95% para os coeficientes e interprete.
      
      confint(modelo)
      #             2.5 %       97.5 %
      # (Intercept) 2.80841159  4.20222820
      # youtube     0.04301292  0.04849671
      # facebook    0.17213877  0.20384969
      
      # IC95% (Intercepto) = [2.808; 4.202]
      # IC95% (youtube) = [0.043; 0.048]
      # IC95% (facebook) = [0.172; 0.204]
      
      
      # PONTO II) #
      # Qual a variável com maior contribuição relativa para o modelo?
      
      (modelo.beta <- lm(data.frame(scale(modelo$model))))  # Betas Estandardizados 
      # ŷ = 4.951e-18 + 7.529e-01*youtube + 5.350e-01*facebook
      
      # Assim, a variável youtube tem o maior coeficiente padronizado (0.7529), 
      # o que indica que é a variável com maior contribuição relativa para 
      # explicar a variável resposta sales, em comparação com facebook (0.5350).
      
      
      # PONTO III) #
      # Qual o valor previsto para as vendas se forem gastos 100 milhares de 
      # dólares em anúncios no youtube, 50 milhares de dólares no Facebook e 80 
      # milhares de dólares em jornais?
      
      predict(modelo, new = data.frame(youtube = 100, facebook = 80))
      # ŷ (100, 80) = 23.12034 
      
      
      # PONTO IV) #
      # Verifique se a interação entre os preditores facebook e youtube é 
      # significativa.
      
      mod_interacao <- lm(sales ~ youtube * facebook, data = mrk)
      summary(mod_interacao)
      # Valor p (youtube:facebook) < 2e-16 < 0.001
      # Logo, admitimos que a interação é significativa.
      
      
      # PONTO V) #
      # Valide os pressupostos do modelo.
      
      # Pressupostos e Problemas se violados:
      # 1. Independência: 
      #    Se violada -> p-values e ICs incorretos
      # 2. Resíduos normais: 
      #    Se violado -> p-values e ICs incorretos. problema com n pequeno, não 
      #    tanto com n grande (pelo menos 10 observações por preditor) 
      # 3. Linearidade: 
      #    Se violada -> estimativas enviesadas
      # 4. Homogeneidade dos Resíduos: 
      #    Se violado -> p-values e ICs incorretos mesmo com n grande
      
      (mod_final <- lm(sales ~ youtube + facebook, data = mrk))
      # ŷ = 8.1002642 + 0.0191011*youtube + 0.0288603*facebook + 
      #     + 0.0009054*youtube:facebook
      
      ri <- rstandard(mod_final)
      yi <- fitted(mod_final)
      
      n <- dim(mrk)[1]
      p <- length(mod_final$coefficients) -1
      
      # Multicolinearidade
      library(car)
      vif(mod_final)
      #  youtube facebook 
      # 1.003013 1.003013 
      
      # Ambos os VIFs estão muito próximos de 1 logo não temos problemas de 
      # multicolinearidade.
      
      # Análise Gráfica
      par(mfrow=c(2,2))
      plot(mod_final)
      # Gráfico 1 (Residuals vs Fitted)
      # - Apresenta uma curva por isso rejeitamos a Linearidade.
      # - Os valores encontram-se aproximados a y=0 por isso admitimos que a 
      #   Média dos Resíduos é aproximadamente Nula.
      # Gráfico 2 (Q-Q Residuals)
      # - Existem inúmeros outliers nas caudas o que leva à rejeição da 
      #   normalidade.
      # Gráfico 3 (Scale-Location)
      # - Não existe qualquer afunilamento por isso admitimos a 
      #   Homocedasticidade.
      # Gráfico 4 (Residuals vs Leverage)
        (hii <- 2*(p+1)/n) # = 0.03
      # - Existem pontos com Distância de Cook superior a 1.
      # - Existem pontos com Leverage superior a 0.03 .
      
      # Independência
      plot(ri)
      # O gráfico não apresenta qualquer padrão, estando os pontos distribuídos 
      # aleatoriamente, então admitimos a independência e verificamos o 
      # pressuposto.
      library(car)
      durbinWatsonTest(mod_final)
      # Valor p = 0.544 > alfa
      # Verificamos a Independência
      
      # Normalidade
      shapiro.test(ri)
      # Valor p = 3.806e-09 < 0.001
      # Rejeitamos a Normalidade
      
      # Homocedasticidade
      library(lmtest)
      lmtest::bptest(mod_final)
      # Valor p = 0.0903
      # Não rejeitamos a homocedasticidade, logo verificamos o pressuposto.
      
      # Distância de Cook
      par(mfrow=c(1,1))
      plot(mod_final, which=4)
      # Todos com Distância de Cook inferior a 0.3, sendo necessário serem 
      # inferiores a 1, então está tudo bem.
      
      # Leverage
      plot(mod_final, which = 5)
      # Existem valores com Leverage superior 0.03
      
      (LEVERAGE <- hatvalues(mod_final))    # Valores Leverage
      plot(LEVERAGE, type='h')
      maximo <- 2*(p+1)/n
      abline(h=maximo, col="red")
      
      table(LEVERAGE>maximo)
      # Existem 2 valores superiores ao máximo, logo têm de ser investigados.
      
      # DFBETAS
      (DFBETAS <- dfbetas(mod_final))  
      # Tem tantas colunas quantos os betas no modelo
      
      par(mfrow=c(3,1))
      plot(abs(DFBETAS[,1]), type="h", main="intercept")
      maximo <- 2/sqrt(n)
      abline(h=c(-maximo, maximo), col="green")
      plot(abs(DFBETAS[,2]), type='h', main="youtube")
      abline(h=c(-maximo, maximo), col="green")
      plot(abs(DFBETAS[,3]), type='h', main="facebook")
      abline(h=c(-maximo, maximo), col="green")

      table(abs(DFBETAS[,1]) > maximo)  
      # Existem 15 valores com problemas no Beta0 que têm de ser investigados.
      table(abs(DFBETAS[,2]) > maximo)  
      # Existem 20 valores com problemas no Beta1 que têm de ser investigados.
      table(abs(DFBETAS[,3]) > maximo)  
      # Existem 20 valores com problemas no Beta2 que têm de ser investigados.
      
      # DFFITS
      (DFFITS <- dffits(mod_final))
      plot(abs(DFFITS), type="h")
      maximo <- 2*sqrt((p+1)/n)  
      abline(h=maximo, col="green")
      
      table(abs(DFFITS)>maximo)  
      # Existem 16 valores com problemas que têm de ser investigados.
      
      # ALTERNATIVAS PARA OBTER ALGUMAS DAS MEDIDAS ACIMA MAIS FACILMENTE
      # Graficos com Distancia de Cook, Residuos studentizados, leverage e teste 
      # Bonferroni para avaliar se há outliers
      library(car)
      influenceIndexPlot(mod_final)  
      # Identifica o 131 como outlier
      
      # Mostra as medidas para todos os "individuos" da nossa base de dados
      library(stats)
      (influentes <- influence.measures(mod_final, infl = influence(mod_final)))
      # Mostra quais "individuos" sao possiveis observacoes influentes (se destacam em pelo menos 1 medida)?
      which(apply(influentes$is.inf, 1, any))   
      # Posição dos possíveis pontos influentes:
      # 6  36  127 131 179 
      # Para ver os valores das medidas apenas nos possiveis pontos influentes
      summary(influentes)
      
      
      
  ## ALÍNEA G ##
  # Caso o modelo ajustado em d) não satisfaça os pressupostos, tente encontrar 
  # um modelo alternativo. Experimente:
      
  # ŷ = 3.50532 + 0.04575*youtube + 0.18799*facebook
  # R-Squared = 0.8972

      # PONTO I) #
      # Transformar as variáveis (resposta, preditoras ou ambas).
      
      # Transformar variável resposta:
      (mod1 <- lm(log(sales) ~ youtube + facebook, data = mrk))
      # ŷ = 1.927400 + 0.003061*youtube + 0.009987*facebook 
      summary(mod1)  
      # R-Squared = 0.7995
      par(mfrow=c(2,2))
      plot(mod1)
      # Já podemos admitir a Linearidade porém continuamos a rejeitar a 
      # Normalidade dos Resíduos.
      
      # Transformação de Box-Cox
      library(MASS)
      lambda <- boxcox(mod_final)                 
      # Sugere lambda próximo de 1 então não devemos transformar y.
      lambda$x[which(lambda$y==max(lambda$y))]  
      # Lambda = 0.9090909    Aproximadamente 1 então não devemos transformar y.
      
      # Transformar variáveis preditoras
      par(mfrow=c(1,1))
      plot(mrk$facebook, ri)  
      # facebook não apresenta padrão
      plot(mrk$youtube, ri)   
      # youtube apresenta padrão então deve ser transformado.
      # Como faz curva voltada para baixo, talvez uma raiz quadrada, para cortar 
      # efeito nos extremos...

      # Transformar X=youtube
      (mod2 <- lm(sales ~ facebook+I(sqrt(youtube)), data=mrk))
      # ŷ = -1.9415 + 1.0679*I(sqrt(youtube)) + 0.1945*facebook
      summary(mod2)  
      # R-Squared = 0.929
      par(mfrow=c(2,2))
      plot(mod2)     
      # Agora já podemos admitir a Normalide dos Resíduos porém voltámos a 
      # rejeitar a Linearidade e identificamos possíveis problemas com 131.

      
      # PONTO II) #
      # Incluir de termos polinomiais e/ou interações.
      
      # Incluir Interação
      (mod3 <- lm(sales ~ facebook * youtube, data=mrk))
      # ŷ = 8.1002642 + 0.0191011*youtube + 0.0288603*facebook + 0.0009054*facebook:youtube
      summary(mod3)
      # R-Squared = 0.9678
      par(mfrow=c(2,2))
      plot(mod3)
      # Neste caso, além de rejeitamos a Normalidade e Linearidade, começámos a
      # rejeitar a Homocedasticidade também, se excluirmos 131 e 156, talvez 
      # resolva os problemas com Linearidade e Homogeneidade.
      
      
      # Incluir Termo Polinomial e Tranformar y
      (mod4 <- lm(sales ~ facebook + poly(youtube, 2), data=mrk))
      # ŷ = 11.439 + 66.428*poly(youtube, 2)1 - 12.399*poly(youtube, 2)2 + 0.193*facebook
      summary(mod4)
      # R-Squared = 0.9167
      plot(mod4)     # normalidade ok, mas nao linear. 
      # Agora admitimos a Normalidade dos Resíduos porém não admitimos a 
      # Linearidade, existem possíveis problemas com 131 e talvez 6.
      
      AIC(mod_final, mod2, mod3, mod4)
      #             AIC
      # mod_final   853.3227
      # mod2        779.4293
      # mod3        623.2065
      # mod4        813.2458
      
      # mod3 (Incluir Interação) é o melhor, seguido de mod2 (Transformar 
      # x=youtube).
      
      (mod5 <- lm(sales ~ facebook * I(sqrt(youtube)), data=mrk))
      # ŷ = 5.33329 + 0.48024*I(sqrt(youtube)) - 0.05010*facebook + 0.01964*facebook:I(sqrt(youtube)) 
      summary(mod5)  
      # R-Squared = 0.9928
      plot(mod5)     
      # Sem 131 e 156, parecem ser admitidos os pressupostos.
      
      
      # PONTO III) #
      # Remover pontos influentes, caso existam.
      
      mod6 <- update(mod5, data=mrk[-c(131,156),])
      plot(mod6)

      # MODELO ESCOLHIDO: mod6
      (modeloFinal <- mod6)
      # ŷ = 5.49480 + 0.46870*I(sqrt(youtube)) - 0.04885*facebook + 0.01956*facebook:I(sqrt(youtube)) 
  
      
      
  ## ALÍNEA H ##
  # Caso em g) tenha optado por um modelo com variáveis transformadas e/ou 
  # interações, interprete o modelo obtido.
      
  summary(modeloFinal)
  # Coefficients:
  #                             Estimate  Std. Error  t value   Pr(>|t|)    
  # (Intercept)                5.4948018   0.1965237   27.960    < 2e-16 ***
  # facebook                  -0.0488459   0.0057478   -8.498   5.06e-15 ***
  # I(sqrt(youtube))           0.4686989   0.0149660   31.318    < 2e-16 ***
  # facebook:I(sqrt(youtube))  0.0195610   0.0004317   45.309    < 2e-16 ***
  
  # ŷ = 5.4948018 + 0.4686989*I(sqrt(youtube)) - 0.0488459*facebook + 0.0195610*facebook:I(sqrt(youtube)) 
  
  # FIXAR FACEBOOK
  # Se facebook = c
  # ŷ = 5.49480 + 0.46870*I(sqrt(youtube)) - 0.04885*c + 0.01956*c:I(sqrt(youtube)) 
  
  # Cada unidade aumentada em I(sqrt(youtube)) tem o efeito marginal de 
  # I(sqrt(youtube)) dado pela derivada de sales em ordem a I(sqrt(youtube)).
  # Se c = 0, cada unidade aumentada em I(sqrt(youtube)) espera aumentar os 
  # sales em 0.4686989 e quanto maior for o investimento no facebook, maior será
  # o investimento em I(sqrt(youtube)), dada a interação facebook:I(sqrt(youtube))
  
  
  # FIXAR YOUTUBE
  # Se I(sqrt(youtube)) = k
  # ŷ = 5.49480 + 0.46870*k - 0.04885*facebook + 0.01956*k*facebook

  # Cada unidade aumentada em facebook tem o efeito marginal dado pela 
  # derivada de sales em ordem a facebook.
  # Se k = 0, cada unidade aumentada em facebook espera diminuir os sales em 
  # 0.0488459. No entanto, quanto maior for o investimento em youtube menor será 
  # o impacto negativo do investimento em facebook, podendo mesmo tornar-se 
  # positivo, dada a interação facebook:I(sqrt(youtube)).
  
  
      
  

#### ANÁLISE CLASSIFICATÓRIA ####
### EXERCÍCIO 4.1 ###

(dados <- data.frame(x1 = c(7, 6, 3, 5, 4),
                     x2 = c(6, 8, 2, 6, 1),
                     x3 = c(3, 4, 4, 2, 2)))
rownames(dados) <- c("A", "B", "C", "D", "E")
  
  ## ALÍNEA A ##
  # Calcule a matriz de distâncias euclidianas entre os indivíduos.
  (matriz.distancias <- dist(dados, method = "euclidean"))
  #          A        B        C        D
  # B 2.449490                           
  # C 5.744563 6.708204                  
  # D 2.236068 3.000000 4.898979         
  # E 5.916080 7.549834 2.449490 5.099020
  
  

  ## ALÍNEA B ##
  # Proceda à junção dos indivíduos com base nos métodos:

      # PONTO I) #
      # Ligação simples (i.e., menor distância).
 
      (simples <- hclust(matriz.distancias, method = "single"))
      simples$height  # Distância a que foram realizadas as uniões
      # #1       #2       #3       #4
      # 2.236068 2.449490 2.449490 4.898979
      
      # PONTO II) #
      # Ligação completa (i.e., maior distância).
      
      (completa <- hclust(matriz.distancias, method = "complete"))
      completa$height  # Distância a que foram realizadas as uniões
      # #1       #2       #3       #4
      # 2.236068 2.449490 3.000000 7.549834
  
      
      # PONTO III) #
      # Distância média (i.e., maior distância).
  
      media <- hclust(matriz.distancias, method = "average")
      media$height  # Distância a que foram realizadas as uniões
      # #1       #2       #3       #4
      # 2.236068 2.449490 2.724745 5.986113
  
  
  
  ## ALÍNEA C ##
  # Construa os respetivos dendogramas.
  
  # Ligação SIMPLES
  plot(simples)
  # Pela análise das distâncias devemos considerar 2 grupos
  # cutree: permite cortar a arvore tendo em conta o numero de grupos definidos
  (membros <- cutree(simples, 2)) # A que grupos pertencem
  # A B C D E 
  # 1 1 2 1 2 
  table(membros)  # Número de membros por grupo
  #         Grupo 1   Grupo 2 
  # membros       3         2 
  rect.hclust(simples,2)  # Assinalar os grupos formados no dendograma (Nota: O dendograma foi previamente construído)
  
  
  # Ligação COMPLETA
  plot(completa)
  # Pela analise das distancias considerar 2 grupos
  (membros <- cutree(completa, 2)) # A que grupos pertencem
  # A B C D E 
  # 1 1 2 1 2 
  table(membros)  # Número de membros por grupo
  #         Grupo 1   Grupo 2 
  # membros       3         2 
  rect.hclust(completa,2)  
  
  
  # Distância MÉDIA
  plot(media)
  # Pela analise das distancias considerar 2 grupos
  (membros <- cutree(media, 2)) # A que grupos pertencem
  # A B C D E 
  # 1 1 2 1 2 
  table(membros)  # Número de membros por grupo
  #         Grupo 1   Grupo 2 
  # membros       3         2 
  rect.hclust(media,2)  
  
  # Independentemente do método utilizado, os resultados indicam sempre que 
  # devemos considerar 2 grupos: (A, D, B) e (C, E).
  
  
  
  ## ALÍNEA E ##
  # No R explore outros métodos de agrupamento, e compare os resultados.
  
  # Método do CENTRÓIDE:
  centroide <- hclust(matriz.distancias, method = "centroid")
  centroide$height  # Distância a que foram realizadas as uniões
  # #1       #2       #3       #4
  # 2.236068 2.165728 2.449490 4.519790
  
  # Nota: Estas distâncias não correspondem às obtidas manualmente porque a 
  #       função hclust considera que a matriz de entrada é o quadrado da 
  #       distância euclideana (sem raiz quadrada).
  # Para obter os mesmos resultados dos cálculos manuais:
  centroide2 = hclust(matriz.distancias^2, method='centroid')
  sqrt(centroide2$height)  # Distância a que foram realizadas as uniões
  
  plot(centroide2)
  # Pela analise das distancias considerar 2 grupos
  (membros <- cutree(centroide2, 2)) # A que grupos pertencem
  # A B C D E 
  # 1 1 2 1 2 
  table(membros)  # Número de membros por grupo
  #         Grupo 1   Grupo 2 
  # membros       3         2 
  rect.hclust(centroide2,2)
  
  
  # Método de WARD
  ward <- hclust(matriz.distancias, method = "ward.D2")
  ward$height  # distancia a que foram realizadas as unioes
  # #1       #2       #3       #4
  #2.236068 2.449490 2.886751 8.891944
  
  # ou
  library(cluster)
  agnes(matriz.distancias, method = "ward", trace.lev=3)
  
  plot(ward)
  # Pela analise das distancias considerar 2 grupos
  (membros <- cutree(ward, 2)) # A que grupos pertencem
  # A B C D E 
  # 1 1 2 1 2 
  table(membros)  # Número de membros por grupo
  #         Grupo 1   Grupo 2 
  # membros       3         2 
  rect.hclust(ward,2)
  
  
  
### EXERCÍCIO 4.2 ###

  library(readxl)
  wescheler <- read_excel("Análise classificatória/Wescheler.xlsx") # le em formato tibble
  head(wescheler)
  
  wescheler <- data.frame(wescheler) # Converter tibble em data.frame
  rownames(wescheler) <- wescheler$Indivíduo
  wescheler <- wescheler[, -1]  # Remover coluna Indivíduo da análise  OU (wescheler <- subset(wescheler, select=-Indivíduo))


  
  ## ALÍNEA A ##
  # Utilize a análise classificatória hierárquica para tentar identificar grupos 
  # de indivíduos aos quais deva ser recomendado um acompanhamento médico mais 
  # frequente. 

  (matriz.distancias <- dist(wescheler, method = "euclidean"))
  #            I1        I2        I3        I4        I5        I6        I7        I8        I9
  # I2  13.601471                                                                                
  # I3   8.485281  8.544004                                                                      
  # I4  14.764823  8.774964  8.124038                                                            
  # I5   7.280110  6.480741  5.000000  9.110434                                                  
  # I6   5.830952 12.845233 10.295630 14.142136  7.416198                                        
  # I7   7.416198  7.071068  2.645751  8.660254  2.449490  8.660254                              
  # I8   4.000000 14.594520 10.198039 16.792856  9.000000  5.830952  9.110434                    
  # I9  13.747727  6.480741  9.433981  5.567764  7.348469 11.445523  8.366600 15.264338          
  # I10  8.774964  5.830952  3.000000  7.937254  2.828427  9.539392  1.414214 10.246951  7.615773
  
  
  
  # PONTO I) #
  # Considere a distância euclidiana como medida de distância, e utilize 
  # diferentes métodos de aglomeração (menor distância, maior distância, 
  # distância média e Ward).
  
  # Ligação SIMPLES (Menor Distância)
  simples <- hclust(matriz.distancias, method = "single")
  simples$height
  # #1       #2       #3       #4       #5       #6       #7       #8       #9
  # 1.414214 2.449490 2.645751 4.000000 5.567764 5.830952 5.830952 6.480741 7.280110
  plot(simples)
  # Pela análise das distâncias considerar 5 grupos ou 3 (muito forçado)
  # Scree plot que pode ser útil para decidir o número de grupos: num. de grupos 
  # dado pelo ponto de inflexão
  nH <- length(simples$height)
  plot(c(simples$height[nH:1],0), type="b")
  # 3 ou 7 grupos
  
  
  # Solução para 3 grupos
  plot(simples)
  rect.hclust(simples,3)
  (membros <- cutree(simples, 3))
  #  I1  I2  I3  I4  I5  I6  I7  I8  I9 I10 
  #   1   2   2   3   2   1   2   1   3   2 
  table(membros)
  # G1: 3 elementos - I1 + I6 + I8
  # G2: 5 elementos - I2 + I3 + I5 + I7 + I10
  # G3: 2 elementos - I4 + I9

  aggregate(wescheler,list(membros),mean)
  # G1: < Média em todas as avaliações (individuos em risco)
  # G2: > Média a IVP
  # G3: > Média a ICV, IOP e IMT
  # Todas as variáveis diferenciam bem os grupos
  
  library(cluster)
  plot(silhouette(cutree(simples,3), matriz.distancias))
  # Observações com barra próxima de 1 estão bem agrupadas
  # Observações com barra próxima de 0 estão entre 2 grupos
  # Observações com barra negativa, ou estão no grupo errado ou são outliers e 
  # podem ter de ser removidas.
  
  # Conclusão: Nada a observar
  
  
  # Solução para 5 grupos
  plot(simples)
  rect.hclust(simples,5)
  (membros <- cutree(simples, 5))
  #  I1  I2  I3  I4  I5  I6  I7  I8  I9 I10 
  #   1   2   3   4   3   5   3   1   4   3 
  table(membros)
  # G1: 2 elementos - I1 + I8
  # G2: 1 elemento  - I2
  # G3: 4 elementos - I3 + I5 + I7 + I10
  # G4: 2 elementos - I4 + I9
  # G5: 1 elemento  - I6
  
  aggregate(wescheler,list(membros),mean)
  # G1: < Média a ICV, IOP e IMT (individuos em risco)
  # G2: > Média a ICV e IOP e Médias intermédias a IMT e IVP
  # G3: Médias intermédias a IMT, IOP e IMT e elevada a IVP 
  # G4: > Média a IMT elevadas a ICV e IOP
  # G5: < media a IVP e media baixa a ICV e IOP -> possivel individuo em risco
  # A escala ICV é a que menos diferencia os 5 grupos
  
  plot(silhouette(cutree(simples,5), matriz.distancias))
  # Conclusão: Nada a observar
  
  
  # Ligação COMPLETA
  completa <- hclust(matriz.distancias, method = "complete")
  completa$height
  # #1        #2        #3        #4        #5        #6        #7        #8       #9
  # 1.414214  2.828427  4.000000  5.000000  5.567764  5.830952  8.544004  9.433981 16.792856
  plot(completa)
  # Pela análise das distâncias considerar 4 grupos
  # Scree plot
  nH <- length(completa$height)
  plot(c(completa$height[nH:1],0), type="b")
  # 2 ou 4 grupos
  
  # Solução com 4 grupos
  plot(completa)
  rect.hclust(completa,4)
  (membros <- cutree(completa, 4))
  # I1  I2  I3  I4  I5  I6  I7  I8  I9 I10 
  #  1   2   3   4   3   1   3   1   4   3 
  table(membros)
  # G1: 3 elementos - I1 + I6 + I8
  # G2: 1 elemento  - I2
  # G3: 4 elementos - I3 + I5 + I7 + I10
  # G4: 2 elementos - I4 + I9
  
  aggregate(wescheler,list(membros),mean)
  # G1: < Média a ICV, IOP, IMT e IVP (individuos em risco)
  # G2: > Média a ICV e IOP
  # G3: medias intermedias a IMT e IOP e elevada a IVP 
  # G4: > media a IMT e media elevada a ICV e IOP
  # Em suma: IOP parece ser a escala que mais diferencia os 4 grupos.
  
  plot(silhouette(cutree(completa,4), matriz.distancias))
  # nada a observar
  
  
  # 3) metodo da ligacao MEDIA
  media <- hclust(matriz.distancias, method = "average")
  media$height
  # dendograma
  plot(media, 
       main = "Método da ligação MÉDIA", 
       sub = "Dendograma", xlab = "", ylab="Distância")
  # Pela analise das distancias considerar 4 grupos
  # scree plot
  nH <- length(media$height)
  plot(c(media$height[nH:1],0), type="b")
  # 4 grupos
  
  # solucao com 4 grupos
  plot(media, 
       main = "Método da ligação MÉDIA", 
       sub = "Dendograma", xlab = "", ylab="Distância")
  rect.hclust(media,4)
  (membros <- cutree(media, 4))
  table(membros)  # numero de membros por grupo
  # G1: 3 elementos - I1 + I6 + I8
  # G2: 1 elemento  - I2
  # G3: 4 elementos - I3 + I5 + I7 + I10
  # G4: 2 elementos - I4 + I9
  
  # obter caracteristicas dos grupos (media de cada variavel numerica)
  aggregate(wescheler,list(membros),mean)
  # G1: < media a todas as vars -> individuos em risco
  # G2: > media a ICV e IOP
  # G3: > media IVP e medias intermedias às restantes
  # G4: > media IMT, medias elevadas a ICV e IOP
  
  plot(silhouette(cutree(media,4), matriz.distancias))
  # nada a observar
  
  
  # 4) metodo de WARD
  ward <- hclust(matriz.distancias, method = "ward.D2")
  ward$height
  # dendograma
  plot(ward, 
       main = "Método de WARD", 
       sub = "Dendograma", xlab = "", ylab="Distância")
  # Pela analise das distancias considerar 3 ou 4 grupos
  # scree plot
  nH <- length(ward$height)
  plot(c(ward$height[nH:1],0), type="b")
  # 4 grupos
  
  # solucao com 3 grupos
  plot(media, 
       main = "Método da ligação MÉDIA", 
       sub = "Dendograma", xlab = "", ylab="Distância")
  rect.hclust(ward,3)
  (membros <- cutree(ward, 3))
  table(membros)  # numero de membros por grupo
  # G1: 3 elementos - I1 + I6 + I8
  # G2: 3 elementos - I2 + I4 + I9
  # G3: 4 elementos - I3 + I5 + I7 + I10
  
  # obter caracteristicas dos grupos (media de cada variavel numerica)
  aggregate(wescheler,list(membros),mean)
  # G1: < media a todas as vars -> individuos em risco
  # G2: > media a ICV, IOP e IMT
  # G3: > media IVP e medias intermedias às restantes
  
  plot(silhouette(cutree(ward,3), matriz.distancias))
  # individuo 2 nao parece fazer parte do grupo -> separar do grupo, i.e., formar 4 grupos
  plot(silhouette(cutree(ward,4), matriz.distancias))
  # sem problemas
  plot(silhouette(cutree(ward,2), matriz.distancias)) 
  # sem problemas
  # optar por solucao com 2 ou 4 grupos
  
  # solucao com 4 grupos
  (membros <- cutree(ward, 4))
  table(membros)  # numero de membros por grupo
  # G1: 3 elementos - I1 + I6 + I8
  # G2: 1 elemento  - I2
  # G3: 4 elementos - I3 + I5 + I7 + I10
  # G4: 2 elementos - I4 + I9
  
  # obter caracteristicas dos grupos (media de cada variavel numerica)
  aggregate(wescheler,list(membros),mean)
  # G1: < media a todas as vars -> individuos em risco
  # G2: > media a ICV e IOP
  # G3: > media IVP e medias intermedias às restantes
  # G4: > media IMT, medias elevadas a ICV e IOP
  
  # conclusao: ligacao simples apresenta solucao com mais grupos (6) ou menos (3), 
  #            ao passo que os restantes metodos apresentam uma solucao com 4 grupos
  #            e com a mesma composicao (i.e., ha concordancia entre os resultados).
  
  
  
  
  # b)
  
  # -- matriz de distancias de Manhattan
  (matriz.distancias <- dist(wescheler, method = "manhattan"))
  
  # 1) metodo da ligacao SIMPLES
  simples <- hclust(matriz.distancias, method = "single")
  simples$height
  # dendograma
  plot(simples, 
       main = "Método da ligação SIMPLES", 
       sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
  # Pela analise das distancias considerar 7 grupos!!! 
  # scree plot
  nH <- length(simples$height)
  plot(c(simples$height[nH:1],0), type="b")
  # 2 ou 7 grupos
  
  # solucao 2 grupos
  plot(simples, 
       main = "Método da ligação SIMPLES", 
       sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
  rect.hclust(simples,2)
  (membros <- cutree(simples, 2))
  # obter caracteristicas dos grupos (media de cada variavel numerica)
  aggregate(wescheler,list(membros),mean)
  # IOP e a variavel que mais diferencia os grupos
  plot(silhouette(cutree(simples,2), matriz.distancias))
  # conclusao: nada a observar
  
  # solucao 7 grupos
  plot(simples, 
       main = "Método da ligação SIMPLES", 
       sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
  rect.hclust(simples,7)
  (membros <- cutree(simples, 7))
  # obter caracteristicas dos grupos (media de cada variavel numerica)
  aggregate(wescheler,list(membros),mean)
  # IOP e a variavel que mais diferencia os grupos
  plot(silhouette(cutree(simples,7), matriz.distancias))
  # conclusao: nada a observar
  
  
  # 2) metodo da ligacao COMPLETA
  completa <- hclust(matriz.distancias, method = "complete")
  completa$height
  # dendograma
  plot(completa, 
       main = "Método da ligação COMPLETA", 
       sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
  # Pela analise das distancias considerar 4 grupos
  # scree plot
  nH <- length(completa$height)
  plot(c(completa$height[nH:1],0), type="b")
  # 4 grupos
  
  # solucao 4 grupos
  plot(completa, 
       main = "Método da ligação COMPLETA", 
       sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
  rect.hclust(completa,4)
  (membros <- cutree(completa, 4))
  # obter caracteristicas dos grupos (media de cada variavel numerica)
  aggregate(wescheler,list(membros),mean)
  plot(silhouette(cutree(completa,4), matriz.distancias))
  # nada a observar
  
  
  # 3) metodo da ligacao MEDIA
  media <- hclust(matriz.distancias, method = "average")
  media$height
  # dendograma
  plot(media, 
       main = "Método da ligação MÉDIA", 
       sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
  # Pela analise das distancias considerar 3 ou 4 grupos
  # scree plot
  nH <- length(media$height)
  plot(c(media$height[nH:1],0), type="b")
  # ... talvez 3 ou 4 grupos
  
  # solucao 4 grupos
  plot(media, 
       main = "Método da ligação MÉDIA", 
       sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
  rect.hclust(media,4)
  (membros <- cutree(media, 4))
  # obter caracteristicas dos grupos (media de cada variavel numerica)
  aggregate(wescheler,list(membros),mean)
  plot(silhouette(cutree(media,4), matriz.distancias))
  # nada a observar
  
  
  # 4) metodo de WARD
  ward <- hclust(matriz.distancias, method = "ward.D2")
  ward$height
  # dendograma
  plot(ward, 
       main = "Método de WARD", 
       sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
  # Pela analise das distancias considerar 3 grupos
  # scree plot
  nH <- length(ward$height)
  plot(c(ward$height[nH:1],0), type="b")
  # 3 grupos
  
  # solucao 3 grupos
  plot(ward, 
       main = "Método de WARD", 
       sub = "Dendograma", xlab = "", ylab="Distância de Manhattan")
  rect.hclust(ward,3)
  (membros <- cutree(ward, 3))
  # obter caracteristicas dos grupos (media de cada variavel numerica)
  aggregate(wescheler,list(membros),mean)
  plot(silhouette(cutree(ward,3), matriz.distancias))
  # individuo 2 nao parece fazer parte do grupo -> separar do grupo, i.e., formar 4 grupos
  plot(silhouette(cutree(ward,2), matriz.distancias))
  # ok
  plot(silhouette(cutree(ward,4), matriz.distancias))
  # ok
  # a solucao com 2 grupos parece ser melhor...
  
  (membros <- cutree(ward, 2))
  aggregate(wescheler,list(membros),mean)
  # G1: medias mais baixas em todas as variaveis
  # G2: medias mais elevadas em todas as variaveis
  
  (membros <- cutree(ward, 4))
  aggregate(wescheler,list(membros),mean)
  # G1: medias mais baixas em todas as variaveis
  # G2: medias mais elevadas a ICV e IOP
  # G3: media mais elevada a IVP
  # G4: media mais elevada a IMT
  
  
  # -- matriz de distancias de Minkowski 
  (matriz.distancias <- dist(wescheler, method = "minkowski", p=3))
  
  # 1) metodo da ligacao SIMPLES
  simples <- hclust(matriz.distancias, method = "single")
  simples$height
  # dendograma
  plot(simples, 
       main = "Método da ligação SIMPLES", 
       sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
  # Pela analise das distancias considerar 3 ou 7 grupos!!! 
  # scree plot
  nH <- length(simples$height)
  plot(c(simples$height[nH:1],0), type="b")
  # 3 ou 7 grupos
  
  # solucao 3 grupos
  plot(simples, 
       main = "Método da ligação SIMPLES", 
       sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
  rect.hclust(simples,3)
  (membros <- cutree(simples, 3))
  # obter caracteristicas dos grupos (media de cada variavel numerica)
  aggregate(wescheler,list(membros),mean)
  # IOP e a variavel que mais diferencia os grupos
  plot(silhouette(cutree(simples,3), matriz.distancias))
  # conclusao: nada a observar
  
  
  # 2) metodo da ligacao COMPLETA
  completa <- hclust(matriz.distancias, method = "complete")
  completa$height
  # dendograma
  plot(completa, 
       main = "Método da ligação COMPLETA", 
       sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
  # Pela analise das distancias considerar 2 ou 4 grupos
  # scree plot
  nH <- length(completa$height)
  plot(c(completa$height[nH:1],0), type="b")
  # 2 ou 4 grupos
  
  # solucao 2 grupos
  plot(completa, 
       main = "Método da ligação COMPLETA", 
       sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
  rect.hclust(completa,2)
  (membros <- cutree(completa, 2))
  # obter caracteristicas dos grupos (media de cada variavel numerica)
  aggregate(wescheler,list(membros),mean)
  plot(silhouette(cutree(completa,2), matriz.distancias))
  # nada a observar
  
  
  # 3) metodo da ligacao MEDIA
  media <- hclust(matriz.distancias, method = "average")
  media$height
  # dendograma
  plot(media, 
       main = "Método da ligação MÉDIA", 
       sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
  # Pela analise das distancias considerar 3 ou 4 grupos
  # scree plot
  nH <- length(media$height)
  plot(c(media$height[nH:1],0), type="b")
  # 4 grupos
  
  # solucao 4 grupos
  plot(media, 
       main = "Método da ligação MÉDIA", 
       sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
  rect.hclust(media,4)
  (membros <- cutree(media, 4))
  # obter caracteristicas dos grupos (media de cada variavel numerica)
  aggregate(wescheler,list(membros),mean)
  plot(silhouette(cutree(media,4), matriz.distancias))
  # nada a observar
  
  
  # 4) metodo de WARD
  ward <- hclust(matriz.distancias, method = "ward.D2")
  ward$height
  # dendograma
  plot(ward, 
       main = "Método de WARD", 
       sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
  # Pela analise das distancias considerar 4 grupos
  # scree plot
  nH <- length(ward$height)
  plot(c(ward$height[nH:1],0), type="b")
  # 4 grupos
  
  # solucao 4 grupos
  plot(ward, 
       main = "Método de WARD", 
       sub = "Dendograma", xlab = "", ylab="Distância de Minkowski")
  rect.hclust(ward,4)
  (membros <- cutree(ward, 4))
  # obter caracteristicas dos grupos (media de cada variavel numerica)
  aggregate(wescheler,list(membros),mean)
  plot(silhouette(cutree(ward,4), matriz.distancias))
  # ok
  
  
  
  
  ## PACOTES ALTERNATIVOS PARA ANALISE CLASSIFICATORIA HIERARQUICA
  # exemplo com metodo simples e definindo 3 grupos
  library(factoextra)
  # Se soubermos quantos grupos queremos
  agrupamento <- hcut(wescheler, 
                      k = 3,                   # numero de grupos
                      hc_method = "single",    # metodo de ligacao
                      hc_metric = "euclidean", # distancia a usar 
                      stand = F,               # se usa dados originais (=F) ou estandardizados (=T)
                      graph=F)                 # se mostra ou nao o dendograma
  # grupo a que pertence cada observacao
  agrupamento$cluster
  # tamanho dos grupos
  agrupamento$size
  # dendograma
  fviz_dend(agrupamento, 
            rect = TRUE) # se representa um rectangulo a delimitar os grupos
  # grafico silhouette
  fviz_silhouette(agrupamento)
  # se quisermos ver a solucao em 2D com base nas componentes principais
  fviz_cluster(agrupamento)
  
  
  # outra alternativa
  library(FactoMineR)
  agrupamento <- HCPC(wescheler, 
                      nb.clust=3,           # numero de grupos
                      metric="euclidean",   # distancia a usar
                      method="single",      # metodo de ligacao
                      graph=T)              # mostra grafico de dispersao com grupos
  fviz_dend(agrupamento, 
            rect = TRUE) # se representa um rectangulo a delimitar os grupos
  fviz_cluster(agrupamento)
  
  # sem definir o numero de grupos
  agrupamento <- HCPC(wescheler, nb.clust=-1, metric="euclidean", method="single", graph=T)
  fviz_dend(agrupamento, 
            rect = TRUE) # se representa um rectangulo a delimitar os grupos
  fviz_cluster(agrupamento)
  
  
### EXERCÍCIO 4.3 ###
  # optativo: definir a diretoria atual como diretoria de trabalho
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  # ler dados 
  dados <- read.csv2("Análise classificatória/profissoes.csv")
  head(dados)
  # considerar profissao como nome das linhas
  rownames(dados) <- dados$profissão
  
  # remover colunas 1 e 2 do data.frame
  dados <- dados[,-c(1:2)]
  head(dados)
  # equivalente a 
  #dados <- subset(dados, select=-c(ID, profissão))
  
  # a) 
  # dados --> variaveis originais
  # dadosE --> variaveis estandardizadas
  dadosE <- within(dados,{
    prestigioZ <- scale(prestigio)
    suicídioZ <- scale(suicídio)
    rendimentoZ <- scale(rendimento)
    educaçãoZ <- scale(educação)
  }
  )
  # reter apenas dados das variaveis estandardizadas
  dadosE <- dadosE[,5:8]
  
  # d(canalizadores,cozinheiros)
  # usando dados originais
  (distancia <- dist(dados[c("Canalizadores","Cozinheiros"),], , method = "euclidean"))
  # usando dados estandardizados
  (distanciaE <- dist(dadosE[c("Canalizadores","Cozinheiros"),], , method = "euclidean"))
  # conclusao: os valores sai muito diferentes
  
  
  # b)
  (matriz.distancias <- dist(dados, method = "euclidean"))
  
  agrupamento <- hclust(matriz.distancias, method = "complete")
  
  # b) i)
  plot(agrupamento,  horiz=T)
  # alternativa: por vezes e util apresentar o dendograma na horizontal
  par(mar=c(4,1,2,6))
  plot(as.dendrogram(agrupamento),  horiz=T,
       xlab="distancia euclidiana",
       main="Método da MAIOR distancia")
  
  # b) ii)
  # pelo dendograma: 4 ou 8 grupos
  
  # scree plot
  par(mar=c(4,4,2,2))
  nH <- length(agrupamento$height)
  plot(c(agrupamento$height[nH:1],0), type="b", xaxt="n", horiz=T)
  axis(1, at = seq(0, 30, by = 2))
  # 4 ou 8 grupos
  
  # solucao com 4 grupos
  plot(agrupamento, 
       main = "Método da MAIOR distancia", 
       sub = "Dendograma", xlab = "", ylab="Distância euclidiana")
  rect.hclust(agrupamento,4)
  # ou
  library(factoextra)
  fviz_dend(agrupamento, k = 4)+
    labs(y="Distância euclidiana", title="Método da MAIOR distância")
  
  
  # b) iii)
  (membros <- cutree(agrupamento, 4))
  table(membros)
  # G1 (n=4):  Advogados, Autores, Engenheiros, Prof.Univers. 
  # G2 (n=3):  Arquitectos, Padres, Químicos
  # G3 (n=12): Barbeiros, Camionistas, Canalizadores, Carpinteiros, Criadas, Guardas, 
  #            Mecânicos, Pintores, Polícias, Porteiro, Receptionistas, Vendedores
  # G4 (n=10): Comerciantes, Contabilistas, Cozinheiros, Dentistas, Electricistas, 
  #            Maquinistas, Marceneiros, Prof.Liceu, Seguros, Trab.Sociais 
  
  # obter caracteristicas dos grupos (media de cada variavel numerica)
  aggregate(dados,list(membros),mean)
  # G1: 2a maior media a todas as variaveis
  # G2: > media a todas as variaveis
  # G3: < media em prestigio, rendimento, educacao 
  # G4: < media em suicidio, e 2a menor media nas restantes variaveis
  # rendimento e prestigio sao as variaveis que mais diferenciam os grupos
  # G2: profissoes com elevado rendimento e prestigio
  # G3: profissoes com baixo rendimento e prestigio
  # ou graficamente
  library(tidyr)
  library(dplyr)
  dataG <- aggregate(dados,list(membros),mean) %>%
    pivot_longer(cols = -Group.1,  # mantem a coluna Group.1 como fixa
                 names_to = "Variavel", 
                 values_to = "Media")
  ggplot(dataG, aes(x = Variavel, y = Media, group = factor(Group.1), color = factor(Group.1))) +
    geom_line() +          
    geom_point(size = 3) + 
    labs(x = "", y = "Média", title = "Média das variáveis por grupo", color="Grupo")+
    theme_minimal()
  # Em falta: que nome sugerem para cada grupo, com base nas suas características?
  # (ou seja, no final devem nomear os grupos formados)
  
  library(cluster)
  plot(silhouette(cutree(agrupamento,4), matriz.distancias))
  # individuos 2 (arquitectos) e 12 (dentistas) parecem nao fazer parte dos grupos -> separar do grupo, i.e., formar 4 grupos
  plot(silhouette(cutree(agrupamento,3), matriz.distancias))
  # problemas com 1 e 28
  plot(silhouette(cutree(agrupamento,5), matriz.distancias))
  # problemas com 2, 12 e 28
  plot(silhouette(cutree(agrupamento, 6), matriz.distancias))
  # problemas com 1, 12 e 28
  # conclusao: existem sempre casos que parecem nao fazer parte -> outliers? experimentar remover e refazer agrupamentos
  # (...)
  
  
  # c)
  (matriz.distanciasE <- dist(dadosE, method = "euclidean"))
  
  agrupamentoE <- hclust(matriz.distanciasE, method = "complete")
  
  # c) i)
  plot(agrupamentoE, hang = - 1)
  
  # c) ii)
  # pelo dendograma: 5 grupos
  
  # scree plot
  nH <- length(agrupamentoE$height)
  plot(c(agrupamentoE$height[nH:1],0), type="b", xaxt="n")
  axis(1, at = seq(0, 30, by = 2))
  # 5 grupos
  
  # solucao com 5 grupos
  plot(agrupamentoE, 
       main = "Método da MAIOR distancia (variaveis estandardizadas)", 
       sub = "Dendograma", xlab = "", ylab="Distância euclidiana")
  rect.hclust(agrupamentoE, 5)
  # ou
  fviz_dend(agrupamentoE, k = 5)+
    labs(y="Distância euclidiana", title="Método da MAIOR distancia (variaveis estandardizadas)")
  
  # c) iii)
  (membrosE <- cutree(agrupamentoE, 5))
  table(membrosE)
  # G1 (n=5):  Advogados, Arquitectos, Autores, Padres, Químicos
  # G2 (n=12): Barbeiros, Camionistas, Canalizadores, Carpinteiros, Cozinheiros, 
  #            Criadas, Electricistas, Guardas, Marceneiros, Mecânicos, Porteiro, Vendedores 
  # G3 (n=4):  Comerciantes, Pintores, Polícias, Receptionistas,
  # G4 (n=5):  Contabilistas, Dentistas, Prof.Liceu, Prof.Univers., Trab.Sociais
  # G5 (n=3): Engenheiros, Maquinistas, Seguros
  
  # obter caracteristicas dos grupos (media de cada variavel numerica)
  aggregate(dadosE,list(membrosE),mean)
  # G1: todas as medias > 0 -> acima da media em todas as variaveis
  # G2: todas as médias < 0 -> abaixo da média em todas as variaveis
  # G3: tx de suicidio muito acima da media
  # G4: semelhante a 1, mas com tx sucidio abaixo da media
  # G5: educacao abaixo a media, mas tudo o resto acima embora proximo da media
  # rendimento e prestigio sao as variaveis que mais diferenciam os grupos
  # G2: profissoes com elevado rendimento e prestigio
  # G3: profissoes com baixo rendimento e prestigio
  # ou graficamente
  dataG <- aggregate(dadosE,list(membrosE),mean) %>%
    pivot_longer(cols = -Group.1,  # mantem a coluna Group.1 como fixa
                 names_to = "Variavel", 
                 values_to = "Media")
  ggplot(dataG, aes(x = Variavel, y = Media, group = factor(Group.1), color = factor(Group.1))) +
    geom_line() +          
    geom_point(size = 3) + 
    labs(x = "", y = "Média", title = "Média das variáveis por grupo", color="Grupo")+
    theme_minimal()
  
  plot(silhouette(cutree(agrupamentoE, 5), matriz.distanciasE))
  # 13 e 8 perto de 0
  plot(silhouette(cutree(agrupamentoE, 4), matriz.distanciasE))
  # tudo ok -> sugere que solucao com 4 grupos deve ser preferivel
  
  # solucao com 4 grupos
  plot(agrupamentoE, 
       main = "Método da MAIOR distancia (variaveis estandardizadas)", 
       sub = "Dendograma", xlab = "", ylab="Distância euclidiana")
  rect.hclust(agrupamentoE, 4)
  (membrosE <- cutree(agrupamentoE, 4))
  table(membrosE)
  # G1 (n=5):  Advogados, Arquitectos, Autores, Padres, Químicos
  # G2 (n=12): Barbeiros, Camionistas, Canalizadores, Carpinteiros, Cozinheiros, 
  #            Criadas, Electricistas, Guardas, Marceneiros, Mecânicos, Porteiro, Vendedores 
  # G3 (n=7):  Comerciantes, Engenheiros, Maquinistas, Pintores, Polícias, Receptionistas, Seguros
  # G4 (n=5):  Contabilistas, Dentistas, Prof.Liceu, Prof.Univers., Trab.Sociais
  
  # obter caracteristicas dos grupos (media de cada variavel numerica)
  aggregate(dadosE,list(membrosE),mean)
  # graficamente
  dataG <- aggregate(dadosE,list(membrosE),mean) %>%
    pivot_longer(cols = -Group.1,  # mantem a coluna Group.1 como fixa
                 names_to = "Variavel", 
                 values_to = "Media")
  geom_line() +          
    geom_point(size = 3) + 
    labs(x = "", y = "Média", title = "Média das variáveis por grupo", color="Grupo")+
    theme_minimal()
  # G1: todas as medias > 0 -> acima da media em todas as variaveis
  # G2: todas as médias < 0 -> abaixo da média em todas as variaveis
  # G3: tx de suicidio muito acima da media
  # G4: semelhante a 1, mas com tx sucidio abaixo da media e rendimento inferior (mais proximo da media)
  # ou seja:
  # G1: profissoes com elevado rendimento, prestigio e educacao
  # G2: profissoes com baixo rendimento, prestigio, educacao e suicidio
  # G3: profissoes com baixo rendimento, prestigio e educacao, mas elevado sucicidio
  # G4: profissoes com elevado prestigio e educacao, mas com rendimnto proximo da media e baixo suicidio
  
  # Em falta: que nome sugerem para cada grupo, com base nas suas características?
  # (ou seja, no final devem nomear os grupos formados)
  
  
  # d)
  # outra alternativa para visualizar as caracteristicas dos grupos por individuo
  library(pheatmap)
  pheatmap(t(dados), cutree_cols = 4)
  # rendimento é a variavel que mais distingue os grupos
  pheatmap(t(dadosE), cutree_cols = 4)
  # suicidio e rendimento sao as variaveis que mais distinguem os grupos
  
  # diferencas com 4 grupos: versao Estandardizada vs original:
  # juntou (arquitetos, padres, quimicos) com (advogados, autores) 
  # (cozinheiros, eletricistas, marceneiros)  mudaram para grupo (barbeiros, ...)
  # juntou Comerciantes a (Pintores, Polícias, Receptionistas)
  # juntou (Contabilistas, Dentistas) a (Prof.Liceu, Trab.Sociais) e Prof.Univers.
  # juntou Engenheiros a (Maquinistas, Seguros)
  
  
  
### EXERCÍCIO 4.4 ###
  # conjunto de dados
  (dados <- data.frame(x1 = c(7, 6, 3, 5, 4),
                       x2 = c(6, 8, 2, 6, 1)))
  
  # alterar nome das linhas
  rownames(dados) <- c("A", "B", "C", "D", "E")
  
  # a)
  # Quando queremos definir os grupos iniciais: criar matriz com os centros as coordenadas dos grupos
  Medias <- aggregate(dados, 
                      list(c(1,1,2,2,2)), # grupo inicial a que foi associada cada linha
                      mean)               # medias por grupo  
  
  agrupaKmeans <- kmeans(dados, 
                         centers=Medias[,-1], # centro dos grupos: pode ser data.frame ou matriz
                         nstart = 1)
  # ou em alternativa
  # converter centros dos grupos num objeto matriz
  centros <- as.matrix(Medias[,-1],  ncol=2, byrow=T)
  agrupaKmeans <- kmeans(dados, 
                         centers=centros,
                         nstart = 1)
  
  # ver agrupamento final
  agrupaKmeans$cluster  # agrupamento final
  
  plot(dados, 
       col  = agrupaKmeans$cluster)  # a cor dos pontos varia com o grupo a que foi associado  
  # adicionar o centro dos grupos
  points(agrupaKmeans$centers, 
         col = c("green","blue"),   # cor
         pch = 7)                   # tipo de simbolo
  
  # alternativa para visualizar os agrupamentos
  library(factoextra)
  fviz_cluster(agrupaKmeans, data = dados,
               ggtheme = theme_bw())
  
  
  # b)
  Medias <- aggregate(dados, 
                      list(c(1,2,2,2,1)), # grupo inicial a que foi associada cada linha
                      mean)               # medias por grupo  
  
  agrupaKmeans <- kmeans(dados, 
                         centers=Medias[,-1], # centro dos grupos
                         nstart = 1)
  agrupaKmeans$cluster  # agrupamento final
  
  
  plot(dados, col  = agrupaKmeans$cluster)
  # adicionar o centro dos grupos
  points(agrupaKmeans$centers, 
         col = c("green","blue"),   # cor
         pch = 7)                   # tipo de simbolo
  
  # ver agrupamentos
  library(factoextra)
  fviz_cluster(agrupaKmeans, data = dados,
               ggtheme = theme_bw())
  
  
  # c)
  agrupaKmeans <- kmeans(dados, 
                         centers=2,   # numero de grupos
                         nstart = 25)
  agrupaKmeans$cluster  # agrupamento final
  
  plot(dados, col  = agrupaKmeans$cluster)
  # adicionar o centro dos grupos
  points(agrupaKmeans$centers, 
         col = c("green","blue"),   # cor
         pch = 7)                   # tipo de simbolo
  
  # ver agrupamentos
  library(factoextra)
  fviz_cluster(agrupaKmeans, data = dados,
               ggtheme = theme_bw())
  
  
  # d)
  # obter valor de R^2
  # numerador da var de cada variavel
  SQ <- function(x){ var(x)*(length(x)-1) }
  
  (SQT <- SQ(dados$x1)+SQ(dados$x2))
  
  g1 <- dados[c("A", "B", "D"),]
  g2 <- dados[c("C", "E"),]
  (SQDg1 <- SQ(g1$x1)+SQ(g1$x2))
  (SQDg2 <- SQ(g2$x1)+SQ(g2$x2))
  
  (R2 <- 1-(SQDg1+SQDg2)/SQT)
  # Nota: Ha outras formas mais eficientes de obter o R2. 
  # Com esta salienta-se o que esta a ser calculado
  
  
### EXERCÍCIO 4.5 ###
  # optativo: definir a diretoria atual como diretoria de trabalho
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  ## a
  # ler dados
  library(readxl)
  dados <- read_excel("Análise classificatória/Wescheler.xlsx") # le em formato tibble
  head(dados)
  
  # converter tibble em data.frame
  dados <- data.frame(dados)
  # alterar nome das linhas para o ID do individuo
  rownames(dados) <- dados$Indivíduo
  
  # remover coluna individuo da analise
  dados <- subset(dados, select=-Indivíduo)
  
  
  ## a)
  agrupaKmeans <- kmeans(dados, 
                         4, # grupos
                         nstart = 25)
  agrupaKmeans
  # R2 = 84.9%
  
  # ver agrupamentos
  (gKmeans <- fviz_cluster(agrupaKmeans, 
                           data = dados,
                           main="K-means, R2=84.9%",
                           ggtheme = theme_bw()))
  
  
  
  ## b)
  32
  # Funcao para obter R2 de uma solucao de agrupamento hierarquico
  R2 <- function(dados, membros){  # input:
    # dados=dados usados no agrupamento, 
    # membros=indica o grupo a que pertence cada observacao
    # centrar variaveis
    dadosCentrados <- scale(dados, scale=F)
    # obter SQTotal = soma dos numeradores das variancias de cada variavel
    SQTotal <- sum(dadosCentrados^2)
    
    # obter as SQDentro dos grupos
    SQDentro <- sum(         # somar a SQ de todos os grupos
      # para cada grupo
      sapply(unique(membros),
             # obter o SQ dentro de cada grupo, i.e., o numerador da variancia de cada variavel
             function(k) {sum(scale(dados[membros == k, ], scale=F)^2)}  
      ))
    R2 <- 1 - (SQDentro / SQTotal)  # valor de R2
    return(list=c(SQTotal=SQTotal, SQDentro=SQDentro, R2_Perc=R2*100))
  }
  
  
  # matriz de distancias euclidiana
  (matriz.distancias <- dist(dados, method = "euclidean"))
  
  # 1) metodo da ligacao SIMPLES
  agrupamento <- hclust(matriz.distancias, method = "single")
  membros <- cutree(agrupamento, 5)           # solucao com 5 grupos
  R2(dados, membros)   # R2=90.1%
  (gSimples <- fviz_cluster(list(cluster=membros, data = dados),
                            main="SIMPLES, R2=90.1%", ggtheme = theme_bw()))
  # ou em alternativa
  #agrupamento <- hcut(dados, k = 5, hc_metric = "euclidean", 
  #                    hc_method = "single",
  #                    stand = F, graph=F)
  #R2(dados, agrupamento$cluster)  # R2=90.1%
  #(gSimples <- fviz_cluster(agrupamento, 
  #                          main="SIMPLES, R2=90.1%", ggtheme = theme_bw()))
  
  
  # 2) metodo da ligacao COMPLETA
  agrupamento <- hclust(matriz.distancias, method = "complete")
  membros <- cutree(agrupamento, 4) # solucao com 4 grupos
  R2(dados, membros)  # R2=84.9%
  (gCompleta <- fviz_cluster(list(cluster=membros, data = dados),
                             main="COMPLETA, R2=84.9%",
                             ggtheme = theme_bw()))
  
  # 3) metodo da ligacao MEDIA
  agrupamento <- hclust(matriz.distancias, method = "average")
  membros <- cutree(agrupamento, 4) # solucao com 4 grupos
  R2(dados, membros)  # R2=84.9%
  (gMedia <- fviz_cluster(list(cluster=membros, data = dados),
                          main="MEDIA, R2=84.9%",
                          ggtheme = theme_bw()))
  
  # 4) metodo da ligacao WARD
  agrupamento <- hclust(matriz.distancias, method = "ward.D2")
  membros <- cutree(agrupamento, 4) # solucao com 4 grupos
  R2(dados, membros)  # R2=84.9%
  (gWard <- fviz_cluster(list(cluster=membros, data = dados),
                         main="WARD, R2=84.9%",
                         ggtheme = theme_bw()))
  
  # comparar as solucoes
  library(gridExtra)
  grid.arrange(gKmeans, gSimples, gCompleta, gMedia, gWard, ncol=3)
  
  
  
  
  
#### ANÁLISE DISCRIMINANTE ####
### EXERCÍCIO 5.1 ###
  pop1 <- data.frame(g=1, x1=c(-2,0,-1), x2=c(5,3,1))
  pop2 <- data.frame(g=2, x1=c(0,2,1), x2=c(6,4,2))
  pop3 <- data.frame(g=3, x1=c(1,0,-1), x2=c(-2,0,-4))
  dados <- rbind(pop1, pop2, pop3)
  
  # constantes ou vetores que vao fazer falta
  # dimensao dos grupos
  (ni <- table(dados$g))     # dimensao de cada grupo
  grupos <- unique(dados$g)  # grupos
  n <- dim(dados)[1]         # numero total de observacoes
  p <- dim(dados)[2]-1       # numero de vars independentes
  g <- length(grupos)        # total de grupos
  
  
  # a) -----
  # medias por grupos
  with(dados, by(cbind(x1, x2), g, colMeans))
  
  
  # b) -----
  # matrizes variancias e covariancias por grupo
  with(dados, by(cbind(x1, x2), g, cov))
  
  # mas como obter varPooled
  VV <- matrix(0, ncol=p, nrow=p)   # iniciar matriz Spooled
  for (i in grupos){
    dadosg <- subset(dados, g==i, select=-g)
    covg <- cov(dadosg)
    VV <- VV + cov(dadosg)*(ni[g]-1)/(n-g)
  }
  # ver matriz Spooled
  (Spooled <- VV)
  
  
  # c) -----
  
  # obter Matrizes W e B
  # media geral
  media_geral <- colMeans(dados[,-1])
  # iniciar matrizes
  W <- matrix(0, ncol=p, nrow=p)   # matriz W
  B <- matrix(0, ncol=p, nrow=p)   # matriz B
  
  for (i in grupos){
    dadosg <- subset(dados, g==i, select=-g)
    mediasg <- colMeans(dadosg)
    W <- W + cov(dadosg) * (nrow(dadosg) - 1)
    difmedias <- matrix(mediasg - media_geral, ncol=1)
    B <- B + nrow(dadosg) * (difmedias %*% t(difmedias))
  }
  # ver matrizes W e B
  W
  B
  
  # alternativa para obte W
  # W <- (n-g)*Spooled
  
  
  # d) -----
  # Inversa de W
  Winv <- solve(W)
  Winv
  
  # W^{-1} * B
  Winv_B <- Winv %*% B
  Winv_B
  
  # Valores proprios reais
  eigen_result <- eigen(Winv_B)
  eigen_result$values
  
  # vetores proprios normalizados
  eigenvectors <- eigen_result$vectors  # Matriz cujas colunas são os vetores próprios
  eigenvectors
  
  
### EXERCÍCIO 5.2 ###
  # optativo: definir a diretoria atual como diretoria de trabalho
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  dados <- read.csv2("resultadoMat.csv", row.names=NULL, stringsAsFactors=TRUE)
  # eliminar variavel nota da base
  dados <- subset(dados, select=-nota)
  
  
  ## a) variaveis nhoras e autoconc -----
  
  ### i) -----
  
  # nuvem de pontos com indicacao do grupo
  gXY <- ggplot(data=dados, 
                aes(x=nhoras, y=autoconc, color=grupo))+ 
    geom_point(size=2, )+
    theme_classic()
  gXY
  
  # nuvem de pontos com letras em vez de pontos
  gXY <- ggplot(data=dados, 
                aes(x=nhoras, y=autoconc, label=grupo, color=grupo))+ 
    geom_text()+
    theme_classic()
  gXY
  # a var n horas diferencia claramente R e A
  # a variavel autoconceito diferencia claramente A e AR 
  # conclusao: nenhuma das vars diferencia totalmente os 3 grupos
  
  
  ### ii) -----
  library(MASS)
  fit <- lda(grupo ~nhoras + autoconc, data=dados)
  fit
  # maior contribuicao para D1: variavel autoconc 
  # maior contribuicao para D2: variaveis autoconc e nhoras
  # nota: estes coeficientes dependem da escala das variaveis
  # proportion of trace = % da variancia explicada em termos de diferenças entre grupos, por cada função discriminante
  # ou seja, a importancia relativa de cada funcao discriminante
  # 1a função 99.16%
  # 2a função 0.84%
  
  
  ### iii) -----
  # temos que ajustar como mlm para obtermos os coeficientes estandardizados
  fit1 <- lm(cbind(nhoras, autoconc) ~ factor(grupo),  data = dados)
  library(candisc)
  fitcan <- candisc(fit1)
  fitcan$coeffs.std
  # maior contribuicao relativa para D1: variavel autoconc 
  # maior contribuicao relativa para D2: variaveis nhoras
  
  # curiosidade: coeficientes originais da lda
  fitcan$coeffs.raw
  
  
  ### iv) -----
  n <- nrow(dados)                  # numero de observacoes
  p <- 2                            # numero de variaveis
  g <- length(unique(dados$grupo))  # numero de grupos
  
  # Teste lambda de Wilks
  # H0: nenhuma funcao tem poder discriminante
  # H1: pelo menos uma funcao tem poder discriminante
  lambdaT <- prod(1/(1+fitcan$eigenvalues))   # lambda de Wilks
  # estatística de teste
  (QQ <- -(n-(p+g)/2-1)*log(lambdaT) )
  # p-value
  (pvalue <- 1-pchisq(QQ,p*(g-1)))
  # rej H0 -> pelo menos 1 das funcoes tem poder disciminante -> repetir teste sem funcao 1
  
  # H0: a funcao 2 nao tem poder discriminante
  # H1: a funcao 2 tem poder discriminante
  lambdaT <- 1/(1+fitcan$eigenvalues[2])   # lambda de Wilks
  # estatística de teste
  (QQ <- -(n-(p+g)/2-1)*log(lambdaT) )
  # p-value
  (pvalue <- 1-pchisq(QQ,(p-1)*(g-1-1)))
  # nao rej H0 -> a funcao 2 nao tem poder discriminante
  
  
  ### v) -----
  library(klaR) 
  partimat(factor(grupo)~nhoras+autoconc, data = dados, method = "lda")
  # a vermelho identifica os elementos mal classificados
  # os pontos representam os centroides das zonas de classificacao
  
  
  ### vi) -----
  previstos <- predict(fit)
  library(caret)
  confusionMatrix(previstos$class, dados$grupo)
  # 2 elementos do grupo R foram classificados como AR
  # 2 elementos do grupo AR foram classificados como R
  # 81.8% dos elementos forem bem classificados (> 36.4% classificações corretas obtidas pelo acaso)
  # todos os elementos de A forem bem classificados
  # 75% dos elementos de AR forem bem classificados
  # 71.4% dos elementos de R forem bem classificados
  # 100% dos elementos que nao pertencem a A foram classificados como nao A
  # 85.7% dos elementos que nao pertencem a AR foram classificados como nao AR
  # 86.7% dos elementos que nao pertencem a R foram classificados como nao R
  
  # leave-one-out cross-validation 
  fitV <- lda(grupo ~nhoras + autoconc, data=dados, CV=T)
  table(fitV$class, dados$grupo)
  # tabela igual a obtida com a matriz de confusao, ou seja, 
  # continuamos a não conseguir classificar corretamente 2 AR e 2 R
  
  
  
  ## b) -----
  
  ### i) -----
  
  # 1. normalidade multivariada
  
  # via teste de Mardia
  library(MVN)
  mvn(dados[,c("nhoras","autoconc","dimensao", "rendimento", 
               "apoiofam","apoioprof")],
      mvnTest="mardia")
  # sem problemas de assimetria (p=0.872) e achatamento (p=0.338) multivariados 
  # rej. normalidade univariada para a variavel dimensao
  
  
  # via teste de Henze-Zirkler
  mvn(dados[,c("nhoras","autoconc","dimensao", "rendimento", 
               "apoiofam","apoioprof")], mvnTest="hz")
  # p=0.307 -> nao rej normalidade multivariada 
  
  
  # via QQplot
  mvn(dados[, c("nhoras","autoconc","dimensao", "rendimento", 
                "apoiofam","apoioprof")], 
      multivariatePlot = "qq")
  
  # via QQplot para avaliar normalidade e presenca de outliers
  # usa o quadrado das distancias de Mahalanobis das observacoes aos centroides
  cqplot(fitmlm)
  
  
  # via analise dos residuos da MANOVA
  fitmlm <- lm(cbind(nhoras,autoconc,dimensao, rendimento, apoiofam, apoioprof) ~ grupo, 
               data=dados)          # aplicar MANOVA
  residuos <- residuals(fitmlm)     # obter residuos
  residuos <- data.frame(residuos)  # converter residuos em data.frame
  residuos$grupo <- dados$grupo     # adicionar o grupo
  # testes à normalidade
  with(residuos, by(residuos$nhoras, grupo, shapiro.test))
  with(residuos, by(residuos$autoconc, grupo, shapiro.test))
  with(residuos, by(residuos$dimensao, grupo, shapiro.test))
  with(residuos, by(residuos$rendimento, grupo, shapiro.test))
  with(residuos, by(residuos$apoiofam, grupo, shapiro.test))
  with(residuos, by(residuos$apoioprof, grupo, shapiro.test))
  # todos com p>0.05 -> nao rej. hipotese de normalidade
  
  
  # 2. igualdade das matrizes de var-cov
  # ver matrizes de var-cov
  with(dados, 
       by(cbind(nhoras,autoconc,dimensao, rendimento, apoiofam,apoioprof), grupo, cov))
  
  # graficamente (recorrendo a elipses):
  #library(heplots)
  covEllipses(dados[,c("nhoras","autoconc","dimensao", "rendimento", "apoiofam","apoioprof")],  # variaveis independentes
              dados$grupo,   # variavel grupo
              fill=TRUE, pooled=FALSE, 
              col=c("blue", "red", "darkgreen"), # cores
              variables=1:3)    # grupos a considerar
  
  # pretendemos elipses com a mesma dimensao: grupo A parece diferir um pouco dos restantes
  table(dados$grupo)
  
  # via teste M de Box
  library(heplots)
  testeBox <- boxM(cbind(nhoras,autoconc,dimensao, rendimento, apoiofam,apoioprof) ~ factor(grupo), data=dados)
  # igual a 
  # fitmlm <- lm(cbind(nhoras,autoconc,dimensao, rendimento, apoiofam,apoioprof) ~ factor(grupo), data=dados)
  # testeBox <- boxM(fitmlm)
  testeBox
  # p=0.10 -> nao rej H0 -> podemos assumir a igualdade das matrizes de var-covar
  
  # extra
  testeBox$cov     # devolve as matrizes var-covar de cada grupo
  testeBox$pooled  # devolve a matriz Spooled
  
  # pela analise dos log determinantes: temos que ter n>p em cada grupo
  testeBox$logDet
  # com amostras grandes e n>>p podemos tambem comparar os IC para os logdeterminantes
  plot(testeBox)
  
  
  # 3. não colinearidade
  cor(dados[, -1])    # sem a variavel grupo
  # correlacoes > 0.8: (rendimento, autoconc) 
  
  
  
  ### ii) -----
  # metodo stepwise com criterio lambda de Wilks
  library(klaR)
  mod_step <- greedy.wilks(grupo ~ ., data = dados, niveau = 0.05)
  mod_step  # selecionou apenas autoconc + rendimento + apoioprof
  
  
  ### iii) -----
  fit <- lda(grupo ~autoconc + rendimento + apoioprof, data=dados)
  fit
  # maior contribuicao para D1: variavel autoconc 
  # maior contribuicao para D2: variaveis apoioprof
  # 98.27% da variancia (em termos das diferenças entre grupos) é explicada pela função 1 (D1)
  # 1.7% da variancia é explicada pela função 2 (D2) 
  
  
  ### iv) -----
  # ajustar como mlm
  fit1 <- lm(cbind(autoconc, rendimento, apoioprof) ~ factor(grupo),  data = dados)
  library(candisc)
  fitcan <- candisc(fit1)
  fitcan$coeffs.std
  # maior contribuicao relativa para D1: variaveis autoconc e rendimento 
  # maior contribuicao relativa para D2: variaveis apoioprof
  
  
  ### v) -----
  n <- nrow(dados)                  # numero de observacoes
  p <- 3                            # numero de variaveis
  g <- length(unique(dados$grupo))  # numero de grupos
  
  # Teste lambda de Wilks
  # H0: nenhuma funcao tem poder discriminante
  # H1: pelo menos uma funcao tem poder discriminante
  lambdaT <- prod(1/(1+fitcan$eigenvalues))   # lambda de Wilks
  # estatística de teste
  (QQ <- -(n-(p+g)/2-1)*log(lambdaT) )
  # p-value
  (pvalue <- 1-pchisq(QQ,p*(g-1)))
  # rej H0 -> pelo menos 1 das funcoes tem poder disciminante -> repetir teste sem funcao 1
  
  # H0: a funcao 2 nao tem poder discriminante
  # H1: a funcao 2 tem poder discriminante
  lambdaT <- 1/(1+fitcan$eigenvalues[2])   # lambda de Wilks
  # estatística de teste
  (QQ <- -(n-(p+g)/2-1)*log(lambdaT) )
  # p-value
  (pvalue <- 1-pchisq(QQ,(p-1)*(g-1-1)))
  # nao rej H0 -> a funcao 2 nao tem poder discriminante
  
  
  ### vi) -----
  library(klaR) 
  partimat(factor(grupo)~autoconc+rendimento+apoioprof, 
           data = dados, 
           method = "lda")
  # nota: sao 3 graficos
  
  
  ### vii) -----
  library(caret)
  previstos <- predict(fit)
  confusionMatrix(previstos$class, dados$grupo)
  # 1 R mal classificado
  # 1 AR mal classificado
  # 90.9% foram bem classificados > 36.4% classificação ao ao acaso
  # ...
  
  # validacao cruzada
  fitV <- lda(grupo ~autoconc+rendimento+apoioprof, data=dados, CV=T)
  table(fitV$class, dados$grupo)
  # 2 R mal classificados
  # 1 AR mal classificado
  # pretendiamos que os resultados se mantivessem. 
  # Se tal acontecesse, indicaria que as funções mantinham a sua capacidade de classificação perante novos dados.
  # Neste caso, isso não acontece, ou seja, as funções discriminantes ajustadas estao a subestimar a taxa de erro de classificação.
  
  
  
  
### EXERCÍCIO 5.4 ###
  # optativo: definir a diretoria atual como diretoria de trabalho
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  library(foreign)
  dados <- read.spss("emprestimos.sav", to.data.frame =T)
  
  
  ## criar conjuntos de dados ----------------------------------------------------
  
  summary(dados)  # Ha 150 NA incumprimento
  dados.completos <- na.omit(dados)
  (nvalido <- dim(dados.completos)[1])
  dadosClass <- subset(dados, is.na(incumpridor))   # dados para classificacao
  
  # dividir conjunto de dados: 75% para estimar funcao discriminante e 25% para testar
  set.seed(123) # divisao aleatoria dos dados
  linhas <- sample(1:2, nvalido,
                   replace = TRUE,
                   prob = c(0.75, 0.25))
  dadosA <- dados.completos[linhas==1,]  # dados para ajuste
  dadosT <- dados.completos[linhas==2,]  # dados para teste
  
  
  
  ## selecionar variaveis discriminantes -----------------------------------------
  
  cor(dadosA[,c(1, 3:8)])  # todas as correlacoes <0.8
  # metodo stepwise com criterio lambda de Wilks
  library(klaR)
  mod_step <- greedy.wilks(incumpridor ~ ., data = dadosA, niveau = 0.05)
  mod_step  # selecionou apenas txEnd + anosE + valorCC + anosR
  
  
  
  ## estimar funcao discriminante ------------------------------------------------
  # nota: so ha 1 funcao porque m=min(g-1, p)=1
  library(MASS)
  fit <- lda(incumpridor ~ txEnd + anosE + valorCC + anosR, data=dadosA, na.action="na.omit")
  fit
  # a variavel que mais discrimina e o valorCC
  
  
  
  ## validar pressupostos --------------------------------------------------------
  
  # 1. inexistencia de multicolinearidade
  cor(dadosA[,c("txEnd", "anosE", "valorCC", "anosR")])  
  # todas as correlacoes <0.8
  
  
  # 2. normalidade multivariada
  variaveis <- c("txEnd", "anosE", "valorCC", "anosR")
  # via teste de Mardia
  library(MVN)
  mvn(dadosA[, variaveis], mvnTest="mardia")
  # graves problemas de assimetria e achatamento
  # txEnd com assimetria elevada e com alguma leptocurtose
  # valorCC muito assimetrica e leptocurtica
  
  # via teste de Henze-Zirkler
  mvn(dadosA[, variaveis], mvnTest="hz")
  # p<0.001 -> rej normalidade multivariada 
  
  # via QQplot
  mvn(dadosA[, variaveis], multivariatePlot = "qq")
  # grande desvio
  
  # via QQplot para avaliar normalidade e presenca de outliers
  # usa o quadrado das distancias de Mahalanobis das observacoes aos centroides
  fitmlm <- lm(cbind(txEnd, anosE, valorCC, anosR) ~ incumpridor, 
               data=dadosA)          # aplicar MANOVA
  cqplot(fitmlm)
  # grande desvio
  
  # via analise dos residuos da MANOVA
  residuos <- residuals(fitmlm)     # obter residuos
  residuos <- data.frame(residuos)  # converter residuos em data.frame
  residuos$grupo <- dadosA$incumpridor     # adicionar o grupo
  # testes a normalidade
  with(residuos, by(txEnd, grupo, shapiro.test))
  with(residuos, by(anosE, grupo, shapiro.test))
  with(residuos, by(valorCC, grupo, shapiro.test))
  with(residuos, by(anosR, grupo, shapiro.test))
  # todos com p<0.05 -> rej. hipotese de normalidade
  
  # conclusao: rej normalidade multivariada
  # mas n(grupo menor) >= 20 e p<=5 e nestas condicoes a AD é robusta à violacao do pressuposto 
  
  
  # 3. igualdade das matrizes de var-cov
  # ver matrizes de var-cov
  with(dadosA, 
       by(cbind(txEnd, anosE, valorCC, anosR), incumpridor, cov))
  
  # graficamente (recorrendo a elipses):
  #library(heplots)
  covEllipses(dados$A[,variaveis],  # variaveis independentes
              dadosA$incumpridor,   # variavel grupo
              fill=TRUE, pooled=FALSE, 
              col=c("blue", "red"), # cores
              variables=1:4)        # variaveis independentes a considerar 
  # problemas pois nem sempre as elipses tem a mesma dimensao :(
  
  # via teste M de Box
  library(heplots)
  testeBox <- boxM(cbind(txEnd, anosE, valorCC, anosR) ~ incumpridor, data=dadosA)
  testeBox
  # p<0.001 -> rej H0 -> nao podemos assumir a igualdade das matrizes de var-covar
  
  # pela analise dos log determinantes: temos que ter n>p em cada grupo
  testeBox$logDet  
  plot(testeBox)   # os ICs nao se sobrepoem -> diferem
  
  # mas a AD e robusta à violacao dos pressupostos desde que:
  # * dimensao da amostra menor > numero de variaveis discriminantes
  table(dadosA$incumpridor)
  # n menor = 183 > 4 variaveis discriminantes
  # * medias dos grupos nao sao proporcionais às suas variancias
  medias <- aggregate(dadosA[,variaveis], list(dadosA$incumpridor), mean)  # medias por grupo
  desvpad <- aggregate(dadosA[,variaveis], list(dadosA$incumpridor), sd)  # medias por grupo
  as.matrix(medias[,-1])/as.matrix(desvpad[,-1]^2)*100
  # nao se mantem a constante de proporcionalidade
  # Portanto, podemos avancar com a AD
  
  
  
  ## Avaliar funcao discriminante linear -----------------------------------------
  
  # do ponto do vista do ajuste
  previstos <- predict(fit)
  library(caret)
  confusionMatrix(previstos$class, dadosA$incumpridor)
  # 82.8% classificacoes corretas > 73.9% ao acaso
  # sensibilidade elevada: 94.6%
  # especificidade baixa: 49.3%
  # problema na classificacao do grupo sim (errou mais do que acertou), na amostra usada para estimar AD
  
  library(klaR) 
  partimat(incumpridor ~ txEnd + anosE + valorCC + anosR, 
           data = dadosA, 
           method = "lda")
  # nota: sao 6 graficos
  
  # do ponto de vista da reacao a novas observacoes
  previstosT <- predict(fit, newdata = subset(dadosT, select=-incumpridor))
  confusionMatrix(previstosT$class, dadosT$incumpridor)
  # 78.1% classificacoes corretas > 73.6% ao acaso, mas a diferenca nao é significativa (p=0.1)
  # sensibilidade elevada: 95.4%
  # especificidade baixa: 29.8%
  # problema grave na classificacao do grupo sim (errou mais do que acertou), na amostra usada para testar a AD
  # Conclusao: a funcao nao discrimina corretamente novos elementos 
  
  
  
  ## Construir funcao discriminante quadratica -----------------------------------
  fitQ <- qda(incumpridor ~ txEnd + anosE + valorCC + anosR, data=dadosA, na.action="na.omit")
  fitQ
  
  
  ## Avaliar funcao discriminante quadratica -------------------------------------
  
  # do ponto do vista do ajuste
  previstos <- predict(fitQ)
  confusionMatrix(previstos$class, dadosA$incumpridor)
  # 80.0% classificacoes corretas > 73.9% ao acaso
  # sensibilidade elevada: 92.8%
  # especificidade baixa: 41.9%
  # problema na classificacao do grupo sim (errou mais do que acertou), na amostra usada para estimar AD
  
  #library(klaR) 
  partimat(incumpridor ~ txEnd + anosE + valorCC + anosR, 
           data = dadosA, 
           method = "qda")
  # nota: sao 6 graficos
  
  
  # do ponto de vista da reacao a novas observacoes
  previstosT <- predict(fitQ, newdata = subset(dadosT, select=-incumpridor))
  confusionMatrix(previstosT$class, dadosT$incumpridor)
  # 78.1% classificacoes corretas > 73.6% ao acaso, mas a diferenca nao é significativa (p=0.1)
  # sensibilidade elevada: 96.2%
  # especificidade baixa: 27.7%
  # problema grave na classificacao do grupo sim (errou mais do que acertou), na amostra usada para testar a AD
  # Conclusao: a funcao nao discrimina corretamente novos elementos e os resultados foram piores aos da versao linear 
  
  
  
  ## Construir funcao discriminante linear com variaveis transformadas -----------
  
  # que transformacao considerar?
  
  # por ex. com a análise dos histogramas
  summary(dadosA$txEnd)
  par(mfrow=c(1,2))
  hist(dadosA$txEnd)
  library(car)
  qqPlot(dadosA$txEnd)
  transf <- sqrt(dadosA$txEnd) 
  hist(transf); qqPlot(transf)
  
  summary(dadosA$anosE)
  hist(dadosA$anosE)
  qqPlot(dadosA$anosE)
  transf <- log(dadosA$anosE+1)
  hist(transf); qqPlot(transf)
  transf <- (dadosA$anosE+1)^.5
  hist(transf); qqPlot(transf)
  
  summary(dadosA$valorCC)
  hist(dadosA$valorCC)
  transf <- dadosA$valorCC^.25
  hist(transf); qqPlot(transf)
  transf <- log(dadosA$valorCC)
  hist(transf); qqPlot(transf)
  
  summary(dadosA$anosR)
  hist(dadosA$anosR)
  transf <- log(dadosA$anosR+1)
  hist(transf); qqPlot(transf)
  transf <- (dadosA$anosR+1)^.25
  hist(transf); qqPlot(transf)
  
  # Atraves do Box-Cox
  library(car)
  transfBC <- boxCox(dadosA$txEnd ~ 1)
  (lambda_otimo <- transfBC$x[which.max(transfBC$y)])  # aprox. 0.25
  
  transfBC <- boxCox(I(dadosA$anosE+1) ~ 1)
  (lambda_otimo <- transfBC$x[which.max(transfBC$y)])  # aprox. 0.5
  
  transfBC <- boxCox(dadosA$valorCC ~ 1)
  (lambda_otimo <- transfBC$x[which.max(transfBC$y)])  # aprox. 0
  
  transfBC <- boxCox(I(dadosA$anosR+1) ~ 1)
  (lambda_otimo <- transfBC$x[which.max(transfBC$y)])  # aprox. 0.25
  
  
  ## Transformar variaveis
  dadosA$txEndT <- (dadosA$txEnd)^.25
  dadosA$anosET <- log(dadosA$anosE+1)
  dadosA$valorCCT <- log(dadosA$valorCC) 
  dadosA$anosRT <- (dadosA$anosR+1)^.25
  
  
  ## avaliar pressupostos
  # 1. avaliar normalidade
  variaveisT <- c("txEndT", "anosET", "valorCCT", "anosRT")
  mvn(dadosA[, variaveisT], mvnTest="mardia")
  # sem problemas de assimetria
  
  # via teste de Henze-Zirkler
  mvn(dadosA[, variaveisT], mvnTest="hz")
  # p<0.001 -> rej normalidade multivariada 
  
  # via QQplot
  mvn(dadosA[, variaveisT], multivariatePlot = "qq")
  # ligeiros problemas, mas da para passar
  
  # via QQplot para avaliar normalidade e presenca de outliers
  # usa o quadrado das distancias de Mahalanobis das observacoes aos centroides
  fitmlm <- lm(cbind(txEndT, anosET, valorCCT, anosRT) ~ incumpridor, 
               data=dadosA)          # aplicar MANOVA
  cqplot(fitmlm)
  # passa
  
  # 2. avaliar igualdade das matrizes de var-cov
  # ver matrizes de var-cov
  with(dadosA, 
       by(cbind(txEndT, anosET, valorCCT, anosRT), incumpridor, cov))
  
  # graficamente (recorrendo a elipses):
  #library(heplots)
  covEllipses(dadosA[,variaveisT],  # variaveis independentes
              dadosA$incumpridor,   # variavel grupo
              fill=TRUE, pooled=FALSE, 
              col=c("blue", "red"), # cores
              variables=1:4)        # variaveis independentes a considerar 
  # pretendemos elipses com a mesma dimensao: parecem OK
  
  # via teste M de Box
  library(heplots)
  testeBox <- boxM(cbind(txEndT, anosET, valorCCT, anosRT) ~ incumpridor, data=dados)
  testeBox
  # p=0.055 -> nao rej H0 -> podemos assumir a igualdade das matrizes de var-covar
  
  # pela analise dos log determinantes: temos que ter n>p em cada grupo
  testeBox$logDet  # parecidos
  plot(testeBox)   # os ICs sobrepoem-se -> passa
  
  
  # estimar funcao discriminante linear
  fitT <- lda(incumpridor ~ txEndT + anosET + valorCCT + anosRT, data=dadosA, na.action="na.omit")
  fitT
  
  previstos <- predict(fitT)
  confusionMatrix(previstos$class, dadosA$incumpridor)
  # 80.1% classificacoes corretas > 74.0% ao acaso
  # sensibilidade elevada: 90.93%
  # especificidade baixa: 49.26%
  # problema na classificacao do grupo sim (errou mais do que acertou), na amostra usada para estimar AD
  
  #library(klaR) 
  partimat(incumpridor ~ txEndT + anosET + valorCCT + anosRT, 
           data = dadosA, 
           method = "lda")
  # nota: sao 6 graficos
  
  novos <- within(dadosT,{ 
    txEndT=(txEnd)^.25
    anosET=log(anosE+1)
    valorCCT=log(valorCC)
    anosRT=(anosR+1)^.25
  })
  
  
  previstosT <- predict(fitT, newdata = novos)
  confusionMatrix(previstosT$class, dadosT$incumpridor)
  # 76.4% classificacoes corretas > 73.6% ao acaso, mas a diferenca nao é significativa (p=0.1)
  # sensibilidade elevada: 93.1%
  # especificidade baixa: 29.8%
  # problema grave na classificacao do grupo sim (errou mais do que acertou), na amostra usada para testar a AD
  # Conclusao: a funcao nao discrimina corretamente novos elementos e os resultados foram piores aos da versao linear 
  
  
  ## classificacao ---------------------------------------------------------------
  # o modelo construido que teve melhor desempenho foi 1 funcao discriminante linear sem transformacoes
  previstos <- predict(fit, newdata = dadosClass)
  table(previstos$class)
  
  
  
  
  
#### REGRESSÃO MULTINOMIAL ####
### EXERCÍCIO 6.1 ###
  # criar tabela com dados
  tabela <- matrix(c(4229, 1381, 1046, 599), ncol=2, byrow=T)
  rownames(tabela) <- c("Sem vitimas", "Com vitimas")
  colnames(tabela) <- c("Dentro localidade", "Fora localidade")
  tabela
  
  ## a) ----- 
  # Teste do qui-quadrado de independência
  # H0: X e Y independentes vs H1: Há relação entre X e Y
  chisq.test(tabela)
  # p-value < 0.001 -> rej H0 
  # Aos niveis usuais de significância, há evidência estatística para afirmar que 
  # há relação entre a gravidade do acidente e o local onde este ocorre
  chisq.test(tabela)$expected  # freq. esperadas
  chisq.test(tabela)$stdres    # residuos estandardizados
  
  # se as amostras forem pequenas ou os pressupostos forem violados, usar teste de Fisher
  fisher.test(tabela)
  # p-value < 0.001 -> rej H0 -> (igual conclusão)
  # devolve OR = 1.75 = (a/c)/(b/d) = (tabela[1,1]/tabela[2,1]) / (tabela[1,2]/tabela[2,2])
  
  
  ## b) ---- 
  
  ## --- chances = odds
  (chances <- tabela[2,]/tabela[1,])
  
  ## Em alternativa podemos inverter estes valores, o que torna a interpretação mais "simpática" (pois a chance anterior <1):
  (chances.inv <- tabela[1,]/tabela[2,]) # = 1/chances
  
  
  (chances2 <- tabela[,2]/tabela[,1])
  ## Ou invertendo
  (chances2.inv <- tabela[,1]/tabela[,2]) # = 1/chances2
  
  
  # --- razao de chances = odd ratios
  (OR <- chances[1]/chances[2])
  # Em alternativa podemos inverter estes valores, o que torna a interpretação mais "simpática" (pois o OR anterior <1):
  1/OR
  
  (OR2 <- chances2[1]/chances2[2])
  # ou invertendo
  1/OR2
  
  
  # --- extra: alternativa que devolve resultado do teste Fisher, um OR e respectivo IC
  (odds.ratio(tabela))
  
  
  
### EXERCÍCIO 6.2 ###
  ## Pacotes necessarios ---------------------------------------------------------
  library(summarytools)     # para analise preliminar
  library(Hmisc)            # para analise preliminar
  library(crosstable)       # para analise preliminar
  library(gam)
  library(MASS)
  library(mfp)              # polinomios fracionarios: mfp e fp
  library(car)              # multicoliniearidade GVIF e medidas de influencia: 
  library(aod)              # teste de Wald: wald.test
  library(generalhoslem)    # teste de Hosmer e Lemeshow
  library(rms)              # teste de Cessie-van Houwelingen
  library(DescTools)        # pseudo-R2
  library(Epi)              # curva ROC e AUC
  library(caret)            # medidas de discriminação: confusionMatrix
  library(ggplot2)
  
  
  ## Ler dados -------------------------------------------------------------------
  
  # optativo: definir a diretoria atual como diretoria de trabalho
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  dados <- read.csv2("AFsub.csv", fileEncoding = "utf-8", 
                     stringsAsFactors = T, 
                     na.strings = "")
  str(dados)
  
  
  
  ## a) --------------------------------------------------------------------------
  ## Contar NA -------------------------------------------------------------------
  
  summary(dados) 
  # Ha 1 "vazio" em instrucao
  
  # alternativa
  apply(dados,                      # conjunto de dados
        2,                          # aplicar a função a cada coluna
        function(x) sum(is.na(x)))  # criar função com o que se pretende obter
  # neste caso contar o número de NA por coluna x
  
  # alternativa mais apelativa
  library(summarytools)
  view(dfSummary(dados)) # gráfico e pequeno resumo de todas as variáveis
  
  
  # Nota: se existirem valores omissos decidir se:
  # - eliminar variável com valores omissos
  # - eliminar linhas com valores omissos
  # - imputar valores aos NA
  # A escolha depende da quantidade de NA e do tipo de NA 
  
  dados <- na.omit(dados)  # permite remover linhas com NA
  
  
  
  ## b) --------------------------------------------------------------------------
  ## Criar variavel caminhada ----------------------------------------------------
  
  dados$caminhada <- factor(ifelse(dados$modalidade=="Caminhada", "Sim", "Não"))
  #confirmar se a variável foi criada corretamente
  table(dados$modalidade, dados$caminhada)
  
  
  
  ## c) --------------------------------------------------------------------------
  ## Análise univariada ------------------------------------------------------
  
  # Útil para verificar se temos observações suficientes por pares de categorias
  table(dados$caminhada, dados$sexo)
  table(dados$caminhada, dados$instrucao)
  by(dados$idade, dados$caminhada, summary)
  
  # Alternativa mais expedita
  library(Hmisc)
  summary(caminhada ~ sexo + instrucao + idade,  # var resposta ~ variáveis x
          data=dados,       # nome do conjunto de dados
          fun=table)        # função a usar
  
  # Alternativa mais expedita e apelativa, e com possibilidade de incluir teste
  library(crosstable)
  crosstable(dados,                     # nome do conjunto de dados
             c(sexo, instrucao, idade), # variáveis x
             by=caminhada,              # variável resposta 
             #total=TRUE,               # se quisermos os totais por linha e coluna
             test=TRUE,                 # TRUE para executar testes de independência ou igualdade das distribuições
             showNA='no',               # para mostrar NA, caso existam
             percent_digits=1) %>%      # número de casas decimais
    as_flextable()                      # para mostrar com aspeto mais apelativo
  
  
  # caso se pretenda trocar a categoria de referência de uma variável, por ex., instrucao
  # dados$instrucao <- relevel(dados$instrucao, ref="Superior")
  
  
  ### Também se podem ajustar os modelos univariados 
  # (equivalente a realizar o teste do qui-quadrado de independencia no caso das variaveis categoricas)
  
  ### 1o. ajustar modelo nulo -----
  
  # modelo nulo
  fit0 <- glm(caminhada ~ 1,    # ~ 1 indica que so se considera a constante (modelo nulo) 
              data = dados,                     # nome do conjunto de dados
              family = binomial(link = logit))  # distribuicao de Y e função de ligacao
  # ver modelo ajustado
  summary(fit0)
  
  # Nota: este modelo apenas tem o parametro b0. 
  # Modelo ajustado: logit(P(Y=1)) = logit(pi) = logit(P(caminhada=Sim)) = b0 = -0.6890
  # obs: na regressao logística, habitualmente b0 não tem significado.
  
  (OR <- exp(fit0$coefficients))
  # curiosidade: como surge o valor do OR? ver valores da tabela de contingencia
  # table(dados$caminhada) 
  # exp(b0) = 121/241 = (n Sim)/(n Não)
  
  (OR-1)*100      # (exp(-0.6890)-1)*100 
  # há 49.8% menos chances de ser praticante de caminhada
  
  # ou invertendo
  (1/OR)
  # Há aprox. 2x mais chances de não ser praticante de caminhadas
  
  
  ### 2o. Ajustar modelos univariados -----
  
  #### Variável idade (quantitativa)
  fit1 <- glm(caminhada ~ idade,  
              data = dados, family = binomial(link=logit))
  summary(fit1)  # p-value Wald=0.105
  # Modelo ajustado: logit(P(Y=1)) = logit(pi) = -3.241395 +0.055644*idade
  
  # Interpretação direta dos coeficientes beta
  (betas <- fit1$coefficients)  # coeficientes
  # b0=-3.24139531 -> logit(peso<2500gr) predito para residentes com idade 0
  # b(idade)=0.05564354 -> por cada aumento unitário na idade o logit(P(caminhada=Sim))
  # aumenta, em média, 0.05564354 
  # esta interpretação não é "simpática"...
  
  # Interpretação via OR (preferível)
  (OR <- exp(fit1$coefficients))
  # OR(idade)=1.05722076 > 1 -> (0.9501333 -1)*100% =5.7% 
  #   Há cerca de 5.7% mais possibilidades de ser praticante de caminhada por cada  
  #   aumento de 1 ano na idade do residente
  
  # Significância da variável
  # H0: variável idade significativa vs H1: variável idade não significativa
  # i) opção: via teste Wald
  #    H0: beta(idade)=0 vs H1: beta(idade) diferente de 0
  summary(fit1)  # p-value Wald < 0.001
  # ii) opção: via teste da razão de verosimilhanças (TRV)
  #     H0: modelo nulo vs H1: modelo com variável idade
  anova(fit0, fit1, test = "Chisq") # p-value TRV < 0.001 
  #     alternativa: apenas no caso univariado 
  #anova(fit1, test="Chisq")
  # Conclusão: como p-value < 0.2 -> reter para etapa multipla
  
  
  
  #### Variável sexo (dicotómica)
  # pela análise preliminar (alinea b) há observações suficientes e significativa
  fit1 <- glm(caminhada ~ sexo, 
              data = dados, family = binomial(link = logit))
  summary(fit1)  # p-value Wald 
  # Modelo ajustado: logit(P(Y=1)) = logit(pi) = -0.2814 - 0.9103*Masculino
  
  # Interpretação direta dos coeficientes beta
  (betas <- fit1$coefficients)  # coeficientes
  # b0=-0.2814125 -> logit(P(caminhada=Sim)) predito para residentes femininos
  # b(Masculino)=-0.9102903 -> se for masculino o logit(P(caminhada=Sim)) diminui 0.91
  # esta interpretação não é "simpática"...
  
  # Interpretação via OR (preferível)
  (OR <- exp(fit1$coefficients))
  # OR(beta(Masculino))= 0.4024074 < 1 -> (0.4024074 -1)*100% = -59.75926
  #  Há 59% menos de possibilidades de ser praticante de caminhada se for do sexo 
  #  masculino relativamente a ser do sexo feminino
  #
  # Curiosidade: como surgem os valores dos OR? ver valores da tabela de contingencia
  # table(dados$caminhada, dados$sexo)
  #   exp(b0) = ..
  
  # Significância da variável
  # H0: variável sexo não significativa vs. H1: variável sexo significativa
  # Na análise univariada coincide com a significância geral do modelo, i.e.,
  # H0: modelo nulo (low ~ 1) vs H1: modelo atual
  # i) Opção: via teste de Wald
  #    H0: b1=0 vs H1: b1 diferente de 0
  summary(fit1)
  # ii) Opção: via teste da razão de verosimilhanças (TRV) 
  #     H0: modelo nulo (low ~ 1) vs H1: modelo atual
  #     anova(modelo1, modelo2, test="Chisq")  # em que um dos modelos está encaixado no outro
  #                                            # test="Chisq" ou "LRT" devolvem o TRV
  anova(fit0, fit1, test="Chisq")  
  #     alternativa: apenas no caso univariado 
  anova(fit1, test="Chisq")
  #     p-value TRV =  
  # Conclusao: p-value < 0.2 -> reter para analise multipla
  
  
  
  #### Variável instrucao (categórica com >2 categorias)
  # pela análise preliminar (alinea b) há observações suficientes e significativa a 10%
  fit1 <- glm(caminhada ~ instrucao, 
              data=dados, family=binomial(link=logit))
  summary(fit1)  # Nota: 4 coeficientes para a variável instrucao 
  # Modelo ajustado: logit(P(Y=1)) = logit(pi) = 
  
  # Interpretação
  (OR <- exp(fit1$coefficients))
  #...
  
  
  # Avaliar a significância global da variável
  # H0: variável instrucao não significativa vs H1: variável instrucao não significativa
  # i) opção via teste RV
  #    H0: modelo nulo vs H1: modelo com variável race
  anova(fit0, fit1, test="Chisq") 
  # ou 
  # anova(fit1, test="Chisq")
  #     p-value TRV < 0.001
  # ii) opcao via teste de Wald
  #     H0: beta2=beta3=beta4=beta5=0 vs H1: pelo menos um dos betas (beta2 a beta5) diferente de zero
  library(aod)
  wald.test(vcov(fit1),  # matriz e variancias e covariancias
            coef(fit1),  # vector com coeficientes
            Terms = 2:5) # coeficientes a condiderar
  #     p-value Wald < 0.001
  # Conclusão: como p-value<0.2 -> reter para etapa multipla
  
  
  # Conclusão da análise univariada: todas as vars com p-value<.25 -> incluir todas na multipla
  
  
  
  #### ---- Uma alternativa mais rápida que usa o teste RV
  # 1o ajustar modelo nulo (fit0)
  # 2o usar a função add1 que vai percorrer todos os modelos univariados que indicarmos
  add1(fit0,  # modelo nulo 
       scope = ~ idade + instrucao + sexo, # lista das covariaveis
       test = "Chisq")   # teste TRV
  # igual a fazer várias vezes
  # anova(fit0, fitX, test="Chisq")  # sendo fitX o nome do modelo ajustado considerando a covar X
  
  
  
  
  ## c) --------------------------------------------------------------------------
  
  ### i) -------------------------------------------------------------------------
  ### Modelo logístico múltiplo preliminar ---------------------------------------
  ### inclui todas as vars que na análise univariada tiveram p-value <.20
  
  fit2 <- glm(caminhada ~ idade + instrucao + sexo,  
              data = dados, family = binomial(link = logit)) 
  summary(fit2)
  
  ### Remover variáveis não significativas 
  ## (remover por ordem decrescente de p-value do teste TRV no último modelo em estudo)
  
  drop1(fit2, test="Chisq") # remove individualmente cada variável
  # variável não significativa: instrucao -> remover
  
  fit2a <- glm(caminhada ~ idade + sexo,  
               data = dados, family = binomial(link = logit)) 
  # igual a 
  fit2a <- update(fit2, ~ . - instrucao)
  summary(fit2a)
  # ATENÇÃO: Devem-se confrontar os coeficientes do modelo com (fit2) e sem a 
  # variavel (fit2a). Se existirem alterações substanciais nos coeficientes, então 
  # não remover a variável.
  fit2$coef
  fit2a$coef
  # não há alterações relevantes :-)
  # opcional pois devolve o p-value obtido na instrução drop1
  anova(fit2, fit2a, test="Chisq")  # manter modelo mais simples fit2a 
  
  
  # ALTERNATIVA: selecionar variaveis com base no criterio AIC (NAO preferível)
  # objetivo: minimizar criterio AIC
  # metodos: forward, backward, stepwise (=both)
  # instrucao: step(modelo, scope, direction = c("both", "backward", "forward"))
  
  # pelo metodo backward: partindo do modelo multivariado
  step(fit2, direction = "backward")                # apresenta valores do AIC
  # ou
  step(fit2)  # por defeito aplica o método backward se não for indicado o argumento scope
  step(fit2, test="Chisq")  # também apresenta o teste RV
  # obs: nesta opção a variável ui manteve-se no modelo
  # se pretendermos guardar o modelo selecionado:
  (fit2.back <- step(fit2, test="Chisq"))  # backward via AIC e mostra teste RV
  summary(fit2.back)
  # modelo escolhido: caminhada ~ idade + sexo
  
  
  # pelo metodo forward
  (fit2.forw <- step(fit0, # ponto de partida: modelo nulo
                     scope = ~ idade + instrucao + sexo,  # modelo mais completo 
                     direction = "forward",
                     test="Chisq"))
  summary(fit2.forw)
  # modelo escolhido: caminhada ~ idade + sexo  
  
  # pelo metodo stepwise
  (fit2.step <- step(fit0, # ponto de partida: modelo nulo
                     scope = ~ idade + instrucao + sexo,  # modelo mais completo 
                     direction = "both",
                     test="Chisq"))
  # igual a
  (fit2.step <- step(fit0, # ponto de partida: modelo nulo
                     scope = ~ idade + instrucao + sexo,  # modelo mais completo 
                     test="Chisq"))  # por defeito aplica o método stepwise se for indicado o argumento scope
  summary(fit2.step)
  # modelo escolhido: caminhada ~ idade + sexo
  
  
  
  ### ii) ------------------------------------------------------------------------
  ### Avaliar junção de categorias -----------------------------------------------
  ### (só se aplica a variáveis com 3 ou + categorias)
  
  # i) Opção: via Intervalo de Confiança de Wald (IC)
  #    construir IC (de Wald) para a diferenca entre os coefs a juntar
  
  # ii) Opção: via Teste de Hipóteses (TH)
  #     via TH (de Wald) (equivalente ao IC anterior)
  #     H0: beta.K1=beta.K2 vs H1:beta.K1 diferente de beta.K2
  #library(aod) 
  #wald.test(vcov(modelo), # matriz e variancias e covariancias
  #          coef(modelo), # coeficiente
  #          L = cbind(0,0,1,...,-1,0,0,0)) # colocar 1 e -1 nos coeficientes a comparar
  
  # caso se juntem categorias, para comparar os modelos com e sem juncao de categorias
  #AIC(modelo.inicial, modelo.com.novas.ategorias)    # devolve o num parametros e o AIC de cada modelo
  
  
  
  ### iii) -----------------------------------------------------------------------
  ### Avaliar pressuposto de linearidade -----------------------------------------
  ### (só se aplica a variáveis quantitativas)
  
  # recordar o modelo modelo atual (alínea f)
  summary(fit2a)
  # Só há uma variável quantitativa: lwt
  
  # podemos avaliar por 3 métodos: i) lowess, ii) quartis, iii) polinómios fracionários 
  
  # i) opção: via metodo LOWESS (Mínimos quadrados ponderados localmente)
  plot(lowess(predict(fit2a) ~ dados$idade),  # valores logit preditos ~ valores observados para a var quantitativa
       type="l",                              # gráfico de linhas
       xlab="idade",                          # título do eixo x (nome da var quantitativa)
       ylab="logit = log(OR)")                # título do eixo y
  # Deve apresentar um comportamento linear
  # Perfeito! 
  
  
  # ii) opção: via metodo dos quartis
  #     1o categorizar a variável lwt em 4 classes com igual número de observacoes (i.e., usar quartis)
  dados$IdadeCat<- cut(dados$idade,                   # variavel a categorizar
                       breaks=quantile(dados$idade),  # pontos de corte das classes (min,Q1,Q2,Q3,max)
                       right=FALSE,                  # classes abertas a direita (mas e indiferente) 
                       include.lowest=TRUE)          # para garantir que min e max estao incluidos nas classes
  # igual a:
  #dados$IdadeCat<- cut(dados$idade,                # variavel a categorizar
  #                    breaks=c(min(dados$idade),  # lista com os pontos de corte das classes: comeca no minimo e acaba no maximo
  #                             quantile(dados$idade, 0.25), 
  #                             quantile(dados$idade, 0.5), 
  #                             quantile(dados$idade, 0.75), 
  #                             max(dados$idade)),
  #                    right=FALSE, include.lowest=TRUE) 
  table(dados$IdadeCat)
  #     2o ajustar modelo substituindo a variável quantitativa pela versão categorizada
  fit3a <- update(fit2a, ~ . - idade + IdadeCat)
  # igual a 
  #fit3b <- glm(caminhada ~ sexo + IdadeCat,
  #             data = dados, family = binomial(link = logit))
  summary(fit3a)
  #     3o avaliar linearidade dos coeficientes
  #     um gráfico ajuda a visualizar: para isso precisamos dos pontos médios das classes: (linf + lsup)/2
  x <- c((min(dados$idade) + quantile(dados$idade, 0.25))/2, 
         (quantile(dados$idade, 0.25) + quantile(dados$idade, 0.5))/2, 
         (quantile(dados$idade, 0.5) + quantile(dados$idade, 0.75))/2, 
         (quantile(dados$idade, 0.75) + max(dados$idade))/2)
  y <- c(0, as.numeric(fit3a$coef[3:5])) # coeficientes associados a IdadeCat
  plot(x, y, type="b")
  # ou com linha suavizada
  plot(x, y)
  lines(lowess(x,y))  
  # Deve apresentar um comportamento linear
  # Existe um pequeno desvio da linearidade. 
  # Não parece ser suficiente para a colocar verdadeiramente em causa o pressuposto
  # Devemos sempre conjugar este resultado com o de outros métodos e só depois decidir
  
  
  # iii) opção: via método dos polinómios fracionários
  library(mfp)
  fit3b <- mfp(caminhada ~ fp(idade) + sexo,  # usar a função fp na variável quantitativa
               data = dados, family = binomial(link = logit))
  summary(fit3b)
  # Transformação sugerida (idade/100)^.5, ou seja, trata-se uma transformacao linear (/100)
  # e uma não linear, 1/sqrt! 
  
  # Se for sugerida uma transformacao então:
  # 1) comparar os dois modelos usando o valor p calculado a partir de 1-(pchisq(deviance(modelo.original)-deviance(modelo.com.variavel.transformada)), gl)
  #    com gl=1 no caso de ser uma transformação simples, ou gl=3 no caso de sugerir a transformação dupla
  1-pchisq(deviance(fit2a)-deviance(fit3b), 1)  # p=0.005 -> optar pelo modelo com vaiavel tansformada
  1-pchisq(deviance(fit2a)-deviance(fit3a), 1)  # p=0.004 -> optar pelo modelo com vaiavel tansformada
  # 2) comparar também os AIC dos dois modelos: AIC(modelo.original, modelo.com.variavel.transformada) 
  AIC(fit2a, fit3b)
  # nota: se não há concordância entre os métodos, devemos sempre ter em atenção a interpretação 
  
  
  ### iv) ------------------------------------------------------------------------
  ### Multicolinariedade ---------------------------------------------------------
  library(car)
  vif(fit2a)
  # todos os VIF estao proximos de 1 logo nao ha problemas de multicolinearidade
  
  
  ### v) -------------------------------------------------------------------------
  ### Intracções -----------------------------------------------------------------
  
  fit4 <- update(fit2a, ~ . + sexo:idade)
  summary(fit4)  
  anova(fit2a, fit4, test="Chisq")
  # interacao não significativa
  
  # modelo final para proximas etapas
  fitF <- fit2a
  
  
  ### vi) ------------------------------------------------------------------------
  ### Adequabilidade e bondade do ajustamento ------------------------------------
  
  # --- significância geral do modelo
  # H0: modelo nulo vs H1: modelo atual
  anova(fit0, fitF)
  # há pelo menos 1 parametro significativo
  
  
  # --- bondade do ajustamento
  # opções: 
  # i) Teste de Hosmer-Lemeshow: mais útil quando há vários preditores e/ou preditores quantitativos
  # ii) Teste de Cessie-van Houwelingen: quando há pelo menos 1 preditor quantitativo
  # iii) estatistica deviance: quando só temos variáveis categóricas
  # iv) Coeficientes pseudo-R2 de Nagelkerke e de McFadden (os mais usuais)
  # v) Coeficiente Brier 
  
  # i) opção: Teste de Hosmer-Lemeshow
  # H0: o modelo ajusta-se aos dados vs H1: o modelo não se ajusta aos dados
  library(generalhoslem)          # ativar pacote necessario
  (hl<-logitgof(dados$caminhada,  # valores observados y (dependente)
                fitted(fitF),    # valores ajustados y^ 
                g = 10))          # numero de classes a considerar
  # nao esquecer que se trata de um teste QQ, logo observar os valores esperados para averiguar se é necessario considerar g<10
  hl$expected                     # valores esperados
  # sem problemas
  # nota: logitgof tem as versões adaptadas para modelos logisticos, multinomiais e ordinais
  
  # exitem pacotes alternativos:
  #library(ResourceSelection)
  #hoslem.test(as.numeric(dados$caminhada)-1, fitted(fitF), g=10) # so para modelo logistico
  
  # conclusão: o modelo ajusta-se aos dados
  
  
  # ii) opção: Teste de Cessie e van Houwelingen
  # H0: o modelo ajusta-se aos dados vs H1: o modelo não se ajusta aos dados
  library(rms) 
  fitF.1 <- lrm(caminhada ~ idade + sexo,
                data = dados,            # nome do conjunto de dados
                x = TRUE, y = TRUE)      # para podermos usar a instrucao resid()
  # lrm: funcao alternativa para ajustar um modelo logístico ou um modelo ordinal
  print(fitF.1)  # opcional: para visualizar o modelo ajustado. Neste caso não se usa a funçã summary
  # Model Likelihood Ratio Test: devolve teste a significância geral do modelo
  # Discrimination Indexes: R2 = Nagelkerke
  
  # teste de Cessie
  resid(fitF.1, 'gof')  # "gof" para devolver o teste de ajustamento de Cessie e van Houwelingen
  # p-value = 0.104 -> o modelo ajusta-se aos dados 
  
  # iii) opção: teste Deviance (SE NÃO só tivessemos variáveis categóricas)
  resumo <- summary(fitF)           # modelo ajustado
  (gl <- resumo$df.residual)        # graus de liberdade do qui-quadrado
  (ET <- resumo$deviance)           # Deviance
  (valorp <- 1 - pchisq(ET,gl))     # valor p
  
  # v) opção: coeficientes pseudo-R2 de Nagelkerke e de McFadden
  print(fitF.1)  # Discrimination Indexes: R2 = Nagelkerke
  
  # outros pseudo-R2
  library(DescTools)
  DescTools::PseudoR2(fitF, which = c("McFadden", "CoxSnell", "Nagelkerke"))
  #  McFadden   CoxSnell Nagelkerke 
  # 0.1859167  0.2109407  0.2928212 
  # R2 de McFadden= 0.1859 -> 18.6% de ganho de informacao do modelo fitF em relacao ao modelo nulo
  # ou em alternativa
  #library(pscl)
  #pR2(fitF)
  
  
  # iv) opção: coeficiente Brier score
  #     ( 0 <= Brier <= 1, quanto mais proximo de 0 melhor)
  #     = media(erros^2): mede a precisão das predições. Queremos que esteja próximo de zero
  #     NÃO usar quando estamos perante eventos muitos raros, i.e, quando o número de sucessos ou insucessos é muito pequeno
  print(fitF.1)
  # Brier= 0.175 -> próximo de 0, o que é bom
  
  
  
  ### vii) -----------------------------------------------------------------------
  ###  Capacidade discriminativa -------------------------------------------------
  
  # -- curva ROC
  library(Epi)
  ROC(form=caminhada ~idade+sexo,
      dat=dados,
      plot="sp")
  # o ponto de corte é obtido onde se cruza a sensibilidade e especificidade
  
  # versão com valor do ponto de corte
  ROC(form = caminhada~sexo+idade,
      data=dados, 
      plot="ROC",
      PV=T, MX=T,
      AUC=T)
  # AUC=0.796 (para ponto de corte 0.296) -> capacidade discriminativa razoável/boa
  # devolve a P(caminhar|caminha) > P(caminhar|não caminha), a um indivíduo escolhido ao acaso
  # Para um ponto de corte optimo=0.296 tem-se uma sensibilidade de 79.3% e uma especificadade de 69.3%
  
  # matriz de confusão
  # 1o obter probabilidades preditas
  probsPred <- predict(fitF, type="response")  # prob prevista
  # 2o obter a classificacao com base no ponto de corte obtido na analise da curva ROC
  pontoCorte <- 0.296
  categoriaPred <- as.factor(ifelse(probsPred >= pontoCorte, "Sim", "Não"))
  # atencao: categoriaPred tem que estar no mesmo formato da variavel Y
  
  library(caret)
  confusionMatrix(data = categoriaPred, dados$caminhada)
  # problema: sensibilidade e especificidade trocadas relativamente a ROC
  # isto acontece porque ROC considera a categoria de ref a última, e o caref considera a 1a
  confusionMatrix(data = categoriaPred, dados$caminhada, 
                  positive = "Sim")  # indicar explicitamente a categoria de referencia  
  
  # se quisermos aumentar a especificidade, basta alterar o ponto de corte
  pontoCorte <- 0.33
  categoriaPred <- as.factor(ifelse(probsPred >= pontoCorte, "Sim", "Não"))
  confusionMatrix(data = categoriaPred, dados$caminhada, 
                  positive = "Sim")  # indicar explicitamente a categoria de referencia  
  # claro que muda a sensibilidade, bem como as outras medidas...
  
  
  
  ## e) --------------------------------------------------------------------------
  
  ### i) -------------------------------------------------------------------------
  ### Equação do modelo ----------------------------------------------------------
  fitF$coefficients
  
  ### ii) ------------------------------------------------------------------------
  ### Intervalos de confiança ----------------------------------------------------
  confint(fitF)
  
  ### iii) -----------------------------------------------------------------------
  ### Estimar probabilidades -----------------------------------------------------
  
  # dados com o perfil pretendido
  novosF <- data.frame(idade=c(30, 40, 50), sexo="Feminino")
  # probabilidade predita
  predict(fitF, newdata = novosF, type = "response")
  
  ### iv) ------------------------------------------------------------------------
  ### Perfis de probabilidade ----------------------------------------------------
  
  # dados com o perfil pretendido
  novosFM <- data.frame(idade=rep(seq(15, 80, 1), 2), 
                        sexo=as.factor(c(rep("Feminino", 66), 
                                         rep("Masculino",66))))  # nao esquecer de definir como fator
  # juntar aos dados anteriores os logit preditos
  novosFM <- cbind(novosFM, 
                   predict(fitF, newdata = novosFM, type = "link", se = TRUE))
  # juntar aos dados anteriores os limites dos IC a 90%
  novosFM <- within(novosFM, {
    ProbPrev <- plogis(fit)
    LL <- plogis(fit - qnorm(0.95) * se.fit)
    UL <- plogis(fit + qnorm(0.95) * se.fit)
  })
  # grafico com evolucao do perfil de probabilidade dos homens e mulheres com idade entre 15 e 80
  library(ggplot2)
  ggplot(novosFM, aes(x = idade, y = ProbPrev)) + 
    geom_ribbon(aes(ymin = LL,ymax = UL, fill = sexo), alpha = 0.2) + 
    geom_line(aes(colour = sexo), size = 1) +
    xlab("Idade") +                                 # titulo do eixo x
    ylab("Probabilidade prevista de caminhar")      # titulo do eixo y
  
  
### EXERCÍCIO 6.3 ###
  ## Pacotes necessarios ---------------------------------------------------------
  library(Hmisc)
  library(crosstable)
  library(forcats)
  library(mlogit)    # regressao multinomial
  library(lmtest)    # teste RV para modelos mlogit
  library(mfp)
  library(gam)
  library(MASS)
  library(summarytools)
  library(car)
  library(aod)
  library(generalhoslem)
  library(rms) 
  library(DescTools)
  library(Epi)
  library(caret)
  library(ggplot2)
  library(nnet)
  library(gridExtra)
  
  
  ## Ler dados -------------------------------------------------------------------
  
  # optativo: definir a diretoria atual como diretoria de trabalho
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  dados <- read.csv2("AFsub.csv", fileEncoding = "utf-8", 
                     stringsAsFactors = T, 
                     na.strings = "")
  
  # recordar que havia um NA em instrução
  dados <- na.omit(dados)  # permite remover linhas com NA
  
  
  
  ## a) --------------------------------------------------------------------------
  ## Análise preliminar ------------------------------------------------------
  
  # Útil para verificar se temos observações suficientes por pares de categorias
  table(dados$modalidade, dados$sexo)
  table(dados$modalidade, dados$instrucao)  # problemas: poucas observacoes no 2o ciclo -> decidir juntar com 1o ou 3o ciclo
  by(dados$idade, dados$modalidade, summary)
  
  # problemas com instrucao!!!
  # nota summary de Hmisc e crosstable de crosstable dão erro quando não há observações suficientes
  
  # juntar categorias de instrução
  #library(forcats)
  dados$instrucao2 <- fct_recode(dados$instrucao, 
                                 "3.º ciclo ou inferior" = "1.º ciclo ou inferior",
                                 "3.º ciclo ou inferior" = "2.º ciclo",
                                 "3.º ciclo ou inferior" = "3.º ciclo")
  
  #library(crosstable)
  crosstable(dados,                     # nome do conjunto de dados
             c(sexo, instrucao2, idade), # variáveis x
             by=modalidade,              # variável resposta 
             #total=TRUE,               # se quisermos os totais por linha e coluna
             test=TRUE,                 # TRUE para executar testes de independência ou igualdade das distribuições
             showNA='no',               # para mostrar NA, caso existam
             percent_digits=1) %>%      # número de casas decimais
    as_flextable()                      # para mostrar com aspeto mais apelativo
  # todas significativas
  
  
  ## b) --------------------------------------------------------------------------
  ## Modelos univariados ---------------------------------------------------------
  
  # packages para ajustar modelo multinomial:
  # i) library(mlogit)
  #    fit1 <- mlogit(y ~ 1 | x, refevel="categoriaRef", data = dados, shape="wide")
  #    Seja qual for a ordem das categorias de y, permite definir qual é a categoria
  #    de referência. Se nada for referido, considera a 1a
  # ii) library(nnet)
  #     fit1 <- multinom(y ~ x, data = dados)
  #     Atencao: neste caso nao devolve automaticamente o teste de Wald a significancia dos coeficientes
  #     Considera a 1a ctegoria como a referênica
  # iii) library(VGAM)
  #      fit1 <- vglm(y ~ x, data = dados, family=multinomial). 
  #      Atencao: neste caso nao devolve automaticamente o teste de Wald a significancia dos coeficientes
  #      Considera a ultima categoria como referência
  
  ### modelo nulo
  fit0 <- mlogit(modalidade ~ 1,             # variavel y~1 | covariaveis
                 reflevel = "Caminhada",     # indicar qual a categoria y de referencia
                 data = dados, shape="wide") # para informar o R que a base de dados tem um individuo por linha
  summary(fit0)
  
  
  #### Variável idade (quantitativa)
  fit1 <- mlogit(modalidade ~ 1 | idade,     # variavel y~1 | covariaveis
                 reflevel = "Caminhada",     # indicar qual a categoria y de referencia
                 data = dados, shape="wide")
  summary(fit1)
  # signifcancia geral do modelo (teste RV): p-value<2.22e-16
  # avaliar significancia geral da variavel Idade:
  anova(fit0, fit1)  # não suporta objetos mlogit
  # alternativa
  library(lmtest)
  lrtest(fit0, fit1)
  
  # ou
  #library(nnet)
  fit1b <- multinom(modalidade ~  idade, data = dados)
  summary(fit1b)
  # para podermos comparar resultados dos modelos, é preciso garantir que considera a mesma categoria de referência
  dados$modalidade <- relevel(dados$modalidade, ref="Caminhada")
  fit1b <- multinom(modalidade ~  idade, data = dados)
  summary(fit1b)
  #library(lmtest)
  fit0b <- multinom(modalidade ~  1, data = dados)
  lrtest(fit0b, fit1b) # p<0.001
  # ou
  anova(fit0b, fit1b)  # p<0.001
  
  # ou
  #library(VGAM)
  fit1c <- vglm(modalidade ~  idade, family=multinomial, data = dados)
  summary(fit1c)
  # para podermos comparar resultados dos modelos, é preciso garantir que considera a mesma categoria de referência
  dados$modalidade <- factor(dados$modalidade, levels=c("BTT/Ciclismo", "Fitness", "Outra", "Caminhada"))
  fit1c <- vglm(modalidade ~  idade, family=multinomial, data = dados)
  summary(fit1c)
  fit0c <- vglm(modalidade ~  1, family=multinomial, data = dados)
  lrtest(fit0c, fit1c)  # não suporta objetos vglm
  anova(fit0c, fit1c)   # não suporta objetos vglm
  
  
  #### Variável sexo (dicotómica)
  # pela análise preliminar há observações suficientes
  fit1 <- mlogit(modalidade ~ 1 | sexo,         # variavel y~1 | covariaveis
                 reflevel = "Caminhada",       # opcional: indicar qual a categoria y de referencia
                 data = dados, shape="wide")
  summary(fit1)
  
  
  #### Variável instrucao (categórica com >2 categorias)
  # pela análise preliminar NÃO há observações suficientes
  fit1 <- mlogit(modalidade ~ 1 | instrucao,         # variavel y~1 | covariaveis
                 reflevel = "Caminhada",            # opcional: indicar qual a categoria y de referencia
                 data = dados, shape="wide")
  summary(fit1)  
  # reparar no coef e SD de 2º ciclo
  
  #### Variável instrucao2 (categórica com >2 categorias)
  # pela análise preliminar HÁ observações suficientes
  fit1 <- mlogit(modalidade ~ 1 | instrucao2,         # variavel y~1 | covariaveis
                 reflevel = "Caminhada",            # opcional: indicar qual a categoria y de referencia
                 data = dados, shape="wide")
  summary(fit1)
  
  # Conclusão da análise univariada: todas as vars com p-value<.25 -> incluir todas na multipla
  
  
  
  ## c) --------------------------------------------------------------------------
  ## Modelo multinomial múltiplo -------------------------------------------------
  
  ### Modelo multiplo preliminar
  ### inclui todas as vars que na análise univariada tiveram p-value <.20
  
  fit2 <- mlogit(modalidade ~ 1 | idade + sexo + instrucao2,         # variavel y~1 | covariaveis
                 reflevel = "Caminhada",            # opcional: indicar qual a categoria y de referencia
                 data = dados, shape="wide")
  summary(fit2)
  
  
  ### Remover variáveis não significativas 
  ## (remover por ordem decrescente de p-value do teste TRV no último modelo em estudo)
  
  # pelos p-values sera que instrucao pode ser removida?
  fit2a <- mlogit(modalidade ~ 1 | idade + sexo,
                  reflevel = "Caminhada",
                  data = dados, shape="wide")
  summary(fit2a)
  lrtest(fit2, fit2a)  # p=0.1482 -> remover instrucao2
  
  # e se removermos sexo?
  fit2b <- mlogit(modalidade ~ 1 | idade + instrucao2,
                  reflevel = "Caminhada",
                  data = dados, shape="wide")
  summary(fit2b)
  lrtest(fit2, fit2b)  # p<0.001 -> nao remover sexo
  
  # e se removermos idade?
  fit2c <- mlogit(modalidade ~ 1 | sexo + instrucao2,
                  reflevel = "Caminhada",
                  data = dados, shape="wide")
  summary(fit2c)
  lrtest(fit2, fit2c)  # p<0.001 -> nao remover idade
  
  # conclusão: remover instrucao2
  
  # Nota: se ajustarmos o modelo recorrendo a vglm então podmeos usar o drop1 para
  # remover individualmente cada variável 
  fit2 <- vglm(modalidade ~  idade + sexo + instrucao2, family=multinomial, data = dados)
  drop1.vglm(fit2, test="LRT")
  
  
  ## d) --------------------------------------------------------------------------
  ## Interações -------------------------------------------------
  
  fit2a <- mlogit(modalidade ~ 1 | idade + sexo,
                  reflevel = "Caminhada",
                  data = dados, shape="wide")
  fit3 <- mlogit(modalidade ~ 1 | idade * sexo,
                 reflevel = "Caminhada",
                 data = dados, shape="wide")
  summary(fit3)
  lrtest(fit3, fit2a)  # p=0.1226 -> nao considerar interacao
  
  
  ## e) --------------------------------------------------------------------------
  ## Junção de categorias e linearidade ------------------------------------------
  
  
  # construir modelos logisticos: BTT vs caminhada, Fitness vs Caminhada,
  # Outra vs Caminhada para:
  # a) averiguar possibilidade de juncao de categorias (para categoricas com >2 categorias)
  # b) pressuposto de linearidade com logit (para quantitativas)
  
  # BD1) base de dados para BTT vs caminhada
  dados.1 <- subset(dados, modalidade %in% c("BTT/Ciclismo", "Caminhada")) 
  dados.1$modalidade <- factor(dados.1$modalidade)  # remover niveis sem observacoes
  dados.1$modalidade <- relevel(dados.1$modalidade, ref="Caminhada")
  ## Modelo logístico associado
  fit.l1 <- glm(modalidade ~ idade + sexo, family = binomial(link = logit), data=dados.1)
  summary(fit.l1)
  
  # BD2) base de dados para BTT vs caminhada
  dados.2 <- subset(dados, modalidade %in% c("Fitness", "Caminhada")) 
  dados.2$modalidade <- factor(dados.2$modalidade)  # remover niveis sem observacoes
  dados.2$modalidade <- relevel(dados.2$modalidade, ref="Caminhada")
  ## Modelo logístico associado
  fit.l2 <- glm(modalidade ~ idade + sexo, family = binomial(link = logit), data=dados.2)
  summary(fit.l2)
  
  # BD3) base de dados para BTT vs caminhada
  dados.3 <- subset(dados, modalidade %in% c("Outra", "Caminhada")) 
  dados.3$modalidade <- factor(dados.3$modalidade)  # remover niveis sem observacoes
  dados.3$modalidade <- relevel(dados.3$modalidade, ref="Caminhada")
  ## Modelo logístico associado
  fit.l3 <- glm(modalidade ~ idade + sexo, family = binomial(link = logit), data=dados.3)
  summary(fit.l3)
  
  summary(fit2a)
  # Nota: os coeficientes dos logits idade e sexo do modelo multinomial são muito 
  # idênticos aos coeficientes dos logits dos modelos binomias 2 e 3
  
  
  ## a) juncao de categorias: neste caso nao se aplica
  # o que fazer:
  # i) comparar coeficientes dentro de cada modelo
  # ii) usar teste de wald para comparar
  # iii) comparar modelos recorrendo ao AIC ou BIC
  
  library(car)
  crPlot(fit.l1, variable="idade")
  crPlot(fit.l2, variable="idade")
  crPlot(fit.l3, variable="idade")
  
  ## b) linearidade
  # i) opção: via metodo LOWESS (Mínimos quadrados ponderados localmente)
  plot(lowess(predict(fit.l1) ~ dados.1$idade), xlab="idade",  
       ylab="logit = log(OR)", type="l")
  plot(lowess(predict(fit.l2) ~ dados.2$idade), xlab="idade",  
       ylab="logit = log(OR)", type="l")
  plot(lowess(predict(fit.l3) ~ dados.3$idade), xlab="idade",  
       ylab="logit = log(OR)", type="l")
  # todas apresentam um comportamento linear -> ok
  
  # ii) opção: via metodo dos quartis
  #     1o categorizar a variável idade em 4 classes com igual número de observacoes (i.e., usar quartis)
  dados.1$IdadeCat<- cut(dados.1$idade, breaks=quantile(dados.1$idade), right=FALSE, include.lowest=TRUE)
  dados.2$IdadeCat<- cut(dados.2$idade, breaks=quantile(dados.2$idade), right=FALSE, include.lowest=TRUE)
  dados.3$IdadeCat<- cut(dados.3$idade, breaks=quantile(dados.3$idade), right=FALSE, include.lowest=TRUE)
  table(dados.1$IdadeCat)
  table(dados.2$IdadeCat)
  table(dados.3$IdadeCat)
  #     2o ajustar modelos substituindo a variável quantitativa pela versão categorizada
  fit.l1a <- update(fit.l1, ~ . - idade + IdadeCat)
  fit.l2a <- update(fit.l2, ~ . - idade + IdadeCat)
  fit.l3a <- update(fit.l3, ~ . - idade + IdadeCat)
  summary(fit.l1a)
  summary(fit.l2a)
  summary(fit.l3a)
  #     3o avaliar linearidade dos coeficientes
  #     um gráfico ajuda a visualizar: para isso precisamos dos pontos médios das classes: (linf + lsup)/2
  pontosmedios <- function(variavel){
    c((min(variavel) + quantile(variavel, 0.25))/2, 
      (quantile(variavel, 0.25) + quantile(variavel, 0.5))/2, 
      (quantile(variavel, 0.5) + quantile(variavel, 0.75))/2, 
      (quantile(variavel, 0.75) + max(variavel))/2)
  }
  x <- pontosmedios(dados.1$idade)
  y <- c(0, as.numeric(fit.l1a$coef[3:5])) # coeficientes associados a IdadeCat
  plot(x, y, type="b")
  # Existe um pequeno desvio da linearidade. 
  x <- pontosmedios(dados.2$idade)
  y <- c(0, as.numeric(fit.l2a$coef[3:5])) # coeficientes associados a IdadeCat
  plot(x, y, type="b")
  # desvio da linearidade 
  x <- pontosmedios(dados.3$idade)
  y <- c(0, as.numeric(fit.l3a$coef[3:5])) # coeficientes associados a IdadeCat
  plot(x, y, type="b")
  # desvio da linearidade 
  
  # iii) opção: via método dos polinómios fracionários
  #library(mfp)
  fit.l1b <- mfp(modalidade ~ fp(idade) + sexo,  # usar a função fp na variável quantitativa
                 data = dados.1, family = binomial(link = logit))
  summary(fit.l1b)
  # sugere transformacao linear (/100)
  fit.l2b <- mfp(modalidade ~ fp(idade) + sexo,  # usar a função fp na variável quantitativa
                 data = dados.2, family = binomial(link = logit))
  summary(fit.l2b)
  # sugere transformacao não linear (^-.5)
  fit.l3b <- mfp(modalidade ~ fp(idade) + sexo,  # usar a função fp na variável quantitativa
                 data = dados.3, family = binomial(link = logit))
  summary(fit.l3b)
  # sugere transformacao não linear (^-1)
  
  
  
  # iv) opção: usando a formulação gam
  library(mgcv)
  fit.l1c <- gam(modalidade ~ s(idade) + sexo,  # usar a função fp na variável quantitativa
                 data = dados.1, family = binomial(link = logit))
  summary(fit.l1c)
  # edf=1 -> transformação linear
  # sugere transformacao linear (/100)
  fit.l2c <- gam(modalidade ~ s(idade) + sexo,  # usar a função fp na variável quantitativa
                 data = dados.2, family = binomial(link = logit))
  summary(fit.l2c)
  # edf=2.2 -> transformação não linear
  fit.l3c <- gam(modalidade ~ s(idade) + sexo,  # usar a função fp na variável quantitativa
                 data = dados.3, family = binomial(link = logit))
  summary(fit.l3c)
  # edf=2.5 -> transformação não linear
  # ou graficamente
  par(mfrow=c(1,3))
  plot(fit.l1c)
  plot(fit.l2c)
  plot(fit.l3c)
  
  # conclusao: dado que temos transformações diferentes, talvez se justificasse categorizar a variavel idade 
  # vamos experimentar com a idade invertida
  fit2.o  <- nnet::multinom(modalidade ~ idade + sexo, data = dados)
  fit2.t1 <- nnet::multinom(modalidade ~ I(1/idade^(-.5)) + sexo, data = dados)
  fit2.t2 <- nnet::multinom(modalidade ~ I(1/idade) + sexo, data = dados)
  # comparar os dois modelos usando o valor p calculado a partir de 1-(pchisq(deviance(modelo.original)-deviance(modelo.com.variavel.transformada)), gl)
  #    com gl=1 no caso de ser uma transformação simples, ou gl=3 no caso de sugerir a transformação dupla
  1-pchisq(deviance(fit2.o)-deviance(fit2.t1), 1)  # p=0.013 -> optar pelo modelo com variavel tansformada
  1-pchisq(deviance(fit2.o)-deviance(fit2.t2), 1)  # p<0.0001 -> optar pelo modelo com variavel tansformada
  # comparar os AIC
  AIC(fit2.o, fit2.t1, fit2.t2)
  # optar por 1/idade
  
  # criar variavel transformada
  dados$idadeInv <- 1/dados$idade
  # ajusta modelo
  dados$modalidade <- relevel(dados$modalidade, ref="Caminhada")
  fitF <- mlogit(modalidade ~ 1 | idadeInv + sexo, 
                 reflevel = "Caminhada",   # indicar qual a categoria y de referencia
                 data = dados, shape="wide")
  fitFb <- nnet::multinom(modalidade ~ idadeInv + sexo, data = dados)
  # para podermos comparar resultados dos modelos, é preciso garantir que considera a mesma categoria de referência
  dados$modalidadeVGAM <- factor(dados$modalidade, levels=c("BTT/Ciclismo", "Fitness", "Outra", "Caminhada"))
  fitFc <- vglm(modalidadeVGAM ~  idadeInv + sexo, family=multinomial, data = dados)
  
  summary(fitF)
  
  
  ## f) --------------------------------------------------------------------------
  ## Adequabilidade e bondade
  
  ### adequabilidade
  summary(fitF)
  
  ### bondade do ajustamento
  library(generalhoslem)          # ativar pacote necessario
  generalhoslem::logitgof(dados$modalidade, fitted(fitF, outcome = FALSE))
  generalhoslem::logitgof(dados$modalidade, fitted(fitF, outcome = FALSE))$expected
  generalhoslem::logitgof(dados$modalidade, fitted(fitF, outcome = FALSE), g=6)
  generalhoslem::logitgof(dados$modalidade, fitted(fitF, outcome = FALSE), g=6)$expected
  
  
  ### averiguar a bondade do ajustamento dos modelos individuais
  # recordar que ja ajustamos os modelos logisticos "individuais" mas com idade não foi com idadeInv
  fit.l1t <- glm(modalidade ~ I(1/idade) + sexo, family = binomial(link = logit), data=dados.1)
  fit.l2t <- glm(modalidade ~ I(1/idade) + sexo, family = binomial(link = logit), data=dados.2)
  fit.l3t <- glm(modalidade ~ I(1/idade) + sexo, family = binomial(link = logit), data=dados.3)
  
  # R2 de Nagelkerke (e R2 de McFadden)
  pscl::pR2(fit.l1t) [c(4,6)]  # R2 de McFadden e R2 de Nagelkerke
  pscl::pR2(fit.l2t) [c(4,6)]  # R2 de McFadden e R2 de Nagelkerke
  pscl::pR2(fit.l3t) [c(4,6)]  # R2 de McFadden e R2 de Nagelkerke
  
  # Testes de ajustamento: H0: modelo ajusta-se vs H1: modelo nao se ajusta
  # a) Teste de Hosmer e Lemeshow 
  generalhoslem::logitgof(dados.1$modalidade, fitted(fit.l1t, outcome = FALSE))
  generalhoslem::logitgof(dados.2$modalidade, fitted(fit.l2t, outcome = FALSE)) 
  generalhoslem::logitgof(dados.3$modalidade, fitted(fit.l3t, outcome = FALSE)) 
  generalhoslem::logitgof(dados.3$modalidade, fitted(fit.l3t, outcome = FALSE))$expected 
  generalhoslem::logitgof(dados.3$modalidade, fitted(fit.l3t, outcome = FALSE), g=6) 
  
  
  # b) Teste de Cessie-van Houwelingen e coef Brier
  fit.l1.lrm <- lrm(modalidade ~ idadeInv + sexo, data=dados.1, x=TRUE, y=TRUE)
  resid(fit.l1.lrm, 'gof') # Valor p=0.1670925   -> OK
  print(fit.l1.lrm) # Brier= 0.163  # queremos que seja pequeno
  
  fit.l2.lrm <- lrm(modalidade ~ idadeInv + sexo, data=dados.2, x=TRUE, y=TRUE)
  print(fit.l2.lrm) # Brier= 0.180  # queremos que seja pequeno
  resid(fit.l2.lrm, 'gof') # Valor p=0.4664915 
  
  fit.l3.lrm <- lrm(modalidade ~ idadeInv + sexo, data=dados.3, x=TRUE, y=TRUE)
  print(fit.l3.lrm) # Brier= 0.165  # queremos que seja pequeno
  resid(fit.l3.lrm, 'gof') # Valor p=0.5320719
  
  
  ## g) --------------------------------------------------------------------------
  ## equacoes --------------------------------------------------------------------
  coefficients(fitF)
  summary(fitFb)
  
  ## h) --------------------------------------------------------------------------
  ## probabilidades previstas ----------------------------------------------------
  novosdados <- data.frame(idade=10:80, sexo=as.factor(rep("Masculino", 71)))
  novosdados$idadeInv <- 1/novosdados$idade
  probsPreditas <- predict(fitF, newdata=novosdados)
  probsPreditas <- predict(fitFb, newdata=novosdados, type="probs")
  
  
  
  ## i) --------------------------------------------------------------------------
  ## capacidade discriminativa ---------------------------------------------------
  preditos <- predict(fitF)
  head(preditos)  # probabilidade predita
  # problemas com predict quando se usa o mlogit
  
  probPred <- predict(fitFb, type="probs")
  head(probPred)  # probabilidade predita
  modalidadePred <- predict(fitFb, type="class")
  head(modalidadePred)  # modalidade predita
  confusionMatrix(dados$modalidade, modalidadePred)
  #o que há a referir?
  
  probPred <- predict(fitFc, type="response")
  head(probPred)  # probabilidade predita
  confusionMatrix(dados$modalidade, modalidadePred)
  #o que há a referir?
  
  table(dados$modalidade)
  
  # experimentar categorizar idade ...
  
  
  


