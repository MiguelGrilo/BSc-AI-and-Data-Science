#### TPC4 ####
#### Miguel Grilo   58387
#### Jorge Couto    58656

# Alínea A
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(pROC)
library(performance)
library(car)
library(MASS)

dados <- data.frame(
  perc_edu = c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95),
  n_empresas = c(56, 53, 62, 64, 60, 57, 62, 54, 56, 59),
  n_adotaram = c(0, 0, 7, 12, 25, 43, 45, 48, 55, 59)
) %>%
  mutate(n_nao_adotaram = n_empresas - n_adotaram,
         prop = n_adotaram / n_empresas)

mod_logit <- glm(cbind(n_adotaram, n_nao_adotaram) ~ perc_edu, family = binomial("logit"), data = dados)
mod_probit <- glm(cbind(n_adotaram, n_nao_adotaram) ~ perc_edu, family = binomial("probit"), data = dados)
mod_cloglog <- glm(cbind(n_adotaram, n_nao_adotaram) ~ perc_edu, family = binomial("cloglog"), data = dados)

summary(mod_logit)
summary(mod_probit)
summary(mod_cloglog)

AIC(mod_logit, mod_probit, mod_cloglog)
# Logit = 46.03189
# Probit = 43.35168
# Cloglog = 56.33386
BIC(mod_logit, mod_probit, mod_cloglog)
# Logit = 46.63706
# Probit = 43.95685
# Cloglog = 56.93903

# Tanto em AIC como BIC o modelo Probit apresenta valores menores em comparação
# aos outros modelos

1 - (mod_logit$deviance / mod_logit$null.deviance)
# R-squared = 0.9643174
1 - (mod_probit$deviance / mod_probit$null.deviance)
# R-squared = 0.9704494
1 - (mod_cloglog$deviance / mod_cloglog$null.deviance)
# R-squared = 0.9407477

# Concluímos agora que o modelo Probit também é o que melhor explica a 
# variabilidade, com R-squared = 0.9704494, sendo o maior valor entre os modelos
# Assim, o modelo Probit é o mais adequado


# Alínea B
ld_logit <- dose.p(mod_logit, p = 0.9)
ld_probit <- dose.p(mod_probit, p = 0.9)
ld_cloglog <- dose.p(mod_cloglog, p = 0.9)
data.frame(Logit = ld_logit[1], Probit = ld_probit[1], Cloglog = ld_cloglog[1])
#           Logit     Probit    Cloglog
# p = 0.9:  71.73991  72.39195  72.60419
# Tirando conclusões para o modelo Probit, sendo o mais adequado, concluímos que
# para garantir uma probabilidade de adoção de IA igual ou superior a 90% pelo 
# menos 72.4%€ dos trabalhadores devem ter ensino superior


# Alínea C

# Amostras para prefição, obter intervalo pequeno porque só queremos 50%
grid <- data.frame(perc_edu = seq(49, 51, 0.5))

# Função para obter fit + IC
get_preds <- function(model, grid, nome) {
  pred <- predict(model, newdata = grid, se.fit = TRUE)
  grid %>%
    mutate(
      fit = pred$fit,
      se = pred$se.fit,
      lwr = model$family$linkinv(fit - 1.96 * se),
      upr = model$family$linkinv(fit + 1.96 * se),
      prob = model$family$linkinv(fit),
      Modelo = nome
    )
}

preds <- bind_rows(
  get_preds(mod_logit, grid, "Logit"),
  get_preds(mod_probit, grid, "Probit"),
  get_preds(mod_cloglog, grid, "Cloglog")
)
preds
#     Modelo    Prob      IC_inf    IC_sup
# 1   Logit     0.5166128 0.4533567 0.5793409
# 2   Probit    0.5103737 0.4536784 0.5668599
# 3   Cloglog   0.4432957 0.3901680 0.5002609

# Dito isso, devemos confirmar qual modelo é melhor para se usar nos 50%.
# Recriar preds para que inclua todo o domínio da variável perc_edu
grid <- data.frame(perc_edu = seq(5, 95, 0.05))
preds <- bind_rows(
  get_preds(mod_logit, grid, "Logit"),
  get_preds(mod_probit, grid, "Probit"),
  get_preds(mod_cloglog, grid, "Cloglog")
)


# Plot com bandas de confiança
ggplot(preds, aes(x = perc_edu, y = prob, color = Modelo, fill = Modelo)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  geom_point(data = dados, aes(x = perc_edu, y = prop), inherit.aes = FALSE, color = "black", size = 2) +
  labs(
    title = "Curvas de Probabilidade com Bandas de Confiança (95%)",
    x = "Porcentagem de funcionários com ensino superior",
    y = "Probabilidade da empresa adotar IA"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
# Como vemos, na área da porcentagem de funcionários igual a 50%,
# O logit e o probit são as melhores abordagens por serem os mais próximos
# Das amostras observadas. Portanto, podemos continuar a usar o modelo logit

# Concluindo, sendo 50% dos trabalhadores com ensino superior, a probabilidade
# estimada de adoção de IA é 51,0% com intervalo de confiança 
# entre (45.4%, 56.7%)