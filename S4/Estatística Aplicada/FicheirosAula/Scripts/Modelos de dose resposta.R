# Pacotes
library(ggplot2)
library(dplyr)
library(tidyr)
library(extras)
library(faraway)
library(MASS)
library(broom)
library(pROC)
library(performance)
library(car)

# Dados
dados <- data.frame(
  dose = c(1.69, 1.72, 1.76, 1.78, 1.81, 1.84, 1.86, 1.88),
  mortos = c(6, 13, 18, 28, 52, 53, 61, 60),
  vivos = c(53, 47, 44, 28, 11, 6, 1, 0)
) %>%
  mutate(total = mortos + vivos,
         prop = mortos / total)

# Modelos
mod_logit   <- glm(cbind(mortos, vivos) ~ dose, family = binomial("logit"), data = dados)
mod_probit  <- glm(cbind(mortos, vivos) ~ dose, family = binomial("probit"), data = dados)
mod_cloglog <- glm(cbind(mortos, vivos) ~ dose, family = binomial("cloglog"), data = dados)

# Resumo dos modelos
summary(mod_logit)
summary(mod_probit)
summary(mod_cloglog)

# Estimativa para dose = 1.86
dose0 <- 1.86
pred_1.86 <- data.frame(
  dose = dose0,
  Logit = ilogit(predict(mod_logit, newdata = data.frame(dose = dose0))),
  Probit = pnorm(predict(mod_probit, newdata = data.frame(dose = dose0))),
  Cloglog = 1 - exp(-exp(predict(mod_cloglog, newdata = data.frame(dose = dose0)))),
  Observado = dados$prop[dados$dose == dose0]
)

print(pred_1.86)

# LD estimadas
ld_logit   <- dose.p(mod_logit, p = c(0.01, 0.25, 0.5, 0.75, 0.99))
ld_probit  <- dose.p(mod_probit, p = c(0.01, 0.25, 0.5, 0.75, 0.99))
ld_cloglog <- dose.p(mod_cloglog, p = c(0.01, 0.25, 0.5, 0.75, 0.99))
data.frame(ld_logit,ld_probit,ld_cloglog )

# Comparação dos modelos
AIC(mod_logit, mod_probit, mod_cloglog)
BIC(mod_logit, mod_probit, mod_cloglog)


# Doses para prefição
grid <- data.frame(dose = seq(1.65, 1.9, 0.005))

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

# Plot com bandas de confiança
ggplot(preds, aes(x = dose, y = prob, color = Modelo, fill = Modelo)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  geom_point(data = dados, aes(x = dose, y = prop), inherit.aes = FALSE, color = "black", size = 2) +
  labs(
    title = "Curvas de Probabilidade com Bandas de Confiança (95%)",
    x = "Dose",
    y = "Probabilidade de Morte"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# LD50 e LD75 no gráfico com cloglog
ld50 <- dose.p(mod_cloglog, p = 0.5)
ld75 <- dose.p(mod_cloglog, p = 0.75)
ggplot() +
  geom_line(data = preds %>% filter(Modelo == "Cloglog"),
            aes(x = dose, y = prob), color = "blue") +
  geom_point(data = dados, aes(x = dose, y = prop), color = "red") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0.75, linetype = "dotted", color = "blue") +
  geom_vline(xintercept = ld50[1], linetype = "dashed", color = "red") +
  geom_vline(xintercept = ld75[1], linetype = "dotted", color = "blue") +
  annotate("text", x = ld50[1], y = 0.52, label = paste0("LD50 = ", round(ld50[1], 3)), color = "red", hjust = -0.1) +
  annotate("text", x = ld75[1], y = 0.77, label = paste0("LD75 = ", round(ld75[1], 3)), color = "blue", hjust = -0.1) +
  labs(title = "Modelo Cloglog com LD50 e LD75",
       x = "Dose", y = "Probabilidade") +
  theme_minimal()

## Pseudo-R² de McFadden
1 - (mod_logit$deviance / mod_logit$null.deviance)
1 - (mod_probit$deviance / mod_probit$null.deviance)
1 - (mod_cloglog$deviance / mod_cloglog$null.deviance)

### Curvas ROC ###
# Expandir os dados: um registo por indivíduo
dados_expandido <- dados %>%
  rowwise() %>%
  mutate(
    resposta = list(c(rep(1, mortos), rep(0, vivos))),
    dose_exp = list(rep(dose, total))
  ) %>%
  unnest(c(resposta, dose_exp)) %>%
  ungroup
# Reajustar os modelos com dados originais
mod_logit   <- glm(cbind(mortos, vivos) ~ dose, family = binomial("logit"), data = dados)
mod_probit  <- glm(cbind(mortos, vivos) ~ dose, family = binomial("probit"), data = dados)
mod_cloglog <- glm(cbind(mortos, vivos) ~ dose, family = binomial("cloglog"), data = dados)
# Obter previsões para cada indivíduo
dados_expandido <- dados_expandido %>%
  mutate(
    pred_logit = predict(mod_logit, newdata = data.frame(dose = dose_exp), type = "response"),
    pred_probit = predict(mod_probit, newdata = data.frame(dose = dose_exp), type = "response"),
    pred_cloglog = predict(mod_cloglog, newdata = data.frame(dose = dose_exp), type = "response")
  )
# Calcular curvas ROC e AUCs
roc_logit <- roc(dados_expandido$resposta, dados_expandido$pred_logit)
roc_probit <- roc(dados_expandido$resposta, dados_expandido$pred_probit)
roc_cloglog <- roc(dados_expandido$resposta, dados_expandido$pred_cloglog)
# Gráfico das curvas ROC
ggplot() +
  geom_line(aes(x = 1 - roc_logit$specificities, y = roc_logit$sensitivities), color = "blue", size = 1) +
  geom_line(aes(x = 1 - roc_probit$specificities, y = roc_probit$sensitivities), color = "green", size = 1) +
  geom_line(aes(x = 1 - roc_cloglog$specificities, y = roc_cloglog$sensitivities), color = "red", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(title = "Curvas ROC dos Modelos Logísticos",
       x = "1 - Especificidade (FPR)",
       y = "Sensibilidade (TPR)") +
  annotate("text", x = 0.6, y = 0.3, label = paste0("Logit AUC = ", round(roc_logit$auc, 3)), color = "blue") +
  annotate("text", x = 0.6, y = 0.25, label = paste0("Probit AUC = ", round(roc_probit$auc, 3)), color = "green") +
  annotate("text", x = 0.6, y = 0.20, label = paste0("Cloglog AUC = ", round(roc_cloglog$auc, 3)), color = "red") +
  theme_minimal()

## Diagnóstico geral com Desempenho
check_model(mod_cloglog)
# As curvas simuladas (azul) acompanham bem os dados observados (verde), sugerindo que o modelo reproduz razoavelmente a distribuição da variável resposta.
# Todos os pontos estão dentro das bandas em forma de diamante, e os resíduos médios estão próximos de zero. → Isto sugere ausência de enviesamento sistemático nas predições.
# Os ponto 3 e 5 aparecem próximo das linhas da distância de Cook distance (curvas verdes), o que indica possivelmente alguma influência.
# Também há um leve desvio na linha de tendência dos resíduos. O ponto 6 tem resíduos pequeno e leverage elevada.
# Os pontos desviam-se um pouco da linha nos extremos. Isso pode indicar ligeira má especificação da variância, mas nada grave dado o pequeno tamanho da amostra (apenas 8 pontos).
# --- Calcular e visualizar distância de Cook ---
dados$cook <- cooks.distance(mod_cloglog)
n_obs <- nrow(dados)
ggplot(dados, aes(x = factor(1:n_obs), y = cook)) +
  geom_segment(aes(xend = factor(1:n_obs), yend = 0), color = "skyblue", linewidth = 1) +
  geom_point(size = 2, color = "black") +
  geom_hline(yintercept = 4 / n_obs, linetype = "dashed", color = "red") +
  labs(title = "Distância de Cook por Observação",
       x = "Ponto (índice no dataset)",
       y = "Distância de Cook") +
  theme_minimal()

crPlot(mod_cloglog, variable = "dose") # Linearidade
# Linha azul tracejada: o ajustamento linear feito pelo modelo cloglog 
# Linha rosa (loess): tendência não paramétrica (baseada apenas nos dados)
# A linha rosa segue muito de perto a linha azul → a relação entre a dose e o preditor linear é aproximadamente linear.
# Os pontos distribuem-se de forma razoavelmente simétrica em torno da linha → sem padrões de não-linearidade.

# --- Modelo com todos os pontos
mod_cloglog_all <- glm(cbind(mortos, vivos) ~ dose, family = binomial("cloglog"), data = dados)
ld50_all <- dose.p(mod_cloglog_all, p = 0.5)[1]

# --- Remover ponto 5
dados_reduzido <- dados[-5, ]

mod_cloglog_sem5 <- glm(cbind(mortos, vivos) ~ dose, family = binomial("cloglog"), data = dados_reduzido)
ld50_sem5 <- dose.p(mod_cloglog_sem5, p = 0.5)[1]

# --- Comparação das curvas
grid <- data.frame(dose = seq(1.65, 1.9, 0.005))
grid <- grid %>%
  mutate(
    full = predict(mod_cloglog_all, newdata = ., type = "response"),
    sem5 = predict(mod_cloglog_sem5, newdata = ., type = "response")
  ) %>%
  pivot_longer(cols = c("full", "sem5"), names_to = "Modelo", values_to = "Prob")

# --- Gráfico comparativo
ggplot(grid, aes(x = dose, y = Prob, color = Modelo)) +
  geom_line(size = 1) +
  geom_point(data = dados, aes(x = dose, y = prop), color = "black") +
  geom_vline(xintercept = ld50_all, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = ld50_sem5, linetype = "dotted", color = "red") +
  annotate("text", x = ld50_all, y = 0.8, label = sprintf("LD50 com p5 = %.3f", ld50_all), color = "blue", hjust = -0.1) +
  annotate("text", x = ld50_sem5, y = 0.6, label = sprintf("LD50 sem p5 = %.3f", ld50_sem5), color = "red", hjust = -0.1) +
  labs(title = "Comparação do Modelo Cloglog com e sem Ponto 8",
       x = "Dose", y = "Probabilidade estimada") +
  theme_minimal()

