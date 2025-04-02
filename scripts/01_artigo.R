# BIBLIOTECAS NECESSARIAS --------------------------------------------------

library(readxl)   # Para ler arquivos Excel
library(tidyr)    # Para manipulação de dados (pivotagem)
library(ggplot2)  # Para visualização de dados
library(GGally)   # Para criar gráficos de pares (correlação)
library(pscl)     # Para calcular pseudo R² em modelos logísticos
library(caret)    # Para análise e avaliação de modelos preditivos
library(pROC)     # Para curvas ROC e métricas relacionadas

# CARREGAMENTO DOS DADOS ---------------------------------------------------

dados <- read_excel("data/setor_mapbiomas_30m.xlsx")
summary(dados)

# PREPARAÇÃO DOS DADOS ---------------------------------------------------

# Filtrar dados
dados_filtrados <- dados[, c("SITUACAO_2", "PER2_NAOVEG", "PER2_AGRO", "PER2_VEG", "PER2_FLO")]

# Transformar colunas em números
dados_filtrados <- apply(dados_filtrados, 2, as.numeric)
dados_filtrados <- data.frame(dados_filtrados)

# Conversão para fator
dados_filtrados$SITUACAO_2 <- as.factor(dados_filtrados$SITUACAO_2)

# ANÁLISE EXPLORATÓRIA -----------------------------------------------------

# Gráfico de pares
colnames(dados_filtrados)[2:5] <- c("Área Não Vegetada", "Agropecuária", "Vegetação", "Floresta")

fig_01 <- ggpairs(
  dados_filtrados, 
  aes(color = as.factor(SITUACAO_2), alpha = 0.6),  
  columns = 2:5,  
  upper = list(continuous = wrap("cor", size = 4)),  
  lower = list(continuous = wrap("points", alpha = 0.5, size = 1.5))
) +
  scale_color_manual(values = c("0" = "#ffa500", "1" = "#cccccc")) +  # Alteração das cores
  scale_fill_manual(values = c("0" = "#ffa500", "1" = "#cccccc")) +  # Para os histogramas
  theme_bw()

print(fig_01)

# Salvar Figura 1
ggsave("figs/01_analise_exploratoria.png", plot = fig_01, width = 8, height = 6, dpi = 600)

# PREPARAÇÃO PARA MODELAGEM ------------------------------------------------

# Filtrar dados
dados2 <- dados[, c("SITUACAO_2", "PER2_NAOVEG", "PER2_AGRO", "PER2_VEG")]

# Conversão para numérico
dados2 <- apply(dados2, 2, as.numeric)
dados2 <- data.frame(dados2)

# DIVISÃO DOS DADOS EM TREINO E TESTE --------------------------------------

# Divisão em treino (80%) e teste (20%)
set.seed(1)
tr <- round(0.8 * nrow(dados2))
treino <- sample(nrow(dados2), tr, replace = FALSE)
dados2.treino <- dados2[treino, ]
dados2.teste <- dados2[-treino, ]

# MODELAGEM - REGRESSÃO LOGÍSTICA ------------------------------------------

# Modelagem
mod_treino <- glm(SITUACAO_2 ~ PER2_NAOVEG + PER2_AGRO + PER2_VEG, family = binomial, data = dados2.treino)

# Resumo do modelo
summary(mod_treino)

# Pseudo R2 (métrica de qualidade)
pR2(mod_treino)

# Equação
eq <- paste0("logit(p) = ", round(coef(mod_treino)[1], 4), " + ", 
             paste0(round(coef(mod_treino)[-1], 4), " * X", 1:length(coef(mod_treino)[-1]), collapse = " + "))
print(eq)

# AVALIAÇÃO DO MODELO NO CONJUNTO DE TESTE ---------------------------------

# Previsão com conjunto de teste
prob_teste <- predict(mod_treino, type = "response", newdata = data.frame(
  PER2_NAOVEG = dados2.teste$PER2_NAOVEG,
  PER2_AGRO = dados2.teste$PER2_AGRO,
  PER2_VEG = dados2.teste$PER2_VEG))
pred_teste <- ifelse(prob_teste > 0.5, 1, 0)

# Matriz de confusão (avaliação de desempenho)
confusionMatrix(as.factor(pred_teste), as.factor(dados2.teste$SITUACAO_2))

# Curva ROC e cálculo AUC
roc_teste <- roc(dados2.teste$SITUACAO_2, prob_teste)

fig_02 <- ggroc(roc_teste) +
  ggtitle("Curva ROC") +
  theme_minimal() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")

plot(fig_02)

cat("AUC: ", auc(roc_teste), "\n")

# Salvar Figura 02
ggsave("figs/02_curva_hoc_auc.png", plot = fig_02, width = 8, height = 6, dpi = 600)

# VISUALIZAÇÃO DAS RELAÇÕES ------------------------------------------------

# Gráfico
dados2.teste$SITUACAO_2 <- as.numeric(as.character(dados2.teste$SITUACAO_2))

colnames(dados2.teste)[2:4] <- c("Área Não Vegetada", "Agropecuária", "Vegetação")

dados2.teste_long <- dados2.teste %>%
  pivot_longer(cols = c('Área Não Vegetada', 'Agropecuária', 'Vegetação'),
               names_to = "Variavel",
               values_to = "Valor")

fig_03 <- ggplot(dados2.teste_long, aes(x = Valor, y = SITUACAO_2, color = Variavel)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), size = 1.25) +
  labs(x = "Variáveis", y = "Situação - Probabilidade de ser urbana", color = "Tipo de Variável") +  # Nome da legenda
  theme_bw() +
  scale_color_manual(
    values = c("Área Não Vegetada" = "#d4271e", "Agropecuária" = "#ffefc3", "Vegetação" = "#d6bc74"),
    breaks = c("Área Não Vegetada", "Agropecuária", "Vegetação")  # Ordem que você quer na legenda
  )
plot(fig_03)

# Salvar Figura 03
ggsave("figs/03_curva_regressao.png", plot = fig_03, width = 8, height = 6, dpi = 600)