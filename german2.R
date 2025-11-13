# Trabalho de P&E - German Credit Data

rm(list = ls())
dev.off()

# Carregando pacotes utilizados

library(dplyr)    # Manipulação de dados
library(car)      # Teste de Levene (ANOVA)
library(ggplot2)  # Gráficos
library(MASS)     # Transformações Box-Cox

# Importando Dataframe
setwd("C:/Users/Joao Duarte/Desktop/AB2_R/AB2")

df <- read.table("german.data", header = FALSE)

colnames(df) <- c(
  "Status_conta", "Meses_existencia", "Historico_credito", "Proposito",
  "Valor_credito", "Saldo_poupanca", "Tempo_emprego", "Taxa_parcela_renda",
  "Status_pessoal_sexo", "Fiador", "Residencia_atual", "Bens",
  "Idade", "Planos_parcelamento", "Moradia", "Credito_existente", 
  "Emprego", "Dependentes", "Telefone", "Trabalhador_estrangeiro", "Classe"
)

# Configurações para gráficos futuros
df$Classe[df$Classe == "1.1"] <- "1"
df$Saldo_poupanca <- as.character(df$Saldo_poupanca)

par(mar = c(4, 4, 2, 1))
theme_set(theme_minimal())

# 1. Intervalo de Confiança
mean(df$Valor_credito)
t.test(df$Valor_credito, conf.level = 0.98)

# 2. Intervalo de confiança por proporção
sucessos <- sum(df$Classe == 1)
total <- length(df$Classe)

prop.test(sucessos, total, conf.level = 0.98)

# 3. Teste de Hipóstese para duas Médias (amostras independentes)
homens_divorciados <- df$Valor_credito[df$Status_pessoal_sexo == "A91"]
mulheres_divorciados <- df$Valor_credito[df$Status_pessoal_sexo == "A92"]

var.test(homens_divorciados, mulheres_divorciados)

t.test(homens_divorciados, mulheres_divorciados, var.equal = TRUE, conf.level = 0.98)

# 4. Teste de Hipótese para Proporções (duas amostras)
tab <- table(df$Status_pessoal_sexo, df$Classe)

prop.test(tab, conf.level = 0.95)

# 5. ANOVA de Um Fator
by(df$Meses_existencia, df$Status_conta, shapiro.test)

leveneTest(Meses_existencia ~ Status_conta, data = df)

modelo_anova <- aov(Meses_existencia ~ Status_conta, data = df)
summary(modelo_anova)

# 6. Correlação
cor_test <- cor.test(df$Idade,  df$Valor_credito, method = "pearson")
cor_test

# 7. Regressão Linear Simples
modelo <- lm(Idade ~ Valor_credito, data = df)
summary(modelo)

shapiro.test(residuals(modelo))


# 8. Transformação de Dados
shapiro.test(df$Valor_credito)

df$log_valor_credito <- log(df$Valor_credito)
shapiro.test(df$log_valor_credito)

media_log <- mean(df$log_valor_credito)
media_original <- exp(media_log)
