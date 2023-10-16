install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")


url <- "ENEM22.csv"

data <- read.csv(url, header = TRUE, sep = ";", dec = ".")

data

str(data)

# Questão 1
# Biblioteca necessária para a análise
library(dplyr)

# a. Principais medidas descritivas ou de tendência central para a "nota de matemática"
media_matematica <- mean(data$NU_NOTA_MT)
mediana_matematica <- median(data$NU_NOTA_MT)
moda_matematica <- as.numeric(names(table(data$NU_NOTA_MT)[table(data$NU_NOTA_MT) == max(table(data$NU_NOTA_MT))]))

# b. Principais medidas de dispersão ou variabilidade para a "nota de matemática"
variancia_matematica <- var(data$NU_NOTA_MT)
desvio_padrao_matematica <- sd(data$NU_NOTA_MT)

library(ggplot2)

ggplot(data, aes(x = NU_NOTA_MT)) +
  geom_histogram(binwidth = 20, color = "black", fill = "lightblue") +
  labs(title = "Histograma da Nota de Matemática", x = "Nota de Matemática", y = "Frequência")


# Questão 2
# Calcular a média final
data$media_final <- rowMeans(data[, c("NU_NOTA_CH", "NU_NOTA_CN", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")])

# Carregar a biblioteca ggplot2 para elaborar o gráfico
library(ggplot2)

# Gráfico boxplot com a média final destacada
ggplot(data, aes(x = "", y = media_final)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  geom_point(aes(y = media_final), color = "red", size = 4) +
  labs(title = "Boxplot das Notas do ENEM",
       x = "Provas",
       y = "Notas") +
  scale_y_continuous(limits = c(0, 1000))  # Ajustando os limites do eixo Y


# Questão 3
# Carregar bibliotecas necessárias
library(dplyr)
library(ggplot2)


# Calculando o número de classes usando a regra de Sturges
num_classes <- ceiling(log2(nrow(data$NU_NOTA_ENEM)) + 1)

# Criando as classes com a função cut
dados_enem <- dados_enem %>%
  mutate(classe_nota = cut(nota_enem, breaks = num_classes))

# Tabela de frequências absolutas e relativas
tabela_freq <- dados_enem %>%
  group_by(classe_nota) %>%
  summarize(frequencia_abs = n(), frequencia_rel = n() / nrow(dados_enem), 
            frequencia_acum = cumsum(frequencia_rel))

# Imprimindo a tabela de frequências
print(tabela_freq)



# Questão 4
# Contar a frequência de cada estado civil
estado_civil_freq <- data %>% 
  count(ESTADO_CIVIL)

# Criar o gráfico de barras
ggplot(estado_civil_freq, aes(x = ESTADO_CIVIL, y = n, fill = ESTADO_CIVIL)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribuição dos Estados Civis dos Candidatos no ENEM",
       x = "Estado Civil",
       y = "Frequência")


# Criar o gráfico de barras
ggplot(data, aes(x = NU_NOTA_ENEM)) +
  geom_bar() +
  labs(title = "Distribuição das Notas Finais dos Candidatos no ENEM",
       x = "Nota Final",
       y = "Frequência")

# Questão 5
dados_enem <- data.frame(data$NU_NOTA_CH, data$NU_NOTA_CN, data$NU_NOTA_LC, data$NU_NOTA_MT, data$NU_NOTA_REDACAO, data$NU_NOTA_ENEM)
cor_matrix <- cor(dados_enem)

print(cor_matrix)

library(corrplot)

corrplot(cor_matrix, type = "upper", method = "color", tl.cex = 0.8)

# Encontrar as duas notas com maior correlação
max_corr <- which(cor_matrix == max(cor_matrix[cor_matrix != 1]), arr.ind = TRUE)
row_idx <- max_corr[1]
col_idx <- max_corr[2]

# Gerar o gráfico de dispersão
ggplot(dados_enem, aes(x = dados_enem[, col_idx], y = dados_enem[, row_idx])) +
  geom_point() +
  labs(title = paste("Gráfico de Dispersão entre",
                     names(dados_enem)[col_idx], "e", names(dados_enem)[row_idx]),
       x = names(dados_enem)[col_idx],
       y = names(dados_enem)[row_idx])
