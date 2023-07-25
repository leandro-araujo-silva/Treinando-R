url <- "C:/Users/Windows/Documents/GitHub/Treinando-R/Aprendendendo-r/IMC.csv"

data <- read.csv(url, header = TRUE, sep = ";", dec = ".")

data

str(data)

#Verifique se a coluna "mpg" contém apenas valores numéricos (ou vazios)
all_numeric <- suppressWarnings(!is.na(as.numeric(data$IMC)))

# Verifique se há valores não numéricos na coluna
any_non_numeric <- any(!all_numeric)

# Se houver valores não numéricos, identifique-os
if (any_non_numeric) {
  non_numeric_values <- unique(data$IMC[!all_numeric])
  print(paste("Valores não numéricos encontrados na coluna 'IMC':", paste(non_numeric_values, collapse = ", ")))
}


# Converter atributos para numérico e substituir vírgulas por pontos
data$IMC <- as.numeric(gsub(",", ".", data$IMC))
data$Peso <- as.numeric(gsub(",", ".", data$Peso))
data$Peso <- round(data$Peso, 2)

str(data)

data$IMC <- round(data$IMC, 2)

print(data)


# Carregar a biblioteca ggplot2 para elaborar os gráficos
library(ggplot2)

# Gráfico de barras para a variável "sexo"
ggplot(data, aes(x = Sexo, fill = Sexo)) +
  geom_bar() +
  labs(title = "Distribuição por Sexo", x = "Sexo", y = "Frequência")

# Histograma da idade
ggplot(data, aes(x = Idade)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  labs(title = "Histograma da Idade", x = "Idade", y = "Frequência")

# Histograma do peso
ggplot(data, aes(x = Peso)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(title = "Histograma do Peso", x = "Peso", y = "Frequência")

# Histograma da altura
ggplot(data, aes(x = Altura)) +
  geom_histogram(binwidth = 0.05, color = "black", fill = "lightblue") +
  labs(title = "Histograma da Altura", x = "Altura", y = "Frequência")

# Histograma do IMC
ggplot(data, aes(x = IMC)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  labs(title = "Histograma do IMC", x = "IMC", y = "Frequência")

# Boxplot da idade
ggplot(data, aes(y = Idade)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot da Idade", y = "Idade")

# Boxplot do peso
ggplot(data, aes(y = Peso)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot do Peso", y = "Peso")

# Boxplot da altura
ggplot(data, aes(y = Altura)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot da Altura", y = "Altura")

# Boxplot do IMC
ggplot(data, aes(y = IMC)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot do IMC", y = "IMC")
