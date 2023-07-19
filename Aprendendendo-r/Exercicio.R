install.packages("ggplot2")
library(ggplot2)

url <- "mtcars.csv"

dados <- read.csv(url, header = TRUE, sep = ";", dec = ".")

dados

str(dados) 

# Verifique se a coluna "mpg" contém apenas valores numéricos (ou vazios)
all_numeric <- suppressWarnings(!is.na(as.numeric(dados$mpg)))

# Verifique se há valores não numéricos na coluna
any_non_numeric <- any(!all_numeric)

# Se houver valores não numéricos, identifique-os
if (any_non_numeric) {
  non_numeric_values <- unique(dados$mpg[!all_numeric])
  print(paste("Valores não numéricos encontrados na coluna 'mpg':", paste(non_numeric_values, collapse = ", ")))
}


# Converter atributos para numérico e substituir vírgulas por pontos
dados$mpg <- as.numeric(gsub(",", ".", dados$mpg))
dados$disp <- as.numeric(gsub(",", ".", dados$disp))
dados$drat <- as.numeric(gsub(",", ".", dados$drat))
dados$wt <- as.numeric(gsub(",", ".", dados$wt))
dados$qsec <- as.numeric(gsub(",", ".", dados$qsec))

str(dados)

print(dados)

# Converta a coluna "mpg" para numérico
dados$mpg <- as.numeric(dados$mpg)
str(dados) 


# Verificar a quantidade de valores ausentes na coluna "mpg"
sum(is.na(dados$mpg))

# Mínimo e máximo
min_mpg <- min(dados$mpg)
max_mpg <- max(dados$mpg)

# Média
media_mpg <- mean(dados$mpg)

# Mediana
mediana_mpg <- median(dados$mpg)

# Desvio padrão
desvio_padrao_mpg <- sd(dados$mpg)

# Quartis
quartis_mpg <- quantile(dados$mpg, probs = c(0.25, 0.5, 0.75, 1))

# Histograma
hist(dados$mpg, breaks = "FD", col = "lightblue", xlab = "MPG", ylab = "Frequência", main = "Histograma de MPG")

# Boxplot
boxplot(dados$mpg, col = "lightblue", main = "Boxplot de MPG", ylab = "MPG")

# Questão 2
str(dados)

# Criar o gráfico boxplot
ggplot(dados, aes(x = factor(1), y = disp)) +
  geom_boxplot(fill = "lightblue") +
  geom_boxplot(aes(x = factor(2), y = hp), fill = "lightgreen") +
  scale_x_discrete(labels = c("disp", "hp")) +
  labs(title = "Gráfico Boxplot", x = "Colunas", y = "Valores")


# Questão 3
# Agrupar os dados em seus respectivos quartis
quartis_qsec <- quantile(dados$qsec, probs = c(0, 0.25, 0.5, 0.75, 1))

###################################################################################

# cORRIGIR ESSA PARTE A BAIXO


# Agrupar os dados em seus respectivos quartis
quartis_intervalos <- cut(dados$qsec, breaks = quantile(dados$qsec, probs = c(0, 0.25, 0.5, 0.75, 1)))

# Definir os rótulos dos quartis
rotulos_quartis <- c("Mínimo", "1º Quartil", "Mediana", "3º Quartil", "Máximo")

# Atribuir os rótulos aos intervalos
dados$quartis <- factor(quartis_intervalos, levels = unique(quartis_intervalos), labels = rotulos_quartis)

# Criar uma tabela com as frequências absolutas e relativas e suas respectivas acumuladas
tabela_quartis <- data.frame(
  Quartis = levels(dados$quartis),
  Frequencia_Absoluta = table(dados$quartis),
  Frequencia_Relativa = table(dados$quartis) / nrow(dados),
  Frequencia_Acumulada = cumsum(table(dados$quartis)),
  Frequencia_Relativa_Acumulada = cumsum(table(dados$quartis)) / nrow(dados)
)

# Exibir a tabela com as frequências
print(tabela_quartis)




