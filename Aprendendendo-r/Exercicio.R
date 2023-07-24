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


###############################################################################
# Questão 4
# Gerar o histograma para a coluna "hp"
hist(dados$hp, main = "Histograma - Horsepower", xlab = "Horsepower (hp)", ylab = "Frequência", col = "lightblue", border = "black")

# Definir o número de classes usando a regra de Sturges
num_classes <- ceiling(log2(nrow(dados)) + 1)

###################################################################################################################

# Criar uma tabela de frequência usando as classes sugeridas pelo histograma
tabela_frequencia <- data.frame(
  Classes = cut(dados$hp, breaks = num_classes, include.lowest = TRUE),
  Frequencia_Absoluta = table(cut(dados$hp, breaks = num_classes, include.lowest = TRUE)),
  Frequencia_Relativa = table(cut(dados$hp, breaks = num_classes, include.lowest = TRUE)) / nrow(dados),
  Frequencia_Acumulada = cumsum(table(cut(dados$hp, breaks = num_classes, include.lowest = TRUE))),
  Frequencia_Relativa_Acumulada = cumsum(table(cut(dados$hp, breaks = num_classes, include.lowest = TRUE))) / nrow(dados)
)

# Exibir a tabela de frequência
print(tabela_frequencia)

########################################################################################################

# Gerar o histograma para a coluna "hp" e obter os breaks (limites das classes)
histograma <- hist(dados$hp, plot = FALSE)

# Criar as classes manualmente usando os breaks do histograma
classes <- cut(dados$hp, breaks = histograma$breaks, include.lowest = TRUE, right = TRUE)

# Criar a tabela de frequência
tabela_frequencia <- data.frame(
  Classes = levels(classes),
  Frequencia_Absoluta = table(classes),
  Frequencia_Relativa = table(classes) / length(classes),
  Frequencia_Acumulada = cumsum(table(classes)),
  Frequencia_Relativa_Acumulada = cumsum(table(classes)) / length(classes)
)

# Exibir a tabela de frequência
print(tabela_frequencia)

# Questão 5

# Criar a matriz de frequências das colunas "motor" e "marcha"
matriz_frequencias <- table(dados$motor, dados$marcha)

# Exibir a matriz de frequências
print(matriz_frequencias)

# Criar gráfico de barras motor x marcha
barplot(matriz_frequencias, beside = TRUE, col = c("lightblue", "lightgreen"),
        legend = rownames(matriz_frequencias), xlab = "Motor", ylab = "Frequência",
        main = "Frequência de Motor por Marcha")

# Criar gráfico de barras marcha x motor
barplot(t(matriz_frequencias), beside = TRUE, col = c("lightblue", "lightgreen"),
        legend = colnames(matriz_frequencias), xlab = "Marcha", ylab = "Frequência",
        main = "Frequência de Marcha por Motor")
  
  
# Questão 6
# Contar a quantidade de marcas que utilizam o motor automático e manual
contagem_motor <- table(dados$marcha)

print(contagem_motor)

# Criar gráfico de barras com as frequências de marcas para cada tipo de motor
barplot(contagem_motor, col = c("lightblue", "lightgreen"), 
        names.arg = c("Automático", "Manual"),
        xlab = "Tipo de Marcha", ylab = "Frequência de Marcas",
        main = "Quantidade de Marcas por Tipo de Marcha")


# Questão 7
# Calcular o coeficiente de correlação entre as colunas "mpg" e "hp"
correlacao <- cor(dados$mpg, dados$hp)

# Exibir o coeficiente de correlação
print(paste("Coeficiente de correlação: ", correlacao))

# Gerar o gráfico de dispersão
plot(dados$mpg, dados$hp, main = "Gráfico de Dispersão", 
     xlab = "Milhas por Galão (mpg)", ylab = "Horsepower (hp)")

# Agrupar os dados usando os quartis da coluna "mpg"
quartis_mpg <- quantile(dados$mpg, probs = c(0, 0.25, 0.5, 0.75, 1))
grupo_mpg <- cut(dados$mpg, breaks = quartis_mpg, labels = c("Q1", "Q2", "Q3", "Q4"))

# Calcular a média de "hp" para cada grupo de quartis de "mpg"
media_hp_por_grupo <- tapply(dados$hp, grupo_mpg, mean)

# Exibir a tabela de médias por grupo
print(data.frame(Quartis = levels(grupo_mpg), Média_HP = media_hp_por_grupo))

# Questão 8
# Criar uma nova tabela contendo apenas as colunas numéricas
nova_tabela <- dados[, sapply(dados, is.numeric)]

# Exibir a nova tabela com apenas as variáveis numéricas
print(nova_tabela)

# Calcular a matriz de correlação entre todas as variáveis numéricas
matriz_correlacao <- cor(nova_tabela)

# Exibir a matriz de correlação
print(matriz_correlacao)