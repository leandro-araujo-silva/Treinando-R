url <- "ENEM_CPP.csv"

dados <- read.csv(url, header = TRUE, sep = ";", dec = ".")

dados

str(dados) 

# Deletar a coluna "CO_ESCOLA" do conjunto de dados (substituindo a tabela original)
dados <- dados[, -which(names(dados) == "CO_ESCOLA")]

# Exibir a tabela após deletar a coluna "carb"
print(dados)

# Criar a tabela de frequências cruzadas entre as variáveis sexo e língua estrangeira
tabela_frequencias <- table(dados$TP_SEXO, dados$TP_LINGUA)

# Exibir a tabela de frequências cruzadas
print(tabela_frequencias)

# Gerar o gráfico de barras sexo x língua estrangeira

barplot(tabela_frequencias, beside = TRUE, col = c("lightblue", "lightgreen"),
        legend = TRUE, args.legend = list(x = "topright", bty = "n", inset = c(0, -0.15)),
        xlab = "Sexo", ylab = "Frequência", main = "Frequência de Sexo por Língua Estrangeira")


# Criar a tabela de frequências cruzadas entre as variáveis estado civil e sexo
tabela_frequencias2 <- table(dados$TP_SEXO, dados$TP_ESTADO_CIVIL)

# Exibir a tabela de frequências cruzadas
print(tabela_frequencias2)

# Gerar o gráfico de barras estado civil x sexo
barplot(tabela_frequencias2, beside = TRUE, col = c("lightblue", "lightgreen"),
        legend = TRUE, args.legend = list(x = "topright", bty = "n", inset = c(0, -0.15)),
        xlab = "Estado Civil", ylab = "Frequência", 
        main = "Frequência de Estado Civil por Sexo")

# Exercício 2
# Carregar a biblioteca ggplot2 para elaborar os gráficos
library(ggplot2)

# Histograma da nota de natureza
ggplot(dados, aes(x = NU_NOTA_CN)) +
  geom_histogram(binwidth = 50, color = "black", fill = "lightblue") +
  labs(title = "Histograma da Nota de Natureza", x = "Nota de Natureza", y = "Frequência")

# Histograma da nota de humanas
ggplot(dados, aes(x = NU_NOTA_CH)) +
  geom_histogram(binwidth = 50, color = "black", fill = "lightblue") +
  labs(title = "Histograma da Nota de Humanas", x = "Nota de Humanas", y = "Frequência")

# Histograma da nota de linguagem
ggplot(dados, aes(x = NU_NOTA_LC)) +
  geom_histogram(binwidth = 50, color = "black", fill = "lightblue") +
  labs(title = "Histograma da Nota de Linguagem", x = "Nota de Linguagem", y = "Frequência")

# Histograma da nota de matemática
ggplot(dados, aes(x = NU_NOTA_MT)) +
  geom_histogram(binwidth = 50, color = "black", fill = "lightblue") +
  labs(title = "Histograma da Nota de Matemática", x = "Nota de Matemática", y = "Frequência")

# Histograma da nota de redação
ggplot(dados, aes(x = NU_NOTA_REDACAO)) +
  geom_histogram(binwidth = 50, color = "black", fill = "lightblue") +
  labs(title = "Histograma da Nota de Redação", x = "Nota de Redação", y = "Frequência")

# Boxplot da nota final no ENEM
ggplot(dados, aes(x = "", y = NU_NOTA_ENEM)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot da Nota Final no ENEM", x = "Nota Final no ENEM", y = "Nota")

# Boxplot de outras variáveis quantitativas (exemplo: nota de natureza)
ggplot(dados, aes(x = "", y = NU_NOTA_CN)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot da Nota de Natureza", x = "Nota de Natureza", y = "Nota")

# Exercício 3

# Tabela de frequências cruzadas entre as variáveis sexo e lingua_estrangeira
tabela_frequencias_qualitativas <- table(dados$TP_SEXO, dados$TP_LINGUA)

# Exibir a tabela de frequências cruzadas
print(tabela_frequencias_qualitativas)

# Gráfico de barras com a relação entre sexo e lingua_estrangeira
barplot(tabela_frequencias_qualitativas, beside = TRUE, col = c("lightblue", "lightgreen"),
        legend = TRUE, args.legend = list(x = "topright", bty = "n", inset = c(0, -0.15)),
        xlab = "Sexo", ylab = "Frequência",
        main = "Relação entre Sexo e Língua Estrangeira")

# Boxplot da nota_enem agrupado por estado_civil
ggplot(dados, aes(x = TP_ESTADO_CIVIL, y = NU_NOTA_ENEM, fill = TP_ESTADO_CIVIL)) +
  geom_boxplot() +
  labs(title = "Boxplot da Nota Final no ENEM por Estado Civil", x = "Estado Civil", y = "Nota Final no ENEM")

# Gráfico de dispersão entre nota_natureza e nota_humanas
ggplot(dados, aes(x = NU_NOTA_CN, y = NU_NOTA_CH)) +
  geom_point(color = "blue") +
  labs(title = "Gráfico de Dispersão entre Nota de Natureza e Nota de Humanas", x = "Nota de Natureza", y = "Nota de Humanas")
