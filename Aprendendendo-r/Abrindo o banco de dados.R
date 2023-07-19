# Instalando pacotes

## Op??o 1: atrav?s de "packages" no canto inferior direito

## Op??o 2:
install.packages("dplyr")

install.packages("readxl")

## Op??o 3:
if(!require(dplyr))
  install.packages("dplyr")


#################### Carregando pacotes ####################

library(car)
require(dplyr)
library(readxl)




############### Carregando o banco de dados ###############

# Passo 1: selecionar o diret?rio de trabalho (working directory)


## Op??o 1 - Manualmente: Session > Set Working Directory > Choose Directory

## Op??o 2:
setwd("C:\Users\Windows\Downloads\Aprendendendo-r")


# Passo 2: carregar o banco de dados


date <- read_excel("dataset4.xlsx")
dados <- read.csv('Banco de Dados 2 Codificado.csv', sep = ';', dec = ',')


## Fun??es para visualizar o banco de dados:
view(date)
glimpse(date)


########## Ajustando as variáveis ############

# Transformando gênero em fator:

dados$Genero <- factor(dados$Genero, label = c("M", "F"), levels = c(0, 1))

# Transformando Grau de Instrução em fator:

dados$Grau_de_Instruçao <- factor(dados$Grau_de_Instruçao,
                                  labels = c("Fundamental", "Médio", "Superior"),
                                  levels = 0:2, order = T)

# Codificando valores ausentes
dados[dados == -999] <- NA

# Passo 3: Verificação da normalidade dos dados

shapiro.test(dados$Altura)

# Passo 4: Realização do teste t para uma amostra
t.test(dados$Altura, mu = 167)

