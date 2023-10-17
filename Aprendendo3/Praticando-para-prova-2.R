library(readxl)

1)

fornecedores <- read_xlsx("fornecedores.xlsx")

fornecedores

Hipóteses:
H0: u1 = u2 = u3 = u4 = u
H1: pelo menos um é diferente dos demais

result <- aov(fornecedores$Quantidade ~ factor(fornecedores$Fornecedores))
anova(result)

f-value < nivel de significância (5%)

resp: Como Pr ( > F ) *  rejeitar H଴, ou seja, que existe diferenças
entre os grupos (tratamentos).


TukeyHSD(result)

Resp: O fornecedor 2 com o fornecedor 1 são significativamente diferentes.


2)

fabricas <- read_xlsx("fabricas.xlsx")

fabricas

result <- aov(fabricas$Valores ~ factor(fabricas$Fabricas))
anova(result)

resp: Como Pr ( > F ) *  rejeitar H଴, ou seja, que existe diferenças
entre os grupos (tratamentos).

TukeyHSD(result)

Resp: A fábrica de Seatle e a do Dallas são significativamente diferentes.

4) 

url <- ("C:/Users/Windows/Documents/GitHub/Treinando-R/Aprendendo3/amostra.csv")

Reg = read.table(file = "C:/Users/Windows/Documents/GitHub/Treinando-R/Aprendendo3/amostra.csv", 
                 header = TRUE, sep = ";", dec = ",") 








