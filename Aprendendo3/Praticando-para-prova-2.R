install.packages("readxl")

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

3)

jacare <- read_xlsx("jacare.xlsx")

jacare

result <- aov(jacare$Quantidade ~ factor(jacare$Local))
anova(result)

resp: Como Pr ( > F ) *  rejeitar H଴, ou seja, que existe diferenças
entre os grupos (tratamentos).

TukeyHSD(result)

Resp: Considerando o nível de significância de 1%, os locais L3-L1, L3-L2, são significativamente diferentes.


4) 

x <- c(2.0, 2.0, 2.0, 4.0, 4.0, 4.0, 6.0, 6.0, 6.0, 8.0, 8.0, 8.0, 10.0, 10.0, 10.0)
y <- c(2.1, 1.8, 1.9, 4.5, 4.2, 4.0, 6.2, 6.0, 6.5, 8.2, 7.8, 7.7, 9.6, 10.0, 10.1)
tabela <- data.frame(x, y)
tabela

plot(tabela$x, tabela$y)

reglinear <- lm(data = tabela, formula = tabela$y ~ tabela$x)
abline(reglinear)
summary(reglinear)

correlacao = cor(tabela$x,tabela$y)
correlacao
Determinacao = 0.9922

y = 0.98*x + 0.16

f(5) = 0.98 * 5 + 0.16
f(12) = 0.98 * 12 + 0.16

5)

dados <- read_xlsx("dados.xlsx")
dados

plot(dados$x, dados$y)

reglinear <- lm(data = dados, formula = dados$y ~ dados$x)
abline(reglinear)
summary(reglinear)

R2 = 0.926

#correlacao = sqrt(R2)
correlacao = cor(dados$x,dados$y)

correlacao

r2 = correlacao ^2
r2



y = 0.9553*x + 9.8757





