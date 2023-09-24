# Questão 1

dbinom(3, size=4, prob=1/5)


tpm3 = 1 - (dbinom(7,7,prob = 0.6)+dbinom(6,7,prob = 0.6)+dbinom(5,7,prob = 0.6))
tpm3

dbinom(1, size = 4, prob = 0.3)

dbinom(4, size = 10, prob= 0.25)

dbinom(2, size = 6, prob= 0.8)

dbinom(4, size = 6, prob= 1/3)

dbinom(0, size = 6, prob= 1/3)

## No máximo duas 
pbinom(2, size = 6, prob = 1/3)

## No máximo duas
pbinom(2, size = 6, prob = 0.2)

## Pelo menos três
pbinom (6, size = 6, prob = 1/3) - pbinom (2, size = 6, prob = 1/3)

1 - pbinom (2, size = 6, prob = 1/3)

## quatro coroas
dbinom(4, size = 6, prob = 1/2)

## Pelo menos duas
1 - pbinom(1, size =6, prob= 1/2)

## Menor ou igual a três
pbinom(3, size = 6, prob = 1/2)

## Igual a 4
dbinom(4, size = 6, prob = 1/2)


## Poisson
dpois(2, lambda = 5)

## No máximo dois
ppois(2, lambda = 6)

## Qual a probabilidade de mais de 19 alunos serem aprovados?
ppois(19, lambda = 23, lower.tail = F)
1 - ppois(19, lambda = 23)

## Qual a quantidade de aprovados se a probabilidade acumulada for de 80%?
qpois(0.80, lambda=23)


dpois(5, lambda= 8)

taxa = 2/50000 * 112500
ppois (2, taxa, lower.tail = F)