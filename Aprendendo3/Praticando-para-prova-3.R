install.packages("readxl")

library(readxl)


3)

Uma nova metodologia de desenvolvimento de software se propõe a reduzir o
tempo de projeto e desenvolvimento de sistemas de informação. Assim, foram
considerados 24 projetos, sendo 12 de tecnologia atual e 12 com a nova proposta.
Os valores, em horas, estão a seguir:
  
TecAtual: 300, 280, 344,385, 372, 360, 288, 321, 376, 290, 301, 325

TecNova: 274, 220, 308, 336, 198, 300, 315, 258, 318, 310, 273, 300

Considere nível de significância de 5 %, as hipóteses:
  

H0 : μଵ − μଶ ≤ 0
H1 : μଵ − μଶ > 0

Calcule t, o p-value e interpretar os resultados.

qt(0.025, 11); qt(0.975, 11)

A <- c(300, 280, 344,385, 372, 360, 288, 321, 376, 290, 301, 325)
N <- c(274, 220, 308, 336, 198, 300, 315, 258, 318, 310, 273, 300)

t.test(A,N, mu = 0, paired = FALSE, conf.level = 0.95)

resp: Rejeitamos H0 e pode-se concluir que a nova tecnologia reduziu o
tempo de projeto e desenvolvimento de sistemas.


4)

alunos <- read_xlsx("aluno.xlsx")

alunos

result <- aov(alunos$Nota ~ factor(alunos$Pessoa))
anova(result)

resp: Como Pr ( > F ) * rejeitar H଴, ou seja, existe diferenças
entre as notas dos alunos.

TukeyHSD(result)


Resp: Tanto a 5%, como 1%, as notas do Aluno4-Aluno1 são as únicas significativamente diferentes (0.0064442).


1) 
a. Nível de Confiança = 98%; n = 25; média populacional = XX, desvio padrão
√XX + 10.

alfa = 0.2
desvio = sqrt(35)
media = 25
n =25

zc = qnorm(1 - alfa/2, 0, 1)
zc = round(zc,2)
zc

erro = zc * desvio / sqrt(n)
erro = round(erro,2)
erro

cat ("[", media - erro, ",", media + erro, "]")

Intervalo de confiança: [ 23.49 , 26.51 ]


b. Nível de Confiança = 92%; n = 12; amostra: 60, 70, 72.5, 90, 77.5, 65.25, 50,
72.5, 87.25, 68, 70, (XX * 3).


valores <- c(60, 70, 72.5, 90, 77.5, 65.25, 50,
             72.5, 87.25, 68, 70, 75)
valores
alfa = 0.08
n = length(valores)
desvio = sd(valores)
media = mean(valores)
media

tc = qt(p = 1 - alfa/2, df = n-1)
tc = round(tc,3)
tc

erro = tc * desvio/sqrt(n)
erro = round(erro,3)
erro

cat("[", media - erro, ",", media + erro, "]")

Intervalo de confiança: [ 65.471 , 77.529 ]



2) 
Um fabricante afirma que o índice de lactose de um de seus biscoitos é de 26 mg.
Um laboratório tirou uma amostra e obteve os seguintes valores, em mg: 26, 24,
23, 22, 28, 25, 27, 26, 28, 24, 27, 25, 20, 24, 24, 27, 25 e XX.
O fabricante está correto para o nível de significância de 5%?
  
a) Faça os cálculos usando tabelas ou o software R como calculadora, desenhe
um gráfico com as regiões (RNR e RC) e os valores obtidos e justifique.

b) Considerado agora 1% de nível de significância, refaça os cálculos usando a
função específica do software R e justifique o resultando (se o fabricante
                                                           está correto) calculando o p-value.

H0 = 26
H1 /= 26
nivel de significancia = 5%

qt(0.025, 17); qt(0.975, 17)

vp = c(26, 24, 23, 22, 28, 25, 27, 26, 28, 24, 27, 25, 20, 24, 24, 27, 25, 25)
media = mean(vp)
media

desvio = sd(vp)
desvio

tobs = (media - 26) / (desvio / sqrt(18)) 
tobs

Resp: Aceita H0 e pode-se afirmar que a afirmação do fabricante está correto.


nivel de significancia = 1%

qt(0.005, 17); qt(0.9995, 17)

vp = c(26, 24, 23, 22, 28, 25, 27, 26, 28, 24, 27, 25, 20, 24, 24, 27, 25, 25)
media = mean(vp)
media

desvio = sd(vp)
desvio

tobs = (media - 26) / (desvio / sqrt(18)) 
tobs

Resp: Aceita H0 e pode-se afirmar que a afirmação do fabricante está correto.


