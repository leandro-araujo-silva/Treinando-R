# Questão 1
1 - Uma fábrica Brasileira de cosméticos, trabalhava com variância de 14 dias. Em uma
amostra de 120 cosméticos produzidos, foi possível obter um tempo médio de produção
de 9 dias. Obtenha o intervalo para um nível de confiança de 90%.

alfa = 0.10
desvio = sqrt(14)
desvio
media = 9
n = 120

zc = qnorm(1 - alfa/2, 0, 1)
zc = round(zc,2)
zc

erro = zc * desvio / sqrt(n)
erro = round(erro,2)
erro

cat ("[", media - erro, ",", media + erro, "]")


# Variância desconhecida
2 - No Hospital Unimed, 10 pacientes do sexo feminino que estavam na sala de espera
foram sorteadas para realizar a medição da pressão sanguínea arterial, obtendo os
seguintes resultados: (80, 75, 71, 82, 77, 64, 78, 67, 81, 79). Determine o intervalo de
confiança para a pressão arterial média feminina com coeficiente de confiança de 98%.

valores <- c(80, 75, 71, 82, 77, 64, 78, 67, 81, 79)
valores
alfa = 0.02
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


t.test(valores, conf = 0.98)


#Intervalo de Confiança para Proporção Amostral
3 - Um partido deseja estimar a proporção de eleitores favoráveis a um determinado
candidato a prefeito. Uma amostra piloto de 3500 eleitores revelou que 65% dos
eleitores são favoráveis a este candidato. Elaborar um intervalo de confiança de 95%.

sucesso =  0.65 * 3500
sucesso
prop.test(x = 2275, n = 3500, conf.level = 95/100)

prop.test(x = 0.65*3500, n = 3500, conf.level = 95/100)


# Média desconhecida
4 - De uma população normal com variância 36, tira-se uma amostra aleatória de
tamanho 20, obtendo-se uma média de 43. Ao nível de significância de 10%, testar as
hipóteses: H଴ : u = 45, Hଵ : u ≠ 45.

qt(0.05, 19) ; qt(0.95, 19)

media = 43 
desvio = sqrt(36)
n = 20
tc = (media - 45) / (desvio/sqrt(n))
tc

Resp: Aceita a hipótese nula e conclui-se que a média populacional é igual a 45.


5 - Um fabricante de contêineres realizou modificações em sua fabricação para
aumentar a resistência média, que é de 510 Kg. Ao retirar uma amostra de 15
contêineres, obteve-se uma média de 550 Kg. Sabendo que o desvio padrão de 25 Kg,
com um nível de significância de 5%, pode o fabricante afirmar que a resistência média
dos contêineres aumentou?
H0 <= 510
H1 > 510

qt(0.95, 14)

media = 550
desvio = 25
n = 15
tc = (media - 510) / (desvio/sqrt(n))
tc

Resp: Rejeita H0 e pode-se afirmar que a resistência média aumentou.


6 - O instituto de engenharia de uma universidade aplica um teste vocacional para os
calouros. Nos últimos anos tem sido admitida uma nota média de 127. Um teste foi
realizado no semestre atual, onde foram obtidas as seguintes notas: (125, 124, 125,
                                                                     125, 125, 125, 124, 123, 122, 123, 123, 123, 123, 124, 124). Queremos saber se a média
do semestre atual foi diferente dos anteriores, então, realize o teste de hipótese,
admitindo um nível de significância de 5% para efetuar o teste.

H0 = 127
H1 /= 127

qt(0.025, 14); qt(0.975, 14)

vp = c(125, 124, 125, 125, 125, 125, 124, 123, 122, 123, 123, 123, 123, 124, 124)
media = mean(vp)
media

desvio = sd(vp)
desvio

tobs = (media - 127) / (desvio / sqrt(15)) 
tobs

Resp: Rejeita H0 e pode-se afirmar que a média foi diferente da admitida nos últimos anos.


7 - Na disciplina de teoria dos grafos, o professor passou como atividade avaliativa, a
implementação de um algoritmo específico de grafos, que deveria ser implementado
utilizando dois tipos de busca: busca em largura e busca em profundidade. O professor
coletou os dados de tempo de execução (em milissegundos) de 10 alunos, para os dois
métodos de busca. Existe diferença na velocidade de execução do algoritmo para os
dois tipos de busca? Verificar a um nível de 5% de significância.

H0: bfs - dfs = 0
H1 /= 0

bfs = (32, 27, 38, 35, 33, 29, 25, 19, 31, 24)
dfs = (26, 21, 20, 37, 30, 18, 19, 25, 32, 34)

qt(0.025, 9); qt(0.975, 9)

A <- c(32, 27, 38, 35, 33, 29, 25, 19, 31, 24)
B <- c(26, 21, 20, 37, 30, 18, 19, 25, 32, 34)

t.test(A,B, mu = 0, paired=TRUE, conf.level = 0.95)

Resp: Aceita HO, logo não existe diferença na velocidade de execução do algoritmo.


8 - Um pesquisador estudou os efeitos de determinada dieta alimentar sobre o aumento
do peso corporal em cobaias adultas. Coletou seus pesos antes e três meses após a
gestão da nova dieta e obteve:
▪ Antes: 54 61 50 74 79 58 55 49 63
▪ Três meses Depois: 57 66 53 73 82 58 56 53 63

Considere as hipóteses: 
H0: μD = μA
H1: μD ≠ μA
Considere α = 0.05 e justifique sua resposta com relação às hipóteses estabelecidas.

qt(0.025, 8); qt(0.975, 8)

A <- c(54, 61, 50, 74, 79, 58, 55, 49, 63)
D <- c(57, 66, 53, 73, 82, 58, 56, 53, 63)

t.test(A,D, mu = 0, paired = TRUE, conf.level = 0.95)

Resp: Rejeita o HO, logo os pesos foram alterados três meses depois da dieta.


9 - Um professor afirma que os alunos vão baixando seus coeficientes de rendimento à
medida que avançam no curso. Oito alunos foram escolhidos aleatoriamente e
observado seus rendimentos nos semestres anterior e atual.
▪ Anterior: 89, 84, 96, 82, 74, 92, 85 e 91.
▪ Atual: 83, 83, 92, 84, 76, 91, 80 e 91.
Assumindo que os coeficientes são distribuídos normalmente, existe evidência
suficiente para apoiar a afirmação do professor para um nível de significância de 10%?
  
qt(0.05, 7); qt(0.95, 7)

A <- c(89, 84, 96, 82, 74, 92, 85, 91)
D <- c(83, 83, 92, 84, 76, 91, 80, 91)

t.test(A,D, mu = 0, paired = TRUE, conf.level = 0.95)

Resp: Aceita H0, logo, não há evidência suficiente para apoiar a afirmação do 
professor de que os coeficientes de rendimento dos alunos estão diminuindo
à medida que avançam no curso.


10 - Uma nova metodologia de desenvolvimento de software se propõe a reduzir o
tempo de projeto e desenvolvimento de sistemas de informação. Assim, foram
considerados 24 projetos, sendo 12 de tecnologia atual e 12 com a nova proposta. Os
valores, em horas, estão a seguir:
▪ TecAtual: 300, 280, 344,385, 372, 360, 288, 321, 376, 290, 301, 283
▪ TecNova: 274, 220, 308, 336, 198, 300, 315, 258, 318, 310, 332, 263

Considerando nível de significância de 2%, as hipóteses:
  
H0 : μଵ − μଶ ≤ 0
H1 : μଵ − μଶ > 0

Calcule t, o p-value e interpretar os resultados.

qt(0.01, 11); qt(0.99, 11)

A <- c(300, 280, 344,385, 372, 360, 288, 321, 376, 290, 301, 283)
N <- c(274, 220, 308, 336, 198, 300, 315, 258, 318, 310, 332, 263)

t.test(A,N, mu = 0, paired = FALSE, conf.level = 0.98)

Resp: Portanto, não rejeitamos a hipótese nula (H0) a um nível
de significância de 2%. Isso significa que, com base nos dados,
não há evidência suficiente para concluir que a nova metodologia
de desenvolvimento de software reduz significativamente o tempo de projeto
e desenvolvimento em relação à tecnologia atual.

