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