2. Em uma fabricação artesanal de componentes ceramicos 12% apresentam defeitos.
Utilizando distribuição Binomial, calcular a probabilidade de, em um lote de quarenta
componentes, encontrar:
  
a) Entre 3 e 5 componestes estejam defeituosos, inclusive.
resp = pbinom(5, size = 40, prob=0.12) - pbinom(2, size = 40, prob = 0.12)
resp

b) Pelo menos dois componentes defeituosos.
resp1 = 1 - pbinom(1, size = 40, prob = 0.12)
resp1

c) No máximo 3 componentes defeituosos.
resp2 = pbinom(3, size = 40, prob = 0.12)
resp2

d) Pelo menos 39 componentes de qualidade.
1 - pbinom(38, size = 40, prob= 0.88)

e) No máximo 39 componentes de qualidade.
pbinom(39, size = 40, prob = 0.88)


3. O Corpo de Bombeiros de uma determinada cidade recebe, em média, 3 chamadas por dia.
Qual a probabilidade de receber:

a. 4 chamadas num dia.
dpois(4, lambda = 3)

b. Nemhuma chamada em um dia.
dpois(0, lambda = 3)

c. 20 chamadas em uma semana.
dpois(20, lambda = 21)


4. Tem-se uma carteira com 15 ações. No pregão de ontem 75% das ações na bolsa de valores
caíram de preço. Supondo que as ações que perderam valor têm distribuição binomial,
calcule:
  
a. Quantas ações da carteira se espera que tenham caído de preço?
15*0.75
  
b. Qual o desvio padrão das ações da carteira?
var = 15*0.75*0.25
desvio = sqrt(var)
desvio
  
c. Qual a probabilidade que as 15 ações da carteira tenham caído?
dbinom(15, size = 15, prob = 0.75)
  
d. Qual a probabilidade que tenham caído de preço exatamente 10 ações?
dbinom(10, size = 15, prob = 0.75)
  
e. Qual a probabilidade que 13 ou mais ações da carteira tenham caído de preço?
pbinom(12, size = 15, prob = 0.75, lower.tail = F)


5. Uma moeda é lançada 6 vezes, encontre a probabilidade de:
a. Ocorrer 4 coroas;
dbinom(4, size = 6, prob = 1/2)

b. Ocorrer pelo menos 2 coroas;
pbinom(1, size = 6, prob = 1/2, lower.tail = F)

c. Ocorrer no máximo 3 coroas.
pbinom(3, size = 6, prob = 1/2)


8. Em fábrica de tubulações hidráulicas, o setor de qualidade sabe que 15 % do que é
produzido tem algum defeito, para averiguar tirou uma amostra de 10 tubulações. Qual a
probabilidade de não mais do uma seja defeituosa?
pbinom(1, size = 10, prob = 0.15)

9. Em um município 70% da população são favoráveis a um certo projeto. Se uma amostra de
5 pessoas for retirada dessa população, qual a probabilidade de exatamente 3 pessoas
serem favoráveis a projeto?
dbinom(3, size = 5, prob = 0.70)


10. A experiência passada indica que um número médio de 6 clientes por hora param
para colocar gasolina numa bomba.
a. Qual é a probabilidade de 3 clientes pararem qualquer hora?
dpois(3, lambda = 6)
  
b. Qual é a probabilidade de 3 clientes ou menos pararem em qualquer hora?
ppois(3, lambda = 6)
  
c. Qual é o valor esperado, a média, e o desvio padrão para esta distribuição?
media = 6
desvio = sqrt(media)


11. Um processo de produção produz 10 itens defeituosos por hora. Encontre a probabilidade
que 4 ou menos itens sejam defeituosos numa retirada aleatória por hora.
ppois(4, lambda = 10)


12. Uma indústria de tintas recebe pedidos de seus vendedores através da internet. A taxa
média é de 5 pedidos por hora.
a. Qual a probabilidade de a indústria receber mais de dois pedidos por hora?
ppois(2, lambda = 5, lower.tail = F)
1 - ppois(2, lambda = 5)

b. Em um dia de trabalho (8 horas) qual seria a probabilidade de haver 50 pedidos?
dpois(50, lambda = 40)  


13. Uma moeda é lançada 5 vezes:
a. Construa a função de Probabilidade.

b. Construa a função Distribuição acumulada.

c. Qual a probabilidade de ocorrer três caras?
dbinom(3, size = 5, prob = 1/2)  

d. Qual a probabilidade de ocorrer pelo menos três caras?
pbinom(2, size = 5, prob = 1/2, lower.tail = F)
1 - pbinom(2, size = 5, prob = 1/2)


15. Dois adversários A e B disputam uma série de 8 partidas de um determinado jogo. A
probabilidade de A ganhar uma partida é 0,6 e não há empate. Qual é a probabilidade de A
ganhar a série?
pbinom(4, size = 8, prob = 0.6, lower.tail = FALSE)


Distribuição normal

1. O volume de correspondência recebido por uma firma quinzenalmente tem distribuição
normal com média de 4.000 cartas e desvio padrão de 200 cartas. Qual a Porcentagem de
quinzenas em que a firma recebe:
a. Entre 3.600 e 4.250 cartas?
pnorm(4250, mean = 4000, sd = 200) - pnorm(3600, mean = 4000, sd = 200)
  
b. Menos de 3.400 cartas?
pnorm(3400, mean = 4000, sd = 200)
  
c. Mais de 4.636 cartas?
pnorm(4636, mean = 4000, sd = 200, lower.tail = F)


2. Uma enchedora automática de refrigerantes está regulada para que o volume médio de
líquido em cada garrafa seja de 1000 cm3 e desvio padrão de 10 cm3. Admita que o volume
siga uma distribuição normal.
a. Qual é a porcentagem de garrafas em que o volume de líquido é menor que 990 cm3?
pnorm(990, mean = 1000, sd = 10)
  
b. Qual é a porcentagem de garrafas em que o volume de líquido não se desvia da média
em mais do que dois desvios padrões?

    
c. Se 10 garrafas são selecionadas ao acaso, qual é a probabilidade de que, no máximo, 4
tenham volume de líquido superior a 1002 cm3?

  
3. Em um certo teste de aptidão para contratação de determinada empresa, os candidatos
devem realizar uma sequência de tarefas no menor tempo possível. Suponhamos que o
tempo necessário para completar esse teste tenha uma distribuição Normal com média 45
minutos e desvio-padrão de 20 minutos. Suponhamos que, numa primeira etapa, esse teste
foi aplicado com uma amostra de 50 candidatos. Qual a probabilidade de encontrarmos
algum candidato que tenha um tempo superior a 50 minutos (candidato muito lento) ou
inferior a 30 minutos (que seria impossível completar o teste)? Qual o número aproximado
de candidatos com tal perfil?
m50 = pnorm(50, mean = 45, sd = 20, lower.tail = F)
m30 = pnorm(30, mean = 45, sd = 20)
m30
m30 + m50
c30 = m30 * 50
c30
c50 = m50 * 50
c50
tc = c30 + c50
tc

4. Uma máquina produz discos de diâmetro médio de 2 cm com desvio padrão de 0,01 cm. As
peças que se afastam por mais de 0,03 cm desse valor médio são consideradas com defeito.
Qual o percentual de peças consideradas defeituosas?
pnorm(2.03, mean = 2, sd = 0.01, lower.tail = F)


5. A vida média de uma marca de televisão é de 8 anos com desvio padrão de 1,8 anos. A
campanha de lançamento diz que todos os produtos que tiverem defeito dentro do prazo
de garantia serão trocados por novos. Se você fosse o gerente de produção, qual seria o
tempo de garantia que você especificaria para ter no máximo 5% de trocas?
qnorm(0.05, mean = 8, sd = 1.8)

9. Sabe-se que os pacotes de queijo ralado vendido em supermercado possuem distribuição
normal com média 100 g e desvio padrão de 10 g.

a. Qual a probabilidade de encontrar um pacote com peso no intervalo de 95g e 105g?
pnorm(105, mean = 100, sd = 10) - pnorm(95, mean = 100, sd = 10)  

b. Se 16 pacotes são escolhidos ao acaso qual a probabilidade de o peso médio estar entre
95g e 105 g.?
pp = pnorm(105, mean = 100, sd = 10) - pnorm(95, mean = 100, sd = 10)
pp
r = pp * 16
r


18. Suponha que as medidas da corrente elétrica em pedaço de fio sigam a distribuição Normal,
com uma média de 10 miliamperes e uma variância de 4 miliamperes.

a. (a) Qual a probabilidade de a medida exceder 13 miliamperes?
pnorm(13, mean = 10, sd = 2, lower.tail = F)
  
b. (b) Qual a probabilidade de a medida da corrente estar entre 9 e 11 miliamperes?
pnorm(11, mean = 10, sd = 2) - pnorm(9, mean = 10, sd = 2)  

c. (c) Determine o valor para o qual a probabilidade de uma medida da corrente estar
abaixo desse valor seja 0,98.
qnorm(0.98, mean = 10, sd = 2)


19. Um elevador tem seu funcionamento bloqueado se sua carga for superior a 450 Kg. Adultos tem
pesos que obedecem a uma distribuição normal, com peso 70 Kg e desvio padrão 15 Kg.
Calcular a probabilidade de ocorrer bloqueio e uma tentativa de transportar 6 adultos.
b = pnorm(450, mean = 70, sd = 15, lower.tail = F)
b
b*6


Probabilidade exponencial
10. O tempo até a falha de um conjunto de motores elétricos tem uma distribuição Exponencial
com média de 28.500 horas. Qual a probabilidade de um destes ventiladores falhar nas
primeiras 24.000 horas de funcionamento?
pexp(24000, rate = 1/28500)


11. Considere que o tempo de resposta de um monitor de LCD tenha distribuição exponencial
com média igual a 5 milissegundos, responda:
a) Qual a probabilidade de o tempo de resposta ser de no máximo 10 milissegundos?
pexp(10, rate = 1/5)

b) Qual a probabilidade de o tempo de resposta estar entre 5 e 10 milissegundos?
pexp(10, rate = 1/5) - pexp(5, rate = 1/5)


12. As indústrias químicas têm alto custo de pesquisa para descobrir novas formas para agilizar
certas reações químicas. Uma delas é o uso de catalisadores por enzimas (cinética
                                                                         enzimática) que normalmente atendem a distribuições exponenciais.
Considerando que uma dessas reações com catalisação de enzimas demore em média
4.000 segundos, calcule:
a) a probabilidade de uma reação durar mais de 2.000 segundos.
pexp(2000, rate = 1/4000, lower.tail = F)

b) a probabilidade de uma reação durar pelo menos 6.000 segundos, sabendo-se que ela já
durou 4.000 segundos?
pexp(2000, rate = 1/4000, lower.tail = F)


13. Uma fábrica de carros sabe que os motores de sua fabricação têm duração normal com média
150000 km e desvio-padrão de 5000 km. Qual a probabilidade de que um carro, escolhido ao
acaso, dos fabricados por essa firma, tenha um motor que dure:
a) Menos de 170000 km?
pnorm(170000, mean = 150000, sd = 5000)
  
b) Entre 140000 km e 165000 km?
pnorm(165000, mean = 150000, sd = 5000) - pnorm(140000, mean = 150000, sd = 5000)
  
c) Se a fábrica subsƟtui o motor que apresenta duração inferior à garanƟa, qual deve ser esta
garanƟa para que a porcentagem de motores subsƟtuídos seja inferior a 0,2%?
qnorm(0.002, mean = 150000, sd = 5000)


14. Suponha que para uma população de adultos o peso seja uma variável aleatória normalmente
distribuída, com média 75 kg e desvio padrão 8 kg. Além disso, suponha que, para adultos
praƟcantes de Sumô, a distribuição dos pesos, é também Normal, com média 110 kg e desvio
padrão 5 kg. Sete adultos comuns e três praƟcantes adultos de Sumô estão prestes a entrar
num elevador com a seguinte inscrição: “Capacidade máxima 10 pessoas ou 850 kg”.
Determine a probabilidade de que o peso dessas 10 pessoas ultrapasse a capacidade máxima
do elevador.


15. A capacidade máxima de um elevador é 500kg. Se a distribuição dos pesos de cada usuário é
normal com média 70 e variância 100:
a) Qual é a probabilidade de sete passageiros ultrapassarem esse limite?
l = pnorm(500, mean = 70, sd = 10, lower.tail = F)  
l
l * 7 
b) E seis passageiros?
  

6. Suponha que, em determinado período do dia, o tempo médio de atendimento em um
caixa de banco seja de 5 minutos. Admitindo que o tempo para atendimento tenha
distribuição exponencial, determinar a probabilidade de um cliente:
a) esperar mais do que 5 minutos;
pexp(5, rate = 1/5, lower.tail = F)

b) esperar menos do que 4 minutos;
pexp(4, rate = 1/5)

c) esperar entre 3 e 8 minutos.
pexp(8, rate = 1/5) - pexp(3, rate = 1/5)