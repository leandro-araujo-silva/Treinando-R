install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)


url <- "TabelaLivro2.csv"

milsa <- read.csv(url, header = TRUE, sep = ";", dec = ".")

milsa

str(milsa) 

# GRAFICO DE Barras

civil.tb <- table(milsa$Est.civil)

barplot(civil.tb)

barplot(civil.tb, cex.names=1.25, col=c("green","blue"),
        ylab="N?mero de Funcion?rios", 
        xlab="Estado civil", cex.axis=1.25,
        main="Propor??o entre casados e solteiros",
        cex.lab=1.25,bty="n", ylim=c(0,25))


# GRÁFICO DE BARRAS COM A REGIÃO

# Criar um gráfico de barras para a contagem de pessoas por região
grafico <- ggplot(milsa, aes(x = Regiao)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Quantidade de Pessoas por Região", x = "Região", y = "Quantidade") +
  theme(plot.title = element_text(hjust = 0.5))

print(grafico)


# GRAFICO DE PIZZA

labs<-paste(c("Casados = ", "Solteiros = "),
       round(civil.tb/length(milsa$Est.civil), 
       digits=2), "%")

pie(civil.tb,labels=labs,col=c("green", "blue"),
       main="Propor??o entre casados e solteiros")


#Plotando legenda no canto superior direito

legend("topright", pch=15, col=c("green","blue"), 
       legend=c("Casados", "Solteiros"), 
       cex=1.0, bty="n")

## Variavel Qualitativa Ordinal 

## Frequ?ncia absoluta

inst.tb <- table(milsa$Instrucao)

## Frequ?ncia relativa
prop.table(inst.tb)

## Gr?fico de Barras

barplot(sort(inst.tb,decreasing = FALSE),
        cex.names=1.15, 
        col=c("green", "blue", "red"),
        ylab="Instru??o de Funcion?rios", 
        xlab="Escolaridade", cex.axis=1.25,
        main="Escolaridade dos Funcion?rios",
        cex.lab=1.25,bty="n", ylim=c(0,20))

barplot(sort(inst.tb,decreasing = TRUE),
        cex.names=1.15, 
        col=c("green", "blue", "red"),
        ylab="Instru??o de Funcion?rios", 
        xlab="Escolaridade", cex.axis=1.25,
        main="Escolaridade dos Funcion?rios",
        cex.lab=1.25,bty="n", ylim=c(0,20))

## Vari?vel quantitativa discreta

## Frequ?ncia absoluta

filhos.tb <- table(milsa$Filhos)
milsa$Filhos

plot(filhos.tb, col =  "green", type = "h",
     lwd = 5, cex.lab=1.2,
     main = " Frequ?ncia Absoluta ",
     xlab= "N?mero de filhos",
     ylab= "Quantidade de Filhos ") 

plot(filhos.tb, type = "S",col = "red",
     main = "Frequ?ncia relativa acumulada",
     lwd = 5 )

## Frequ?ncia relativa
filhos.tbr <- prop.table(filhos.tb)

## Frequ?ncia absoluta e relativa acumulada
filhos.tbra <- cumsum(filhos.tbr)
filhos.tba <- cumsum(filhos.tb)

filhosTabResul = cbind(filhos.tb,filhos.tba,
                 filhos.tbr = round(filhos.tbr*100,digits = 2),
                 filhos.tbra= round(filhos.tbra*100,digits = 2))
filhosTabResul

## Moda
names(filhos.tb)[which.max(filhos.tb)]
 
## Mediana
median(milsa$Filhos, na.rm = TRUE)
 
## M?dia
mean(milsa$Filhos, na.rm = TRUE)

## Quartis
quantile(milsa$Filhos, na.rm = TRUE)

## M?ximo e m?nimo
max(milsa$Filhos, na.rm = TRUE)

min(milsa$Filhos, na.rm = TRUE)

## As duas informa??es juntas
range(milsa$Filhos, na.rm = TRUE)

## Amplitude ? a diferen?a entre m?ximo e m?nimo
diff(range(milsa$Filhos, na.rm = TRUE))

## Vari?ncia
var(milsa$Filhos, na.rm = TRUE)

## Desvio-padr?o
sd(milsa$Filhos, na.rm = TRUE)

## Coeficiente de varia??o
sd(milsa$Filhos, na.rm = TRUE)/mean(milsa$Filhos, na.rm = TRUE)
 
## Quartis
(filhos.qt <- quantile(milsa$Filhos, na.rm = TRUE))

## Summary() para resumir os dados de uma s? vez
summary(milsa$Filhos)

## Vari?vel quantitativa Continua

Salario.tb <- (milsa$Salario)
sort (Salario.tb)

Amplitude <- max (Salario.tb) - min(Salario.tb) ; Amplitude

NK <-  nclass.Sturges (Salario.tb) ; NK 
AmpClasse <- Amplitude / NK ; AmpClasse 
AmpClasse <- 2.8

limitesclas <- seq (4 , 23.60 , AmpClasse )

classes<-c("04.0 |- 06.8","06.8 |- 09.6","09.6 |- 12.4","12.4 |- 15.2",
           "15.2 |- 18.0","18.0 |- 20.8","20.8 |- 23.60")

Freq = table(cut(Salario.tb, breaks = limitesclas, right=FALSE, 
                 include.lowest = TRUE, labels=classes))

FreqAc <- cumsum(Freq); FreqRel <- prop.table(Freq); FreqRelAc <- cumsum(FreqRel)

TabResul = cbind(Freq,FreqAc, FreqRel = round(FreqRel*100,digits = 2),
                              FreqRelAc= round(FreqRelAc*100,digits = 2))
TabResul

h = hist(Salario.tb, breaks=limitesclas,
    ylab="Frequencias absolutas",  labels=classes,main="Histograma", 
    xlim=c(4,25), ylim = c (0,12), col="orange")

lines(c(min(h$breaks), h$mids, max(h$breaks)), 
       c(0,h$counts, 0), type = "l")
       
## Mediana
       median(milsa$Salario)
## M?dia
       mean(milsa$Salario)
       
## Quartis
       quantile(milsa$Salario)
       
## Valorees minio e maximo
       range(milsa$Salario)
       
## Amplitude ? a diferen?a entre m?ximo e m?nimo
       diff(range(milsa$Salario))
       
## Vari?ncia
       var(milsa$Salario)
## Desvio-padr?o
       sd(milsa$Salario)
       
## Coeficiente de varia??o
       sd(milsa$Salario)/mean(milsa$Salario)
       
## Quartis
       (Salario.qt <- quantile(milsa$Salario))
       
## Summary() para resumir os dados de uma s? vez
       summary(milsa$Salario)

boxplot(milsa$Salario)

boxplot(milsa$Salario,  col = "orange", varwidth = TRUE, notch = TRUE)

#----------------------------- ANALISE BIVARIADA ----------------------------

# ---------------- Variavel Qualitativa X Variavel Qualitativa --------------

inst.civ.tb <- with(milsa,table(Instrucao, Est.civil))
inst.civ.tb

civ.inst.tb <- with(milsa,table(Est.civil, Instrucao))
addmargins(civ.inst.tb)

prop.table(civ.inst.tb)

prop.table(civ.inst.tb, margin = 1)

prop.table(civ.inst.tb, margin = 2)

par(mfrow = c(1,1))

# Estado civil x instru??o 

barplot(civ.inst.tb, main = "Estado Civil x Instru??o",
        col= c("green","blue"), beside = TRUE,
        legend = TRUE,  args.legend = list(x = "topright",
        bty = "y", ncol = 1))

#  instru??o X Estado civil

barplot(inst.civ.tb, main = "Instru??o x Estado civil",
        col= c("green","blue", "yellow"),
        beside = TRUE,legend = TRUE, 
        args.legend = list(x = "topright",
        bty = "n", ncol = 1))

#  instru??o X Estado civil na Horizontal 

barplot(inst.civ.tb, main= "Instru??o x Estado civil",
        col= c("green","blue", "yellow"),
        beside = TRUE,legend = TRUE, 
        args.legend = list(x = "topright",bty = "n",
        ncol = 1), horiz = TRUE)
        
#  instru??o X Estado civil Agrupado --> beside = FALSE 

barplot(inst.civ.tb, main = "Instru??o x Estado civil",
        col= c("green","blue", "yellow"),legend = TRUE,
        beside = FALSE, 
        args.legend = list(x = "topright",bty = "n",
        ncol = 1), ylim = c (0,26), )


## Qualitativa vs quantitativa (vari?veis Instru??o e Sal?rio) --------

## Quartis de salario

quantile(milsa$Salario)

## Classifica??o de acordo com os quartis

salario.cut <- cut(milsa$Salario, breaks =  quantile(milsa$Salario),
                   include.lowest = TRUE, right = TRUE)

## Tabela de frequ?ncias absolutas
inst.sal.tb <- table(milsa$Inst, salario.cut)
inst.sal.tb

barplot(inst.sal.tb, col=c("yellow","red","orange"), main= "Sal?rio x Instru??o",
        xlab = "Quantiles", ylab = "Frequ?ncia  Instru??o",
        beside = TRUE, legend = TRUE)

boxplot(Salario ~ Instrucao, data = milsa, col=c("yellow", "red", "orange"))  

prop.table(inst.sal.tb)

# ---- Quantitativa vs Quantitativa (considerar as vari?veis Salario e Idade)

Anos.cut <- cut(milsa$Anos, breaks = quantile(milsa$Anos),
                right = FALSE , include.lowest = TRUE)

salario.cut <- cut(milsa$Salario, breaks = quantile(milsa$Salario),
                   right = FALSE, include.lowest = TRUE)
## Tabela cruzada

Anos.sal.tb <- table(Anos.cut, salario.cut)
Anos.sal.tb

plot(x = milsa$Anos, y = milsa$Salario,
     main = " Sal?rio x Anos",
     xlab = "Anos - Vari?vel preditora", 
     ylab = "Sal?rio - Vari?vel resposta",
     col = "blue", cex.axis = 0.75, 
     xlim = c(20, 50), ylim = c(0, 25))

abline(lsfit(milsa$Anos, milsa$Salario),col="darkred")

## Correla??o - verificar associa??o entre variaveis quatitativas

cor(milsa$Anos, milsa$Salario)

# Exemplo 1.

url <- "C://CCEstatistica 2022/RegPapel.csv"

reg <- read.csv(url, header = TRUE, sep = ";", dec = ",")

reg
cor (reg$PesoTotal, reg$PesoPapel)
plot(reg$PesoTotal, reg$PesoPapel,
     main = "Peso do Papel x Peso Total",
     col = "blue",pch=21, lwd = 2)

abline(lsfit(reg$PesoTotal, reg$PesoPapel),col="darkred")

data(anscombe) 
dados <- anscombe 
str(dados)

cor (dados$x1, dados$y1)

cordados = cbind (dados$x1, dados$y1, dados$y2,
                  dados$y3, dados$y4)
cor (cordados)

plot(dados$x1)
plot(dados$x1, dados$y1)
plot(y1 ~ x1, data=dados) 

data("Orange")
Orange

plot(Orange$age, Orange$circumference,
     col = "blue",pch=21, lwd = 2)

abline(lsfit(Orange$age, Orange$circumference),col="darkgreen")

cor(Orange$age, Orange$circumference)
