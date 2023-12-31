
######################### ANCOVA #########################


# Passo 1: Carregar os pacotes que ser�o usados

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                  
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)
if(!require(car)) install.packages("car")   
library(car)             
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(multcomp)) install.packages("multcomp") 
library(multcomp)
if(!require(emmeans)) install.packages("emmeans") 
library(emmeans)

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diret�rio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.csv2('Banco de Dados 8.csv', stringsAsFactors = T,
                   fileEncoding = "latin1")
View(dados) 
glimpse(dados)


# VD: Sal�rio
# VI: Grau de instru��o
# Covari�vel: Idade


# Passo 3: Verificar se h� efeito da VI sobre a covari�vel (cov ~ VI)
# Pressuposto: "independ�ncia entre a VI e a covari�vel"
# Se rompido: n�o h� outro modelo - � um problema de delineamento
mod_cov <- aov(Idade ~ Grau_Instru��o, data = dados)
summary(mod_cov)


# Passo 4: Verificar se a rela��o entre a covari�vel e a VD � linear (VD ~ cov)
ggplot(data = dados, aes(x = Idade, y = Sal�rio, group = Grau_Instru��o,
                         color = Grau_Instru��o)) +
  geom_point(size = 2) +
  xlab('Idade') +
  ylab('Sal�rio') +
  geom_smooth(method = "lm", se = FALSE, size = 0.5)


# Passo 5: Verificar se o efeito da covari�vel � o mesmo para todos n�veis da VI (VD ~ VI*cov)
# Pressuposto: "homogeneidade dos par�metros de regress�o"
# Compara as inclina��es das retas para cada grupo da VI
mod_int <- aov(Sal�rio ~ Grau_Instru��o*Idade, data = dados)
Anova(mod_int, type = 'III')



# Passo 6: Verificar se h� homogeneidade de vari�ncias (VD ~ VI)
# Se rompido: vers�o robusta da ANCOVA
leveneTest(Sal�rio ~ Grau_Instru��o, center = mean, data = dados)



# Passo 7: Ajustar o modelo de ANCOVA (VD ~ cov + VI)
## Se os resultados forem avaliados pelo tipo I da soma dos quadrados
## � obrigat�rio que a covari�vel seja inserida antes no modelo
options(contrasts = c("contr.sum", "contr.poly"))

mod_ANCOVA <- aov(Sal�rio ~ Idade + Grau_Instru��o, data = dados)
Anova(mod_ANCOVA, type = 'III')



# Passo 8: Verificar a normalidade dos res�duos
# Se rompido: vers�o robusta da ANCOVA
shapiro.test(mod_ANCOVA$residuals)


# Passo 9: Verificar se h� homocedasticidade e outliers
boxplot(mod_ANCOVA$residuals)

par(mfrow=c(1,2))
plot(mod_ANCOVA, which=c(1,3))


leveneTest(mod_ANCOVA$residuals ~ dados$Grau_Instru��o)


# Passo 10: Realiza��o das compara��es entre grupos

## Pelo pacote multcomp
posthoc <- glht(mod_ANCOVA, linfct = mcp(Grau_Instru��o = "Tukey"))
summary(posthoc)
## Outra op��o: Dunnett


## Pelo rstatix
comparacoes <- dados %>% emmeans_test(Sal�rio ~ Grau_Instru��o, covariate = Idade,
                       p.adjust.method = "bonferroni")



# Passo 11: Obten��o das m�dias ajustadas
## Op��o 1:
medias_ajustadas <- emmeans(mod_ANCOVA, ~ Idade:Grau_Instru��o)
medias_ajustadas


## Op��o 2:
get_emmeans(comparacoes)


####### M�dias reais
dados %>% group_by(Grau_Instru��o) %>% 
  get_summary_stats(Sal�rio, type = "mean_sd")


