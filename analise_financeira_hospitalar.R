# organizacao -------------------------------------------------------------
getwd()
setwd('~/Documents/portfolio/analise_hospitalares_R')

# fontes ------------------------------------------------------------------
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Cost-Report/HospitalCostPUF
# https://healthdata.gov/

# dicio -------------------------------------------------------------------
# LOS = (length of stay) tempo internação
# TOTCHG = custo internação
# APRDRG = Grupo diagnóstico refinado do paciente

# install -----------------------------------------------------------------
# install.packages('sqldf')

# import ------------------------------------------------------------------
library(tidyverse)
require(sqldf)

# dados -------------------------------------------------------------------
dados <- read.csv('dataset.csv')

# conhecendo --------------------------------------------------------------
dim(dados)
glimpse(dados)
colSums(is.na(dados))
summary(dados)

# NA ----------------------------------------------------------------------
# somente 1 vou remover
dados <- na.omit(dados)
sum(!complete.cases(dados))

# SQL - ANÁLISE EXPLORATÓRIA ----------------------------------------------
colnames(dados)

# 1- Quantas raças estão representadas no dataset?
?sqldf
sqldf('SELECT RACE, COUNT(RACE) AS NUM_RACE
      FROM dados
      GROUP BY RACE
      ') 

# Seis raças (6)

# 2- Qual a idade média dos pacientes?
sqldf('SELECT AVG(age) AS idade_media
      FROM dados')

# 3- Qual a moda da idade dos pacientes?
sqldf("SELECT AGE AS Idade_moda FROM
      (SELECT AGE, COUNT(AGE) AS age_count FROM dados
      GROUP BY AGE
      ORDER BY AGE_COUNT DESC)
      LIMIT 1")

# 4- Qual a variância da coluna idade?
sqldf(
  'SELECT
  SUM(
  POWER(
  AGE - (SELECT AVG(AGE) FROM dados)
  ,2)) /
  (COUNT(AGE) - 1)
  AS variancia
  FROM dados'
)

# 5- Qual o gasto total com internações hospitalares por idade?
gasto_hosp <- sqldf(
  'SELECT AGE AS idade, 
  SUM(TOTCHG) AS gastos
  FROM dados
  GROUP BY AGE'
)
gasto_hosp
max(dados$AGE)

# 6- Qual idade gera o maior gasto total com internações hospitalares?
sqldf(
  'SELECT AGE, MAX(GASTOS_IDADE)
  AS MAIOR_GASTO
  FROM gasto_hosp
  '
)

# 7- Qual o gasto total com internações hospitalares por gênero?
sqldf(
  'SELECT FEMALE, SUM(TOTCHG)
  AS gasto_total
  FROM dados
  GROUP BY FEMALE'
)

# 8- Qual a média de gasto com internações hospitalares por raça do paciente?
sqldf('
  SELECT RACE AS gênero,
  AVG(TOTCHG) AS média_gasto
  FROM dados
  GROUP BY RACE'
)

# 9- Para pacientes acima de 10 anos, qual a média de gasto total com internações hospitalares?
gasto_maior_10 <- sqldf(
  'SELECT AGE AS idade,
  AVG(TOTCHG) AS média_gastos
  FROM dados
  WHERE AGE > 10
  GROUP BY AGE'
)
gasto_maior_10

# 10- Considerando o item anterior, qual idade tem média de gastos superior a 3000?
sqldf('SELECT idade, média_gastos
      FROM gasto_maior_10
      WHERE média_gastos > 3000')

sqldf(
  'SELECT AGE AS idade,
  AVG(TOTCHG) AS média_gastos
  FROM dados
  WHERE AGE > 10
  GROUP BY AGE
  HAVING AVG(TOTCHG) > 3000'
)

###########################################################################


# Análise de Regressão ----------------------------------------------------

# Pergunta 1: 
# Qual a distribuição da idade dos pacientes que frequentam o hospital?
ggplot(dados, aes(AGE)) +
  ggtitle('Distribuição Idade Pacientes') +
  geom_histogram(binwidth = 1) +
  xlab('Idade') +
  ylab('Contagem')

# Convertendo para fator temos a quantidade de pacientes por idade.
idade_factor <- as.factor(dados$AGE)
summary(idade_factor)
# Crianças entre 0 e 1 ano são as que mais frequentam o hospital.


# Pergunta 2:
# Qual faixa etária tem o maior gasto total no hospital?
ggplot(gasto_hosp, aes(x=idade, y=gastos / 1000)) +
  ggtitle('Gastos por faixa etária') +
  geom_col(aes(fill = gastos / 1000)) +
  xlab('Idade') +
  ylab('Gastos x 1000') +
  scale_y_continuous() +
  theme(legend.position = "none")
# Crianças entre 0 e 1 ano são as que geram maior gasto no hospital.

# Pergunta 3:
# Qual grupo baseado em diagnóstico (Aprdrg) tem o maior gasto total no hospital?
gasto_baseado_APRDRG <- dados %>% 
  aggregate(TOTCHG ~ APRDRG, FUN = 'sum')

gasto_baseado_APRDRG[which.max(gasto_baseado_APRDRG$TOTCHG), ]
# O grupo 640 tem o maior gasto total.

# Pergunta 4:
# A característica étnica do paciente tem relação com o total gasto em internações no hospital?

# Usei um Teste ANOVA.
# Variável dependente no Teste ANOVA: TOTCHG
# Variável independente no Teste ANOVA: Race

# H0: Não há efeito de RACE em TOTCHG.
# H1: Há efeito de RACE em TOTCHG.
mod_anova_race <- aov(TOTCHG ~ RACE, data = dados)
summary(mod_anova_race)
mod_anova_race
# O valor-p é maior que 0.05. Falhamos em rejeitar a H0.
# A característica étnica do paciente não influencia no gasto total com internação hospitalar.

# Pergunta 5:
# A combinação de idade e gênero dos pacientes influência no gasto total 
# em internações no hospital?

# Usaremos um Teste ANOVA.
# Variável dependente no Teste ANOVA: TOTCHG
# Variáveis independentes no Teste ANOVA: AGE, FEMALE

# H0: Não há efeito de AGE e FEMALE em TOTCHG.
# H1: Há efeito de AGE e FEMALE em TOTCHG.
mod_anova_idade_gen <- aov(TOTCHG ~ AGE + FEMALE, data = dados)
summary(mod_anova_idade_gen)

# Resposta da Pergunta 5:
# Em ambos os casos o valor-p é menor que 0.05. Rejeitamos a hipótese nula. 
# Há um efeito significativo da idade e do gênero nos custos hospitalares.

# Pergunta 6:
# Como o tempo de permanência é o fator crucial para pacientes internados, 
# desejamos descobrir se o tempo de permanência pode ser previsto a partir de 
# idade, gênero e raça.

#  Variável dependente: LOS
#  Variáveis independentes: AGE, FEMALE e RACE 

#  H0: Não há relação linear entre variáveis dependente e independentes.
#  H1: Há relação linear entre variáveis dependente e independentes.
mod_lm <- lm(LOS ~ AGE + FEMALE + RACE, dados)
summary(mod_lm)

# Valor-p maior que 0.05 em todos os casos, logo, falhamos em rejeitar a H0.
# O tempo de internação não pode ser previsto a partir das variáveis independentes usadas.

# Pergunta 7:
# Quais variáveis têm maior impacto nos custos de internação hospitalar?

#  Variável dependente: TOTCHG
#  Variáveis independentes: AGE, FEMALE, LOS, RACE e APRDRG

#  H0: Não há relação linear entre variáveis dependente e independentes.
#  H1: Há relação linear entre variáveis dependente e independentes.
mod_lm_custo <- lm(TOTCHG ~ . , dados)
summary(mod_lm_custo)

# Como podemos observar a partir dos valores dos coeficientes, as variáveis idade, 
# tempo de permanência (LOS) e grupo de diagnóstico refinado do paciente (APRDRG) 
# têm três asteriscos (***) ao lado. Então eles são os únicos com significância estatística
# Além disso, RACE não é significativa. 
# Vou remover RACE do modelo.
mod_lm_custo_v4 <- lm(TOTCHG ~ AGE + LOS + APRDRG + FEMALE, dados)
summary(mod_lm_custo_v4)

# A variável FEMALE não é significativa.
# Vou removê-la. 
mod_lm_custo_v3 <- lm(TOTCHG ~ AGE + LOS + APRDRG, dados)
summary(mod_lm_custo_v3)

# As 3 variáveis tem alta significância, mas APRDRG tem valor t negativo.
# Vou removê-la. 
mod_lm_custo_v2 <- lm(TOTCHG ~ AGE + LOS, dados)
summary(mod_lm_custo_v2)


# A remoção de raça e gênero não altera o valor de R2.
# A remoção do APRDRG no modelo aumenta o erro padrão. 
# Logo, o modelo mod_lm_custo_v3 parece ser o melhor e o usaremos para nossa conclusão.
summary(mod_lm_custo_v3)

# Conclusão:

# Como é evidente nos vários modelos acima, os custos dos cuidados de saúde dependem 
# da idade, do tempo de permanência e do grupo de diagnóstico.
# Essas são as 3 variáveis mais relevantes para explicar e prever o gasto com 
# internações hospitalares.
