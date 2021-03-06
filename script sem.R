library(psych)
library(lavaan)
library(semPlot)
# Banco
sem_b<-read.csv("https://raw.githubusercontent.com/wagnerLM/quantia/master/sem_exe",sep=";")
View(sem_b)
cor.plot(cor(sem_b),numbers = T)

# Crie um modelo especificando:
# SV como desfecho (VD), AP, AN, Idade, Sexo e Renda como preditores (VI´s)
sem_mod<-'
SV =~ SV1 + SV2 + SV3 + SV4 + SV5
AP =~ animado + dinamico + entusiasmado + inspirado + produtivo
AN =~ amedrontado + inquieto + irritado + nervoso + perturbado
SV ~ AP + AN + idade + sexo + rendafam
'
# Avalie o modelo:
sem_fit<-sem(sem_mod,sem_b,ordered = colnames(sem_b))
summary(sem_fit,fit.measures=T,standardized=T,rsq=T)

# Visualize o modelo:
semPaths(sem_fit,what = "std",fade=F,style = "lisrel",intercepts = F,thresholds = F,rotation = 2,layout = "tree2",edge.color = "black",residuals = F)

# Veja a existência de índices de modificação:
modindices(sem_fit)
# Quais mudanças são relevantes? Possuem sentido teórico?
# Faça uma abordagem hierárquica, escolhendo a implementando as modificiações com maior índice e que possuam sentido teórico

# Reespecifique seu modelo, incluindo a relação mais relevante: 
sem_mod2<-'
SV =~ SV1 + SV2 + SV3 + SV4 + SV5
AP =~ animado + dinamico + entusiasmado + inspirado + produtivo
AN =~ amedrontado + inquieto + irritado + nervoso + perturbado + SV5
SV ~ AP + AN + idade + sexo + rendafam
'
sem_fit2<-sem(sem_mod2,sem_b,ordered = colnames(sem_b))
summary(sem_fit2,fit.measures=T,standardized=T,rsq=T)
# Compare os modelos, com o teste de ANOVA: 
anova(sem_fit,sem_fit2)
