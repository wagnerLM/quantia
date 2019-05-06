#### Análise Fatorial Confirmatória ####
install.packages("lavaan")
install.packages("semTools")
install.packages("semPlot")
# Testando unidimensionalidade através da CFA

ESV<-read.csv("https://raw.githubusercontent.com/wagnerLM/SBP/master/ESV.csv",sep=";")
View(ESV)

# Especificando o modelo

ESV.mod <- '
SV =~ ESV1 + ESV2 + ESV3+ ESV4 + ESV5
'
library(lavaan)

fit.ESV.mod <- cfa(model = ESV.mod,data = ESV)

summary(fit.ESV.mod,fit.measures = TRUE, standardized=TRUE, rsq=TRUE)

# Fidedignidade
library(semTools)
reliability(fit.ESV.mod)

# Visualização do modelo em diagrama
library(semPlot)
semPaths(fit.ESV.mod, what = "std", edge.label.cex = 0.7,
         edge.color = 1, esize = 1, sizeMan = 4.5, asize = 2.5,
         intercepts = FALSE, rotation = 4, thresholdColor = "red",
         mar = c(1, 5, 1.5, 5), fade = FALSE, nCharNodes = 4)

# computando Escores fatoriais

ESV$Sat_Vida <- predict(fit.ESV.mod)
View(ESV)

### Repita as análises anteriores, estimando o modelo para dados ordinais