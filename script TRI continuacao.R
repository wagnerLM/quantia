# Teoria de Resposta ao Item com R

#pacotes
install.packages("mirt")
install.packages("eRm")
#ativando os pacotes
library(mirt)
library(eRm)
# Exemplo politômico
dasspoly<-read.csv("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasspoly",sep = ";")
View(dasspoly)
# Exemplo dicotômico
dassbin<-read.csv("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassbin",sep = ";")
View(dassbin)
# Nome resumido dos itens
dasslabels<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasslabels",what = "character", sep = "\n")
dasslabels
# Itens completos 
dassnames<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassnames",what = "character", sep = "\n")
dassnames

# Selecionando itens de depressão
dassbin_sub<-dassbin[,c(3,5,10,13,16,17,21)]
dasspoly_sub<-dasspoly[,c(3,5,10,13,16,17,21)]

# Modelo de 2 parâmetros (2PL)
mod1<-mirt(dassbin_sub,1)
coef(mod1)
summary(mod1)
plot(mod1)
plot(mod1, type = 'trace')
plot(mod1, type = 'info')
M2(mod1)

# Modelo de 1 parâmetro (Rasch no mirt)
mod2<-mirt(dassbin_sub,1,itemtype = "Rasch")
coef(mod2)
summary(mod2)
plot(mod2)
plot(mod2, type = 'trace')
plot(mod2, type = 'info')
M2(mod2)
# Comparando modelos em termos de resíduos
anova(mod2,mod1)

# Modelo de 1 parâmetro (Rasch no eRm)
mod3<-RM(dassbin_sub)
summary(mod3)
par(mfrow=c(1,1))
plotINFO(mod3,type="item")
#medidas de ajuste
summary(gof.res)
pres <- person.parameter(mod3)
IC(pres)
itemfit(pres)
personfit(pres)
gof.res <- gofIRT(pres)
gof.res
#curvas
plotICC(mod3)
plotICC(mod3, empICC=list("raw"))
plotjointICC(mod3, legpos = "left")

# Leituras
# Artigo introdutório "http://pepsic.bvsalud.org/pdf/avp/v2n2/v2n2a02.pdf"
# Artigo avançado "https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1745-3992.1997.tb00606.x"

### Continuação

mod4<-mirt(dasspoly_sub,1,itemtype = "graded")
coef(mod4)
summary(mod4)
plot(mod4)
plot(mod4, type = 'trace')
plot(mod4, type = 'info')
M2(mod4,type = "C2")

### DIF funcionamento diferencial 
ESV<-read.csv("https://raw.githubusercontent.com/wagnerLM/SBP/master/ESV.csv",sep=";")
View(ESV)
itemnames<-colnames(ESV[,-c(6,7)])
itemnames
ESV_dif<-multipleGroup(ESV[1:5],1, group = as.factor(ESV$Sexo), invariance = c("free_means","free_var",itemnames[3]), method = "EM",
              dentype = "Gaussian")
coef(ESV_dif)
plot(ESV_dif)
plot(ESV_dif, type = 'trace')
plot(ESV_dif, type = 'info')
DIF(ESV_dif,"a1",items2test = "ESV5",plotdif =T)
DIF(ESV_dif,"d6",items2test = "ESV5",plotdif =T)

### DIF para itens dicotômicos

dassbin_sex<-read.csv("https://raw.githubusercontent.com/wagnerLM/quantia/master/dass_sex",sep = ";")
dassbin_sex<-na.omit(dassbin_sex)
dassnames2<-colnames(dassbin_sex[,c(4,6,11,14,17,18,22)])
dassnames2
dass_dif<-multipleGroup(dassbin_sex[,c(4,6,11,14,17,18,22)],1, group = as.factor(dassbin_sex$sex), invariance = c("free_means","free_var",dassnames2[7]), method = "EM",
                       dentype = "Gaussian")
coef(dass_dif)
plot(dass_dif)
plot(dass_dif, type = 'trace')
plot(dass_dif, type = 'info')
DIF(dass_dif,"a1",plotdif =T)
DIF(dass_dif,"d",plotdif =T)