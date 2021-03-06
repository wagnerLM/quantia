# Teoria de Resposta ao Item com R

#pacotes
install.packages("mirt")
install.packages("eRm")
#ativando os pacotes
library(mirt)
library(eRm)
# Exemplo polit�mico
dasspoly<-read.csv("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasspoly",sep = ";")
View(dasspoly)
# Exemplo dicot�mico
dassbin<-read.csv("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassbin",sep = ";")
View(dassbin)
# Nome resumido dos itens
dasslabels<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasslabels",what = "character", sep = "\n")
dasslabels
# Itens completos 
dassnames<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassnames",what = "character", sep = "\n")
dassnames

# Selecionando itens de depress�o
dassbin_sub<-dassbin[,c(3,5,10,13,16,17,21)]
dasspoly_sub<-dasspoly[,c(3,5,10,13,16,17,21)]

# Modelo de 2 par�metros (2PL)
mod1<-mirt(dassbin_sub,1)
coef(mod1)
summary(mod1)
plot(mod1)
plot(mod1, type = 'trace')
plot(mod1, type = 'info')
M2(mod1)

# Modelo de 1 par�metro (Rasch no mirt)
mod2<-mirt(dassbin_sub,1,itemtype = "Rasch")
coef(mod2)
summary(mod2)
plot(mod2)
plot(mod2, type = 'trace')
plot(mod2, type = 'info')
M2(mod2)
# Comparando modelos em termos de res�duos
anova(mod2,mod1)

# Modelo de 1 par�metro (Rasch no eRm)
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
# Artigo introdut�rio "http://pepsic.bvsalud.org/pdf/avp/v2n2/v2n2a02.pdf"
# Artigo avan�ado "https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1745-3992.1997.tb00606.x"