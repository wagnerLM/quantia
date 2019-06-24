### Bayesian Networks
#
install.packages("bnlearn")

BNA<-read.csv("https://raw.githubusercontent.com/wagnerLM/quantia/master/bna_banco",sep=";")
str(BNA)
View(BNA)
BNA2<-na.omit(BNA)
View(BNA2)
### redes não direcionais
library(qgraph)
BNA_g<-qgraph(cor_auto(BNA),layout="spring",graph="glasso",sampleSize=nrow(BNA),threshold=T,lambda.min.ratio=0.001,labels=colnames(BNA2),posCol="blue")

### redes direcionais
library(bnlearn)
BNA2$Sexo<-as.factor(BNA2$Sexo)
BNA2$SV<-as.numeric(BNA2$SV)
BNA2$AU<-as.numeric(BNA2$AU)
BNA2$AA<-as.numeric(BNA2$AA)
BNA2$Ans<-as.numeric(BNA2$Ans)
BNA2$Dep<-as.numeric(BNA2$Dep)

dag<-hc(BNA2)
dag2<-tabu(BNA2)
par(mfrow=c(1,2))
plot(dag)
plot(dag2)
par(mfrow=c(1,1))
fit<-bn.fit(dag2,BNA2)
fit
qgraph(dag2)
g_dag2<-qgraph(dag2)

## path analysis
library(lavaan)
model<-'
SV ~ Dep + AA
AA ~ Dep
Ans ~ Dep + Sexo
AU ~ AA + Ans
Dep ~~ 0*Sexo
'
model
?sem
fit2<-sem(model,na.omit(BNA),estimator = "MLR")
summary(fit2,standardized=T,fit.measures=T,rsq=TRUE)
