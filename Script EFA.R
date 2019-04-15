# Diagnóstico de itens e pressupostos da Análise Fatorial Exploratória
install.packages("psych")
library(psych)
# Pressupostos, correlação e diagnóstico de itens (inspeção gráfica)
# A análise fatorial é uma técnica que parte da hipótese da causa comum, 
# isto é, a correlação entre itens de um teste ou de subtestes
# é explicada por uma ou mais variáveis latentes, não observáveis. 
# Dito de outra forma, a análise fatorial assume um efeito causal a 
# partir de correlações entre observáveis

# Para investigar essas correlações, vamos observar a distribuição dos itens:
# use este comando para redefinir o plano de gráficos "par(mfrow=c(1,1))"

# carregando os bancos
ESV<-read.csv("https://raw.githubusercontent.com/wagnerLM/SBP/master/ESV.csv",sep=";")
View(ESV)
# Acrescentando labels
ESV_labels<-list("A minha vida está próxima do meu ideal","Minhas condições de vida são excelentes","Eu estou satisfeito com a minha vida","Até agora eu tenho conseguido as coisas importantes que eu quero na vida"," Se eu pudesse viver a minha vida de novo eu não mudaria quase nada")
ESV_labels
View(ESV_labels)
# construa histogramas das variáveis
hist(ESV[,1])
shapiro.test(ESV[,1])

# observe as correlações entre os itens
cor.plot(ESV[,-c(6,7)],numbers = TRUE)

# por fim, produza um gráfico com scatter plot, histogramas e correlações
pairs.panels(ESV[,-c(6,7)], histogram=TRUE, pch=19)

#KMO
?KMO
KMO(ESV[,-c(6,7)])
# Bartlett
?cortest.bartlett
cortest.bartlett(ESV[,-c(6,7)])

#- Tecnicas de retenção de fatores (Kaiser, Scree test, VSS, MAP, Análise paralela)
ESV_eig<-eigen(cor(ESV[,-c(6,7)]))
plot(ESV_eig$values,type="b")

# VSS e MAP
?VSS
ESV_vss<-
  VSS(ESV[,-c(6,7)],cor="poly")
VSS.plot(ESV_vss)

# Análise paralela
fa.parallel(ESV[,-c(6,7)],cor="poly")

# Análise fatorial 
fa(ESV[,-c(6,7)],cor="poly",fm="minrank")
# Cargas fatoriais, comunalidade, uniqueness 

# Fidedignidade: alpha e Lambda 6 Guttman
alpha(ESV[,-c(6,7)])

# Modelos multidiensionais
Big5<-read.csv("https://raw.githubusercontent.com/wagnerLM/SBP/master/big5csv",sep=";")
View(Big5)
fa.parallel(Big5[,-c(26,27)],cor="poly")
fa(Big5[,-c(26,27)],5,cor="poly",fm="minrank")

# rotação oblíqua
fa(Big5[,-c(26,27)],5,cor="poly",fm="minrank",rotate = "oblimin")

# Fidedignidade: alpha e Lambda 6 Guttman
# use o argumento "check.keys = TRUE" no caso de itens invertidos
alpha(Big5[,c(1,6,11,16,21)],check.keys = TRUE)
alpha(Big5[,c(2,7,12,17,22)])
alpha(Big5[,c(3,8,13,18,23)])
alpha(Big5[,c(4,9,14,19,24)])
alpha(Big5[,c(5,10,15,20,25)])

# Escores fatoriais
Big5_fa<-fa(Big5[,-c(26,27)],5,cor="poly",fm="minrank",rotate = "oblimin",scores = "regression")

# Permite usar o escore modelado para investigar associações com variáveis relevantes

# teste t por sexo
t.test(Big5_fa$scores[,1]~Big5$Sexo)
t.test(Big5_fa$scores[,2]~Big5$Sexo)
t.test(Big5_fa$scores[,3]~Big5$Sexo)
t.test(Big5_fa$scores[,4]~Big5$Sexo)
t.test(Big5_fa$scores[,5]~Big5$Sexo)
boxplot(Big5_fa$scores[,5]~Big5$Sexo)

# correlações com variáveis externas/critério
plot(Big5_fa$scores[,3]~Big5$Idade)
abline(lm(Big5_fa$scores[,3]~Big5$Idade),col="red")
cor(Big5_fa$scores[,3],Big5$Idade)