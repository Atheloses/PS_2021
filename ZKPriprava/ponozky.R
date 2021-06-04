################################# zkouska 1 #######################################

####### 1)
p.k=0.3
p.o=0.2
p.p=0.5

p.zk=0.05
p.zo=0.5
p.zp=0.05

p=(p.o*p.zo)/(p.o*p.zo+p.zk*p.k+p.zp*p.p)
p

###### 2)
# a)
#exponencialni
lambda=3
x=seq(0,9,0.01)
y=pexp(x,lambda)
plot(x,y)
#pexp(4,2)
# b)
#P(X>0.5)=1-P(X<=0.5)
1-pexp(0.5,3)
# c)
#P(X>t)=0.98 -> P(X<=t)=0.02 -> F(t)=0.02
qexp(0.02,3)*3600
qexp(0.9,2)
####### 3)

# a)
x=c(0,1,2)
p=c(0.25,0.5,0.25)

#b)
EX=sum(x*p)
EX
DX=sum(x*x*p)-EX^2
sqrt(DX)
#c) 1-P(X=0)
1-p[1]

###### 4
library(openxlsx)
ponozky = readWorkbook("./ZKPriprava/ponozky.xlsx",
                        sheet=1,
                        colNames=TRUE,
                        startRow = 1)
ponozky.stack=stack(ponozky)
boxplot(ponozky.stack$values~ponozky.stack$ind)
# a)
pom=boxplot(ponozky$kotnikove)
#odstranime op
ponozky$kotnikove[ponozky$kotnikove %in% pom$out]=NaN
shapiro.test(ponozky$kotnikove)
#normalita ok t-test IO
t.test(ponozky$kotnikove)

# b)
# m�s��ne 120*2 pono�ek denn� 120*2/30=8
#H0: mu=8
#Ha: mu~=8 pr�padne mu>8
t.test(ponozky$kotnikove,mu=8)
t.test(ponozky$kotnikove,mu=8,alternative="greater")

# c)
spatny=ponozky$kotnikove>=10
tab=table(spatny)
p=tab[2]/sum(tab)
p
9/(p*(1-p))
sum(tab)
# vysledky budou nespolehlive
binom.test(tab[2],sum(tab),conf.level=0.95)

######### 5
#op
# uz jsou bez OP (jedine odstraneno v minule uloze)
ponozky.stack=stack(ponozky)
#a)
boxplot(ponozky.stack$values~ponozky.stack$ind)
#b)
# test normality
tapply(ponozky.stack$values,ponozky.stack$ind,shapiro.test)
# test homoskedasticity
bartlett.test(ponozky.stack$values,ponozky.stack$ind)
# ANOVA
res=aov(ponozky.stack$values~ponozky.stack$ind)
summary(res)
# post hoc
TukeyHSD(res)
library(DescTools)
install.packages("DescTools")

PostHocTest(res)
posthocPairwiseT(res)
#efekty
mean(ponozky.stack$values,na.rm = TRUE)-tapply(ponozky.stack$values,ponozky.stack$ind,mean,na.rm = TRUE)


################################# zkouska 2 #######################################
########## 1
p.S=0.2
p.V=0.55
p.G=1-(p.S+p.V)

p.znVV=0.95
p.znVS=0.025
p.znVG=0.025

p.VznV=(p.V*p.znVV)/(p.V*p.znVV+p.znVS*p.S +p.znVG*p.G)
p.VznV

######## 2

#a)
#P(X>30)=1-F(30)
1-pnorm(30,30,3)
#b)
0
#c)
#P(X<35)=F(35)
pnorm(35,30,3)
#d)
#P(X>M)=0.11 -> F(M)=0.89
qnorm(0.89,30,3)
######### 3
#a)
#hypergem
N=64+28
M=64
n=4
#b)
x=0:4
P=dhyper(x,M,N-M,n)
P
plot(x,P)
?dhyper()
# c)
P[5]

#d)
EX=sum(x*P)
EX

############# 4
library(openxlsx)
kose = readWorkbook("zkouska1902.xlsx",
                       sheet=1,
                       colNames=TRUE,
                       startRow = 1)
# a)
#odstranime op
kose_rozdil=kose$vlhk�-kose$such�
pom=boxplot(kose_rozdil)
kose_rozdil[kose_rozdil %in% pom$out]=NaN
boxplot(kose_rozdil)

shapiro.test(kose_rozdil)
t.test(kose_rozdil)

# b)
#vyssi, tedy tento rozdil>0
#H0: mu=0 ; HA: mu>0
t.test(kose_rozdil,alternative="greater", conf.level = 0.9)

# c)
pom=boxplot(kose$such�)
kose$such�[kose$such� %in% pom$out]=NaN
boxplot(kose$such�)
vyssi60=kose$such�>60
tab=table(vyssi60)
tab
p=tab[2]/sum(tab)
p
9/(p*(1-p))
sum(tab)

binom.test(tab[2],sum(tab))

############ 5

tab=matrix(c(64-12,12,28-10,10),nrow = 2,ncol = 2)
tab
colnames(tab)=c("LILJA","VIOLETA")
rownames(tab)=c("OK","KO")
tab
#asocia�n� tabulka m� p�edepsan� tvar exposed=neobl�ben�(VIOLETTA), outcome poskozene(KO)
#je t�eba transponovat
tab=t(tab)
tab
#je t�eba prohodit ��dky
tab=tab[c(2,1),]
tab
#je t�eba prohodit sloupce
tab=tab[,c(2,1)]
tab

#b)
mosaicplot(tab)
library(lsr)
cramersV(tab)
# c)
res=chisq.test(tab)
res$expected
res

#d)
library(epiR)
epi.2by2(tab)
