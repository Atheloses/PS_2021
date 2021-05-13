# načtení balíčků
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(moments)
library(lawstat)


# načtení dat ze souboru
data = read_excel("./1S/ukol_123.xlsx",
                  sheet = "Vysledky mereni",           # specifikace listu v xlsx souboru
                  skip = 0)                            # počet přeskočených řádků
data = data[,-1]                                       # odstraníme první sloupec s indexy
colnames(data)=c("A22","A5","B22","B5","C22","C5","D22","D5") 
#head(data)
#tail(data)


data22 = data[,c(1,3,5,7)]
colnames(data22) = c("A","B","C","D")

data22S = stack(data22)
colnames(data22S) = c("tok_22","vyrobce")
#tail(data22S)

data5 = data[,c(2,4,6,8)]
colnames(data5) = c("A","B","C","D")

data5S = stack(data5)
colnames(data5S) = c("tok_5","vyrobce")

dataS = cbind(data22S, data5S)

dataS = dataS[,-2] # vynecháme nadbytečný druhý sloupec
dataS = na.omit(dataS) # vynecháme řádky s NA hodnotami

#dataS = na.omit(dataS) # vynecháme řádky s NA hodnotami
#head(dataS)


res = dataS %>% group_by(vyrobce) %>% summarise(
    rozsah = length(tok_5),
    minimum = min(tok_5, na.rm=T),
    Q1 = quantile(tok_5, 0.25, na.rm=T),
    prumer = mean(tok_5),
    median = median(tok_5),
    Q3 = quantile(tok_5, 0.75, na.rm=T),
    maximum = max(tok_5, na.rm=T),
    smerodatna_odchylka = sd(tok_5, na.rm = T),
    variacni_koeficient = (100*(smerodatna_odchylka / prumer)),  # variační koeficient v procentech
    sikmost = (moments::skewness(tok_5, na.rm = T)),       # preventivní specifikace balíčku moments
    spicatost = (moments::kurtosis(tok_5, na.rm = T)-3),
    IQR = Q3 - Q1,
    dolni_mez = Q1 - 1.5*IQR,
    horni_mez = Q3 + 1.5*IQR
    
) 
t(res)

res = dataS %>% group_by(vyrobce) %>% summarise(
    rozsah = length(tok_22),
    minimum = min(tok_22, na.rm=T),
    Q1 = quantile(tok_22, 0.25, na.rm=T),
    prumer = mean(tok_22),
    median = median(tok_22),
    Q3 = quantile(tok_22, 0.75, na.rm=T),
    maximum = max(tok_22, na.rm=T),
    smerodatna_odchylka = sd(tok_22, na.rm = T),
    variacni_koeficient = (100*(smerodatna_odchylka / prumer)),  # variační koeficient v procentech
    sikmost = (moments::skewness(tok_22, na.rm = T)),       # preventivní specifikace balíčku moments
    spicatost = (moments::kurtosis(tok_22, na.rm = T)-3),
    IQR = Q3 - Q1,
    dolni_mez = Q1 - 1.5*IQR,
    horni_mez = Q3 + 1.5*IQR
) 
t(res)

A_tok_22 = dataS$tok_22[dataS$vyrobce == "A"]

dolni_kvartil = quantile(A_tok_22, 0.25, na.rm=T)
horni_kvartil = quantile(A_tok_22,0.75,na.rm=T)
IQR = horni_kvartil - dolni_kvartil  # mezikvartilové rozpěti
dolni_mez_22 = dolni_kvartil - 1.5*IQR  # výpočet dolní mezi vnitřních hradeb
horni_mez_22 = horni_kvartil + 1.5*IQR  # výpočet horní mezi vnitřních hradeb

A_tok_22_R = A_tok_22
A_tok_22_R[A_tok_22>=horni_mez_22 | A_tok_22<=dolni_mez_22] = NA # nastavíme hodnoty které jsou mimo meze na NA 
A_tok_22_R

A_tok_5 = dataS$tok_5[dataS$vyrobce == "A"]

dolni_kvartil = quantile(A_tok_5, 0.25, na.rm=T)
horni_kvartil = quantile(A_tok_5,0.75,na.rm=T)
IQR = horni_kvartil - dolni_kvartil  # mezikvartilové rozpěti
dolni_mez_5 = dolni_kvartil - 1.5*IQR  # výpočet dolní mezi vnitřních hradeb
horni_mez_5 = horni_kvartil + 1.5*IQR  # výpočet horní mezi vnitřních hradeb

A_tok_5_R = A_tok_5
A_tok_5_R[A_tok_5>=horni_mez_5 | A_tok_5<=dolni_mez_5] = NA # nastavíme hodnoty které jsou mimo meze na NA 
A_tok_5_R

filtered = dataS %>% 
    filter(vyrobce=="A") %>%
    mutate(tok_22= ifelse(tok_22>=horni_mez_22 | tok_22 <= dolni_mez_22, NA, tok_22)) %>%
    mutate(tok_5= ifelse(tok_5 >= horni_mez_5 | tok_5 <= dolni_mez_5, NA, tok_5))
filtered
filtered = na.omit(filtered) # vynecháme řádky s NA hodnotami

res = filtered %>% group_by(vyrobce) %>% summarise(
    rozsah = length(tok_5),
    minimum = min(tok_5, na.rm=T),
    Q1 = quantile(tok_5, 0.25, na.rm=T),
    prumer = mean(tok_5),
    median = median(tok_5),
    Q3 = quantile(tok_5, 0.75, na.rm=T),
    maximum = max(tok_5, na.rm=T),
    smerodatna_odchylka = sd(tok_5, na.rm = T),
    variacni_koeficient = (100*(smerodatna_odchylka / prumer)),  # variační koeficient v procentech
    sikmost = (moments::skewness(tok_5, na.rm = T)),       # preventivní specifikace balíčku moments
    spicatost = (moments::kurtosis(tok_5, na.rm = T)-3),
    IQR = Q3 - Q1,
    dolni_mez = Q1 - 1.5*IQR,
    horni_mez = Q3 + 1.5*IQR
    
) 
t(res)

res = filtered %>% group_by(vyrobce) %>% summarise(
    rozsah = length(tok_22),
    minimum = min(tok_22, na.rm=T),
    Q1 = quantile(tok_22, 0.25, na.rm=T),
    prumer = mean(tok_22),
    median = median(tok_22),
    Q3 = quantile(tok_22, 0.75, na.rm=T),
    maximum = max(tok_22, na.rm=T),
    smerodatna_odchylka = sd(tok_22, na.rm = T),
    variacni_koeficient = (100*(smerodatna_odchylka / prumer)),  # variační koeficient v procentech
    sikmost = (moments::skewness(tok_22, na.rm = T)),       # preventivní specifikace balíčku moments
    spicatost = (moments::kurtosis(tok_22, na.rm = T)-3),
    IQR = Q3 - Q1,
    dolni_mez = Q1 - 1.5*IQR,
    horni_mez = Q3 + 1.5*IQR
) 
t(res)

options(repr.plot.width=6, repr.plot.height=6)
boxplot(A_tok_5, A_tok_22,
        names=c("5°C","22°C"),
        #main="Světelný tok po 30 vteřinách po zapnutí", 
        xlab="Teplota",
        ylab="světelný tok (lm)",
        col="grey")

        options(repr.plot.width=8, repr.plot.height=6)
attach(mtcars)
par(mfrow=c(2,2))


hist(filtered$tok_5, 
     main="5°C", 
     xlab="světelný tok (lm)",
     ylab="četnost",
     
     xlim=c(650,950),
     ylim=c(0,20),
     breaks=12,
     
     col="grey30", 
     border="white",
     freq=TRUE, labels=FALSE
    )

qqnorm(filtered$tok_5, 
       xlab="Teoretické kvantily",
       ylab="Výběrové kvantily",
       main="5°C")
qqline(filtered$tok_5)


hist(filtered$tok_22, 
     main="22°C", 
     xlab="světelný tok (lm)",
     ylab="četnost",
     
     xlim=c(650,950),
     ylim=c(0,20),
     breaks=12,
     #breaks=c(700, 725, 750, 775, 800, 825, 850, 875, 900),
     
     col="grey30", 
     border="white",
     freq=TRUE,labels=FALSE
    )     

qqnorm(filtered$tok_22, 
       xlab="Teoretické kvantily",
       ylab="Výběrové kvantily",
       main="22°C")
qqline(filtered$tok_22)

#Ukol 2
# Pokles toku Amber
A_tok_22 = dataS$tok_22[dataS$vyrobce == "A"]
A_tok_5 = dataS$tok_5[dataS$vyrobce == "A"]
A_tok_d = A_tok_22 - A_tok_5

# Pokles toku Bright
B_tok_22 = dataS$tok_22[dataS$vyrobce == "B"]
B_tok_5 = dataS$tok_5[dataS$vyrobce == "B"]
B_tok_d = B_tok_22 - B_tok_5


dolni_kvartil = quantile(A_tok_d, 0.25, na.rm=T)
horni_kvartil = quantile(A_tok_d ,0.75, na.rm=T)
IQR = horni_kvartil - dolni_kvartil  # mezikvartilové rozpěti
dolni_mez_d = dolni_kvartil - 1.5*IQR  # výpočet dolní mezi vnitřních hradeb
horni_mez_d = horni_kvartil + 1.5*IQR  # výpočet horní mezi vnitřních hradeb

A_tok_d_R = A_tok_d
A_tok_d_R[A_tok_d >= horni_mez_d | A_tok_d <= dolni_mez_d] = NA # nastavíme hodnoty které jsou mimo meze na NA 
#A_tok_5_R

dolni_kvartil = quantile(B_tok_d, 0.25, na.rm=T)
horni_kvartil = quantile(B_tok_d ,0.75, na.rm=T)
IQR = horni_kvartil - dolni_kvartil  # mezikvartilové rozpěti
dolni_mez_d = dolni_kvartil - 1.5*IQR  # výpočet dolní mezi vnitřních hradeb
horni_mez_d = horni_kvartil + 1.5*IQR  # výpočet horní mezi vnitřních hradeb

B_tok_d_R = B_tok_d
B_tok_d_R[B_tok_d >= horni_mez_d | B_tok_d <= dolni_mez_d] = NA # nastavíme hodnoty které jsou mimo meze na NA 
#B_tok_d_R

A_tok_d_R = na.omit(A_tok_d_R)
B_tok_d_R = na.omit(B_tok_d_R)

options(repr.plot.width=6, repr.plot.height=6)
#attach(mtcars)
#par(mfrow=c(1, 2))

#boxplot
boxplot(A_tok_d, B_tok_d,
        names=c("Amber","Bright"),
        #main="Světelný tok po 30 vteřinách po zapnutí", 
        #xlab="Teplota",
        ylab="pokles světelného toku (lm)",
        col="grey")

options(repr.plot.width=8, repr.plot.height=6)
attach(mtcars)
#par(mfrow=c(2, 2))
#layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), widths=c(3,1), heights=c(1,2))
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), widths=c(4,2), heights=c(2,2))

hist(A_tok_d_R, 
     main="Amber", 
     xlab="pokles světelného toku (lm)",
     ylab="četnost",
     
     xlim=c(-6,8),
     ylim=c(0,15),
     #breaks=12,
     
     col="grey30", 
     border="white",
     freq=TRUE, labels=FALSE
    )

qqnorm(A_tok_d_R, 
       xlab="Teoretické kvantily",
       ylab="Výběrové kvantily",
       main="Amber")
qqline(A_tok_d_R)

hist(B_tok_d_R, 
     main="Bright", 
     xlab="pokles světelného toku (lm)",
     ylab="četnost",
     
     xlim=c(-6,8),
     ylim=c(0,15),
     #breaks=12,
     
     col="grey30", 
     border="white",
     freq=TRUE, labels=FALSE
    )

qqnorm(B_tok_d_R, 
       xlab="Teoretické kvantily",
       ylab="Výběrové kvantily",
       main="Bright")
qqline(B_tok_d_R)

#směrodatná odchylka
sd(A_tok_d_R)
sd(B_tok_d_R)

#šikmost
moments::skewness(A_tok_d_R)
moments::skewness(B_tok_d_R)

#špičatost
#moments::kurtosis(A_tok_d_R)
#moments::kurtosis(B_tok_d_R)
moments::kurtosis(A_tok_d_R, na.rm = T)-3
moments::kurtosis(B_tok_d_R, na.rm = T)-3
#SW test
shapiro.test(A_tok_d_R)$p.value
shapiro.test(B_tok_d_R)$p.value


lawstat::symmetry.test(A_tok_d_R, boot=FALSE)
lawstat::symmetry.test(B_tok_d_R, boot=FALSE)

"prumer"
mean(A_tok_d_R)
mean(B_tok_d_R)


wilcox.test(A_tok_d_R, alternative="greater", conf.level=0.95, conf.int=T)
wilcox.test(B_tok_d_R, alternative="greater", conf.level=0.95, conf.int=T)

mean(B_tok_d_R) - mean(A_tok_d_R)

#mannův whitneho
wilcox.test(A_tok_d_R, B_tok_d_R,alternative="greater",conf.level=0.95,conf.int=T)
wilcox.test(B_tok_d_R, A_tok_d_R, mu=0, alternative="two.sided",conf.level=0.95,conf.int=T)

