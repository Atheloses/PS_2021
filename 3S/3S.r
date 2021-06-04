#getwd()
library(dplyr)

par(mfrow = c(1, 1))   

# Načtení dat z xlsx souboru
data_xlsx = readxl::read_excel("./1S/ukol_123.xlsx", 
                  sheet = "Vysledky mereni",      
                  skip = 0)        

data_xlsx = data_xlsx[,-1] # odstraníme první sloupec s indexy
colnames(data_xlsx)=c("A22","A5","B22","B5","C22","C5","D22","D5") 
head(data_xlsx)

data=reshape(data=as.data.frame(data_xlsx),
                  direction="long",
                  varying=list(c("A5", "B5", "C5", "D5"),
                               c("A22","B22","C22","D22")),
                  v.names=c("C5","C22"),   
                  times=c("Amber","Bright","Clear","Dim"),  
                  timevar="vyrobce")
row.names(data) = 1:nrow(data)
data = data[-length(data)] 
data = na.omit(data)

head(data)
tail(data)

# A) boxpolt a odstranění OP + histogramy a qq
boxplot(data$C5 ~ data$vyrobce, xlab="", ylab="světelný tok (lm)", main="Boxploty světelnosti při 5°C, podle výrobce")

data = data %>% group_by(vyrobce) %>% mutate(C5_bezOP = C5 %in% boxplot(C5, plot = FALSE)$out)
OP = data %>% filter(C5_bezOP == TRUE)
data = data %>% mutate(C5_bezOP = ifelse(C5_bezOP, NA, C5))
data %>% print(n=40)

# směr. odchylka zaokrouhluji nahoru, 3 platné cifry pro <30;2000>
# při zaokrouhlování nezapomenout přidat nuly, pokud je potřeba
zaokr = data %>% group_by(vyrobce) %>%
    summarise  (pocet = sum(!is.na(C5_bezOP)),
                prumer = round(mean(C5_bezOP, na.rm = TRUE),0),
                smer.odch = ceiling(sd(C5_bezOP, na.rm = TRUE)*10)/10) %>% print.data.frame()
colnames(zaokr) = c('Výrobce','Počet záznamů','Průměr','Směrodatná odchylka')
zaokr %>% print.data.frame()

xvalues= c(720,880)
yvalues= c(0,16)
breaks=seq(min(data$C5_bezOP,na.rm=TRUE),max(data$C5_bezOP,na.rm=TRUE),length.out=16)

par(mfrow = c(4, 2))

data %>% group_by(vyrobce) %>% group_walk(~{
    hist(.x$C5_bezOP, xlim=xvalues, ylim=yvalues, breaks=breaks,
        xlab="Lumeny", ylab="Četnost", main=paste0('Histogram světelnosti při 5°C, výrobce ',.y$vyrobce))
    qqnorm(.x$C5_bezOP, xlab="Teoretické kvantily", ylab="Výběrové kvantily", main=paste0('Q-Q graf světelnosti při 5°C (lm), výrobce ',.y$vyrobce)) 
    qqline(.x$C5_bezOP)
    })


# B) Normalita a symetrie
# šikmost a špičatost zaokrouhlovat na 1 desetinu
# p-hodnoty na o jednu větší cifru než 0.05 - tisíciny
normSymet = data %>% group_by(vyrobce) %>%
    summarise(  sikmost=round(moments::skewness(C5_bezOP, na.rm=TRUE),1),
                spicatost=round(moments::kurtosis(C5_bezOP, na.rm=TRUE)-3,1),
                SW=round(shapiro.test(C5_bezOP)$p.value,3),
                symetrie=round(lawstat::symmetry.test(C5_bezOP, boot=FALSE)$p.value,3))
colnames(normSymet) = c('Výrobce','Šikmost','Špičatost','Shapirův-Wilkův test (p-hodnota)','Test symetrie (p-hodnota)')
normSymet %>% print.data.frame()
# zamítám normalitu


# C) homoskedasticita
# Empirické posouzení
skeda = data %>% group_by(vyrobce) %>%
    summarise(  rozptyl = sd(C5_bezOP, na.rm=TRUE)^2,
                rozptyl_rounded = round(sd(C5_bezOP, na.rm=TRUE)^2,1), #rozptyl exploračně
    )

# poměr Amber/Bright 
round(max(skeda$rozptyl)/min(skeda$rozptyl),2)

colnames(skeda) = c('Výrobce','Rozptyl','Zaokrouhleno')
skeda %>% print.data.frame()

# nemám normalitu, takže beru Leven test místo Bartlet testu
car::leveneTest(data$C5_bezOP ~ data$vyrobce)
# round(bartlett.test(data$C5_bezOP ~ data$vyrobce)$p.value,3)
# zamítám homoskedasticitu


# D) bodové a 95% intervalové odhady
# zaokrouhlit interval (dolů;nahoru)
odhady = data %>% group_by(vyrobce) %>%
    summarise(  median = round(median(C5_bezOP, na.rm=TRUE),1),
                interval = paste0('(',
                    floor(t.test(C5_bezOP, alternative="two.sided",conf.level=0.95)$conf.int[1]*10)/10, " ; ", 
                    ceiling(t.test(C5_bezOP, alternative="two.sided",conf.level=0.95)$conf.int[2]*10)/10
                    ,')'),

    )
colnames(odhady) = c('Výrobce','Bodový odhad (lm)','95% intervalový odhad (lm)')
odhady %>% print.data.frame()

# oboustranný intervalový odhad, nemám normalitu, takže můžu použít mannův-whitneyho?
wilcox.test(data$C5_bezOP, alternative="two.sided",conf.level=0.95,conf.int=T)
# t.test(data$C5_bezOP[data$vyrobce=="Amber"], alternative = "two.sided", conf.level = 0.95)$conf.int
# var.test(data$C5_bezOP[data$vyrobce=="Amber"],alternative="two.sided")


# E) test významnosti a homogennost
# zamítám normalitu, takže nepoužívám ANOVA, ale Kruskalův-Wallisův
# summary(aov(data$C5_bezOP ~ data$vyrobce))

round(kruskal.test(data$C5_bezOP ~ data$vyrobce)$p.value,3)
# <<0,001 - Zamítám H_0
# Existuje statisticky významný rozdíl mezi výrobci

# install.packages("dunn.test")
# post-hoc dun-test, bonferoni
dunn.test::dunn.test(data$C5_bezOP, data$vyrobce, method="bonferroni",altp=TRUE)
# seřadit podle efektů


# počítání efektů
# průměry ve skupinách
efekty = data %>% group_by(vyrobce) %>% 
    summarize(median_skup = median(C5_bezOP, na.rm=T))

# efekty
efekty$efekt = efekty$median_skup - median(data$C5_bezOP, na.rm=T)# celkový průměr

# vypsat setřízené
efekty %>% arrange(desc(efekt)) %>% print.data.frame()


# B   a   
# A   x   b
# C   x   b   c
# D   x   b   c   d

# a(Bright) - mezi 'a' a 'b' je statistiky významný rozdíl v mediánu světelného toku při 5*C
# b(Amber,Clear,Dim) - není statistiky vyz. roz. v medianu
