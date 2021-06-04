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

svetTok = 0.8 * 1000
data_xlsx$A_min = data_xlsx$A5 >= svetTok
data_xlsx$B_min = data_xlsx$B5 >= svetTok
data_xlsx$C_min = data_xlsx$C5 >= svetTok
data_xlsx$D_min = data_xlsx$D5 >= svetTok

data=reshape(data=as.data.frame(data_xlsx),
                  direction="long",
                  varying=list(c("A5", "B5", "C5", "D5"),
                               c("A22","B22","C22","D22"),
                               c("A_min","B_min","C_min","D_min")),
                  v.names=c("C5","C22","min"),   
                  times=c("Amber","Bright","Clear","Dim"),  
                  timevar="vyrobce")
row.names(data) = 1:nrow(data)
data = data[-length(data)] 
data = na.omit(data)

head(data)
tail(data)


# A) kontingenční tabulka a mozaikový graf
data.tab.abs = table(data$vyrobce, data$min)

colnames(data.tab.abs) = c("pod 80 %","nad 80 %")
data.tab.abs

# Explorační analýza
# prop.table(data.tab.abs)    # sdružené relativní četnosti
data.tab.rel = prop.table(data.tab.abs,1)  # řádkové relativní četnosti
# prop.table(data.tab.abs,2)  # sloupcové relativní četnosti

data.tab.celkem = data %>% group_by(vyrobce) %>% summarise( pocet_abs=sum(!is.na(min)))
data.tab.celkem$pocet_rel = prop.table(data.tab.celkem$pocet_abs)

abs.sum.ano = sum(as.numeric(data.tab.abs[,1])) 
abs.sum.ne = sum(as.numeric(data.tab.abs[,2]))
rel.sum.ano = sum(as.numeric(prop.table(data.tab.abs)[,1])) 
rel.sum.ne = sum(as.numeric(prop.table(data.tab.abs)[,2]))

data.tab = data.frame(  nad_abs = as.numeric(data.tab.abs[,2]), nad_rel = as.numeric(data.tab.rel[,2]), nad_rel_r = paste0(round(as.numeric(data.tab.rel[,2])*100,2),' %'), 
                        pod_abs = as.numeric(data.tab.abs[,1]), pod_rel = as.numeric(data.tab.rel[,1]), pod_rel_r = paste0(round(as.numeric(data.tab.rel[,1])*100,2),' %'),
                        celkem_abs = data.tab.celkem$pocet_abs, celkem_rel = data.tab.celkem$pocet_rel, celkem_rel_r = paste0(round(data.tab.celkem$pocet_rel*100,2),' %'))
rownames(data.tab) = rownames(data.tab.rel)

data.tab.presentation = data.tab
data.tab.presentation[nrow(data.tab)+1,] = c(   
                            abs.sum.ne, rel.sum.ne, paste0(round((rel.sum.ne)*100,2),' %'),
                            abs.sum.ano, rel.sum.ano, paste0(round((rel.sum.ano)*100,2),' %'),
                            abs.sum.ano + abs.sum.ne, rel.sum.ano + rel.sum.ne, paste0(round((rel.sum.ano + rel.sum.ne)*100,2),' %'))
rownames(data.tab.presentation) = c(rownames(data.tab),'Celkem')
data.tab.presentation[,c(1,3,4,6,7,9)]
# paste0 osekává nuly, pozor

mosaicplot(data.tab.abs,
           las = 1,
           color = gray.colors(2),
           main = "Světelnost po 30 s při 5°C")

# crammerovo V závislost TODO
# install.packages("lsr")
round(lsr::cramersV(data.tab.rel),3)
# slabá závislost

# B) Bodový a intervalový odhad
# předpoklad pro cloper-pearsnův
bri_ind = match("Bright",rownames(data.tab))
predpoklad = round(9/(as.numeric(data.tab$pod_rel)[bri_ind]*(1-as.numeric(data.tab$pod_rel)[bri_ind])),2)

paste0('$9/(',round(as.numeric(data.tab$pod_rel)[bri_ind],4),'*',round(1-as.numeric(data.tab$pod_rel)[bri_ind],4),')$')

if(predpoklad < as.numeric(data.tab$celkem_abs)[bri_ind]){
    paste0('předpoklad pro cloper-pearsnuv test je splněn s hodnotou ', predpoklad,' < ',as.numeric(data.tab$celkem_abs)[bri_ind])
}else{
    paste0('předpoklad pro cloper-pearsnuv test neni splněn s hodnotou ', predpoklad,' > ',as.numeric(data.tab$celkem_abs)[bri_ind])
}

int.odhad = binom.test(data.tab$pod_abs[bri_ind], n=data.tab$celkem_abs[bri_ind])$conf.int
paste0('bodový odhad: ',data.tab$pod_abs[bri_ind], ' intervalový odhad: (',floor(int.odhad[1]*10000)/100,';',ceiling(int.odhad[2]*10000)/100,'), pomocí binom.test')
# TODO, který test?
int.odhad2 = prop.test(data.tab$pod_abs[bri_ind], n=data.tab$celkem_abs[bri_ind])$conf.int
paste0('bodový odhad: ',data.tab$pod_abs[bri_ind], ' intervalový odhad: (',floor(int.odhad2[1]*10000)/100,';',ceiling(int.odhad2[2]*10000)/100,'), pomocí prop.test')


# C) relativní bodový a intervalový odhad mezi nejlepším a nejhorším
nejlepsi_ind = match(min(data.tab$pod_rel),data.tab$pod_rel)
nejhorsi_ind = match(max(data.tab$pod_rel),data.tab$pod_rel)

# install.packages("epiR")
pom = epiR::epi.2by2(data.tab.abs[c(nejhorsi_ind,nejlepsi_ind),])

paste0('nejlepsi vyrobce: ', rownames(data.tab)[nejlepsi_ind],', nejhorsi vyrobce: ',
    rownames(data.tab)[nejhorsi_ind],', ', round(pom$massoc$RR.strata.wald[1],2) , 'x lepsi') #max(data.tab$pod_rel)/min(data.tab$pod_rel)
paste0('intervalový odhad (',floor(pom$massoc$RR.strata.wald[2]*100)/100,';',ceiling(pom$massoc$RR.strata.wald[3]*100)/100,')')


# D) poměr šancí, bodový a intervalový odhad
nejlepsi_sance = data.tab$pod_abs[nejlepsi_ind]/data.tab$nad_abs[nejlepsi_ind]*1000
nejhorsi_sance = data.tab$pod_abs[nejhorsi_ind]/data.tab$nad_abs[nejhorsi_ind]*1000
paste0('šance, že nejlepší nedosáhne 80 % je ',round(nejlepsi_sance),':1000')
paste0('šance, že nejhorší nedosáhne 80 % je ',round(nejhorsi_sance),':1000')
sance = pom$massoc$OR.strata.wald
paste0('šance na nedosahnutí světelnosti je ',round(sance[1],2),'x vyšší') #nejhorsi_sance/nejlepsi_sance
paste0('95% intervalový odhad je (',floor(sance[2]*100)/100,';',ceiling(sance[3]*100)/100,')')


# E) chikvadrát
# předpoklad
pom = chisq.test(data.tab.abs)
min(as.numeric(pom$expected)>5) == 1
# všechny četnosti jsou větší než 5, splněno
round(pom$p.value,3) # < 0,05, zamítáme H_0 -> 
# existuje statisticky významná závislost mezi výrobcem a ...

