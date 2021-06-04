
(1-0.7*0.7)^2
0.1*0.51*0.51*0.1
1-0.9*0.9*(1-0.2601)


f = function(x){return(6*x-x^2)}
dolniMez = 0
horniMez = 6
1/integrate(f, dolniMez, horniMez)$value

x = 6
3*x^2-x^3/3
x = 0
3*x^2-x^3/3
1/36

f.dens = function(x){
    res = 1/36*(6*x-x^2) # 1/4(6*x+8)
    res[x < 0] = 0 
    res[x > 6] = 0  
    return(res)
}

F.dist = function(x){
    res = 1/36*(3*x^2-x^3/3)
    res[x < 0] = 0 
    res[x > 6] = 1  
    return(res)
}

x = seq(from = -1, to = 7, by = 0.01) 
fx = f.dens(x) 
plot(x, fx, cex = 0.2, main="Graf hustoty") 

FX = cumsum(fx)
FX = F.dist(x)
plot(x, FX, cex = 0.2, main="Distribuční funkce") 


1-F.dist(2)


data_xlsx = readxl::read_excel("./ZK1/data_123.xlsx", 
                  sheet = "Vysledky mereni",      
                  skip = 0)

data = data_xlsx[,-1]              
colnames(data) = c("prez","dist","typ")

boxplot(data$prez~data$typ)
zaokr = data %>% group_by(typ) %>%
    summarise  (pocet = sum(!is.na(prez)),
                prumer = round(mean(prez, na.rm = TRUE),0),
                smer.odch = ceiling(sd(prez, na.rm = TRUE)*100)/100)
colnames(zaokr) = c('Výrobce','Počet záznamů','Průměr','Směrodatná odchylka')
zaokr %>% print.data.frame()

normSymet = data %>% group_by(typ) %>%
    summarise(  sikmost=round(moments::skewness(prez, na.rm=TRUE),1),
                spicatost=round(moments::kurtosis(prez, na.rm=TRUE)-3,1),
                SW=round(shapiro.test(prez)$p.value,3),
                symetrie=round(lawstat::symmetry.test(prez, boot=FALSE)$p.value,3))
colnames(normSymet) = c('Výrobce','Šikmost','Špičatost','Shapirův-Wilkův test (p-hodnota)','Test symetrie (p-hodnota)')
normSymet %>% print.data.frame()

odhady = data %>% group_by(typ) %>%
    summarise(  median = round(median(prez, na.rm=TRUE),2),
                smer.odch = round(mean(prez, na.rm = TRUE)*100)/100,
                interval = paste0('(',
                    floor(wilcox.test(prez, alternative = 'greater',conf.int=T,conf.level=0.93)$conf.int[1]*10)/10, " ; ", 
                    ceiling(wilcox.test(prez, alternative = 'greater',conf.int=T,conf.level=0.93)$conf.int[2]*10)/10
                    ,')'),
                denni.ztrata = round(t.test(prez,mu=8,alternative="greater",conf.level=0.90)$p.value,3),

    )
colnames(odhady) = c('Typ','Bodový odhad (median)','Bodový odhad (str. hodnota)','93% intervalový odhad levostrany (wilcox)', 'denni ztrata')
odhady %>% print.data.frame()

wilcox.test(data$prez, alternative = 'less',conf.int=T)$conf.int


wilcox.test(data$prez[data$typ=="ekonomicky"], mu = 35, alternative = 'greater',conf.level=0.93)$p.value


wilcox.test(x = data$prez[data$typ=="technicky"], y = data$prez[data$typ=="ekonomicky"], alternative = "two.sided",
            conf.level=0.93, conf.int = TRUE)


round(kruskal.test(data$dist ~ data$typ)$p.value,3)


dunn.test::dunn.test(data$dist, data$typ, method="bonferroni",altp=TRUE)
# seřadit podle efektů


# počítání efektů
# průměry ve skupinách
efekty = data %>% group_by(typ) %>% 
    summarize(median_skup = median(dist, na.rm=T))

# efekty
efekty$efekt = efekty$median_skup - median(data$dist, na.rm=T)# celkový průměr

# vypsat setřízené
efekty %>% arrange(desc(efekt)) %>% print.data.frame()

# NEZAPOMENOUT NA TEORETICKOU ČÁST