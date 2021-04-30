#getwd()
#setwd("./1S")
library(readxl)
library(dplyr)
library(openxlsx)
library(moments)

getwd()

# Načtení dat z xlsx souboru
data_xlsx = read_excel("./1S/ukol_123.xlsx", 
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

tail(data)

data.A = data %>% filter(vyrobce=="Amber")
data.A = data.A[-1]

sumarizace_5C = data.A %>%    
    summarise(name="      5C",
        rozsah=sum(!is.na(C5)),
        minimum=round(min(C5,na.rm=T),digits=1),   
        dolni_kvartil=round(quantile(C5,0.25,na.rm=T),digits=1),
        median=round(median(C5,na.rm=T),digits=1),
        prumer=round(mean(C5,na.rm=T),digits=1),
        horni_kvartil=round(quantile(C5,0.75,na.rm=T),digits=1),
        maximum=round(max(C5,na.rm=T),digits=1),
        #rozptyl=round(var(C5,na.rm=T),digits=1),
        smerodatna_odchylka=ceiling(sd(C5,na.rm=T)*10)/10,
        variacni_koeficient=round((100*(smerodatna_odchylka/prumer)),digits=1),  
        sikmost=round((moments::skewness(C5,na.rm=T)),digits=1),      
        spicatost=round((moments::kurtosis(C5,na.rm=T)-3),digits=1), 
        dolni_mez = round(dolni_kvartil - 1.5*(horni_kvartil - dolni_kvartil),digits=1), 
        horni_mez = round(horni_kvartil + 1.5*(horni_kvartil - dolni_kvartil),digits=1))  

            
sumarizace_22C = data.A %>% 
    summarise(name="      22C",
        rozsah=sum(!is.na(C22)),
        minimum=round(min(C22,na.rm=T),digits=1),   
        dolni_kvartil=round(quantile(C22,0.25,na.rm=T),digits=1),
        median=round(median(C22,na.rm=T),digits=1),
        prumer=round(mean(C22,na.rm=T),digits=1),
        horni_kvartil=round(quantile(C22,0.75,na.rm=T),digits=1),
        maximum=round(max(C22,na.rm=T),digits=1),
        #rozptyl=round(var(C22,na.rm=T),digits=1),
        smerodatna_odchylka=ceiling(sd(C22,na.rm=T)*10)/10,
        variacni_koeficient=round((100*(smerodatna_odchylka/prumer)),digits=1),  
        sikmost=round((moments::skewness(C22,na.rm=T)),digits=1),      
        spicatost=round((moments::kurtosis(C22,na.rm=T)-3),digits=1), 
        dolni_mez = round(dolni_kvartil - 1.5*(horni_kvartil - dolni_kvartil),digits=1), 
        horni_mez = round(horni_kvartil + 1.5*(horni_kvartil - dolni_kvartil),digits=1))  


data.A_bezOP = data.A
# nastavíme hodnoty které jsou mimo meze na NA 
data.A_bezOP$C5[data.A_bezOP$C5 >= sumarizace_5C$horni_mez | 
    data.A_bezOP$C5 <= sumarizace_5C$dolni_mez] = NA
data.A_bezOP$C22[data.A_bezOP$C22 >= sumarizace_5C$horni_mez | 
    data.A_bezOP$C22 <= sumarizace_5C$dolni_mez] = NA

sumarizace_5C_bezOP = data.A_bezOP %>%  
    summarise(name="   5C_bezOP",
        rozsah=sum(!is.na(C5)),
        minimum=round(min(C5,na.rm=T),digits=1),   
        dolni_kvartil=round(quantile(C5,0.25,na.rm=T),digits=1),
        median=round(median(C5,na.rm=T),digits=1),
        prumer=round(mean(C5,na.rm=T),digits=1),
        horni_kvartil=round(quantile(C5,0.75,na.rm=T),digits=1),
        maximum=round(max(C5,na.rm=T),digits=1),
        #rozptyl=round(var(C5,na.rm=T),digits=1),
        smerodatna_odchylka=ceiling(sd(C5,na.rm=T)*10)/10,
        variacni_koeficient=round((100*(smerodatna_odchylka/prumer)),digits=1),  
        sikmost=round((moments::skewness(C5,na.rm=T)),digits=1),      
        spicatost=round((moments::kurtosis(C5,na.rm=T)-3),digits=1), 
        dolni_mez = round(dolni_kvartil - 1.5*(horni_kvartil - dolni_kvartil),digits=1), 
        horni_mez = round(horni_kvartil + 1.5*(horni_kvartil - dolni_kvartil),digits=1))  

            
sumarizace_22C_bezOP = data.A_bezOP %>% 
    summarise(name="  22C_bezOP",
        rozsah=sum(!is.na(C22)),
        minimum=round(min(C22,na.rm=T),digits=1),   
        dolni_kvartil=round(quantile(C22,0.25,na.rm=T),digits=1),
        median=round(median(C22,na.rm=T),digits=1),
        prumer=round(mean(C22,na.rm=T),digits=1),
        horni_kvartil=round(quantile(C22,0.75,na.rm=T),digits=1),
        maximum=round(max(C22,na.rm=T),digits=1),
        #rozptyl=round(var(C22,na.rm=T),digits=1),
        smerodatna_odchylka=ceiling(sd(C22,na.rm=T)*10)/10,
        variacni_koeficient=round((100*(smerodatna_odchylka/prumer)),digits=1),  
        sikmost=round((moments::skewness(C22,na.rm=T)),digits=1),      
        spicatost=round((moments::kurtosis(C22,na.rm=T)-3),digits=1), 
        dolni_mez = round(dolni_kvartil - 1.5*(horni_kvartil - dolni_kvartil),digits=1), 
        horni_mez = round(horni_kvartil + 1.5*(horni_kvartil - dolni_kvartil),digits=1))  

sumarizace2 = rbind(sumarizace_5C,sumarizace_22C,sumarizace_5C_bezOP,sumarizace_22C_bezOP)
sumarizace <- data.frame(t(sumarizace2[-1]))
colnames(sumarizace) <- sumarizace2[, 1]
rownames(sumarizace) <- c("rozsah souboru","minimum","dolní kvartil","medián","průměr","horní kvartil","maximum","směrodatná odchylka","variační koeficient (%)","šikmost","špičatost","dolní mez","horní mez")
sumarizace

data.A_boxplot = data.A
colnames(data.A_boxplot) = c("5 stupňů","22 stupňů")
boxplot(data.A_boxplot,ylab="světelný tok (lm)",main="Boxploty světelnosti, výrobce Amber")

data_A_C5_bezOP = na.omit(data.A_bezOP$C5)
data_A_C22_bezOP = na.omit(data.A_bezOP$C22)

min_x = min(c(data_A_C5_bezOP),c(data_A_C22_bezOP))*0.99
max_x = max(c(data_A_C5_bezOP),c(data_A_C22_bezOP))*1.01
min_y = 0
max_y = max(hist(data_A_C5_bezOP,breaks=10)$counts,hist(data_A_C22_bezOP,breaks=10)$counts)*1.05

hist = hist(data_A_C5_bezOP, 
     main="Histogram při 5 stupních, výrobce Amber", 
     xlab="Lumeny",
     ylab="Četnost",
     xlim = c(min_x,max_x),
     ylim = c(min_y,max_y),
     breaks=10)   
     
hist = hist(data_A_C22_bezOP, 
     main="Histogram, světelnost při 22 stupních, výrobce Amber", 
     xlab="Lumeny",
     ylab="Četnost",
     xlim = c(min_x,max_x),
     ylim = c(min_y,max_y),
     breaks=10)   

qqnorm(data_A_C5_bezOP, 
       xlab="Teoretické kvantily",
       ylab="Výběrové kvantily",
       main="QQ-graf, světelnost při 5 stupních, výrobce Amber")
qqline(data_A_C5_bezOP)

qqnorm(data_A_C22_bezOP, 
       xlab="Teoretické kvantily",
       ylab="Výběrové kvantily",
       main="QQ-graf, světelnost při 22 stupních, výrobce Amber")
qqline(data_A_C22_bezOP)


skewness(data_A_C5_bezOP)
kurtosis(data_A_C5_bezOP) - 3 # jiná definice posunutá o 3
mu = mean(data_A_C5_bezOP)
sigma = sd(data_A_C5_bezOP)
paste0("<", round(mu - 2*sigma,digits=1), ", ", round(mu + 2*sigma,digits=1), ">")

skewness(data_A_C22_bezOP)
kurtosis(data_A_C22_bezOP) - 3 # jiná definice posunutá o 3
mu = mean(data_A_C22_bezOP)
sigma = sd(data_A_C22_bezOP)
paste0("<", round(mu - 2*sigma,digits=1), ", ", round(mu + 2*sigma,digits=1), ">")
