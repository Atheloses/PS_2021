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

data_xlsx$A_d = data_xlsx$A22-data_xlsx$A5
data_xlsx$B_d = data_xlsx$B22-data_xlsx$B5
data_xlsx$C_d = data_xlsx$C22-data_xlsx$C5
data_xlsx$D_d = data_xlsx$D22-data_xlsx$D5


data=reshape(data=as.data.frame(data_xlsx),
                  direction="long",
                  varying=list(c("A5", "B5", "C5", "D5"),
                               c("A22","B22","C22","D22"),
                               c("A_d","B_d","C_d","D_d")),
                  v.names=c("C5","C22","delta"),   
                  times=c("Amber","Bright","Clear","Dim"),  
                  timevar="vyrobce")
row.names(data) = 1:nrow(data)
data = data[-length(data)] 
data = na.omit(data)

head(data)
tail(data)

data = data %>% group_by(vyrobce) %>% mutate(delta_bezOP = delta %in% boxplot(delta, plot = FALSE)$out)
OP = data %>% filter(delta_bezOP == TRUE)
data = data %>% mutate(delta_bezOP = ifelse(delta_bezOP, NA, delta))
data %>% print(n=40)

# směr. odchylka zaokrouhluji nahoru
data %>% group_by(vyrobce) %>%
    summarise  (pocet = sum(!is.na(delta_bezOP)),
                prumer = round(mean(delta_bezOP, na.rm = TRUE),digits=5),
                smer.odch = round(sd(delta_bezOP, na.rm = TRUE),digits=5)) %>% print.data.frame()

boxplot(data$delta ~ data$vyrobce, xlab="", ylab="pokles světelného toku (lm)", main="Boxploty světelnosti")

xvalues= c(-16,15)
yvalues= c(0,22)
breaks=seq(min(data$delta_bezOP,na.rm=TRUE),max(data$delta_bezOP,na.rm=TRUE),length.out=11)

par(mfrow = c(4, 2))

data %>% group_by(vyrobce) %>% group_walk(~{
    hist(.x$delta_bezOP, xlim=xvalues, ylim=yvalues, breaks=breaks,
        xlab="Lumeny", ylab="Četnost", main=paste0('Histogram výrobce ',.y$vyrobce))
    qqnorm(.x$delta_bezOP, xlab="Teoretické kvantily", ylab="Výběrové kvantily", main=paste0('Q-Q výrobce ',.y$vyrobce)) 
    qqline(.x$delta_bezOP)
    })


normSymet = data %>% group_by(vyrobce) %>%
    summarise(  sikmost=moments::skewness(delta_bezOP, na.rm=TRUE),
                spicatost=moments::kurtosis(delta_bezOP, na.rm=TRUE)-3,
                SW=shapiro.test(delta_bezOP)$p.value,
                symetrie=lawstat::symmetry.test(delta_bezOP, boot=FALSE)$p.value)
colnames(normSymet) = c('Výrobce','Šikmost','Špičatost','Shapirův-Wilkův test (p-hodnota)','Test symetrie (p-hodnota)')
normSymet


# homoskedasticita
skeda = data %>% group_by(vyrobce) %>%
    summarise(rozptyl = sd(delta_bezOP, na.rm=TRUE)^2, #rozptyl exploračně
    ambr=var.test(x = delta_bezOP, y = data$delta_bezOP[data$vyrobce=="Amber"], ratio = 1, conf.level = 0.95)$p.value,
    br=var.test(x = delta_bezOP, y = data$delta_bezOP[data$vyrobce=="Bright"], ratio = 1, conf.level = 0.95)$p.value,
    cl=var.test(x = delta_bezOP, y = data$delta_bezOP[data$vyrobce=="Clear"], ratio = 1, conf.level = 0.95)$p.value,
    d=var.test(x = delta_bezOP, y = data$delta_bezOP[data$vyrobce=="Dim"], ratio = 1, conf.level = 0.95)$p.value,
    )
colnames(skeda) = c('Výrobce','Exploračně','Amber','Bright','Clear','Dim')
skeda

bartlett.test(data$delta_bezOP ~ data$vyrobce)$p.value

# ANOVA - nedělám, protože jsem zamítl homoskedasticita
summary(aov(data$delta_bezOP ~ data$vyrobce))

kruskal.test(data$delta_bezOP ~ data$vyrobce)$p.value
# Neexistují stat. váznamné rozdíly

# post-hoc
