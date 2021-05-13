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

# data_A = data %>% filter(vyrobce=="Amber")
# data_A = data_A[-1]

# data_B = data %>% filter(vyrobce=="Bright")
# data_B = data_B[-1]

# data_A_d = data_A$C22-data_A$C5

# data_A_DK = quantile(data_A_d, 0.25, na.rm=T)
# data_A_HK = quantile(data_A_d, 0.75, na.rm=T)
# data_A_IQR = data_A_HK - data_A_DK
# data_A_DM = data_A_DK - 1.5*data_A_IQR
# data_A_HM = data_A_HK + 1.5*data_A_IQR

# data_A_d_bezOP = data_A_d
# data_A_d_bezOP[data_A_d<=data_A_DM|data_A_d>=data_A_HM] = NA

data_A_d = data %>% filter(vyrobce=="Amber") %>% pull(delta)
data_A_d_bezOP = data_A_d
data_A_d_bezOP[data_A_d_bezOP %in% boxplot(data_A_d, plot = FALSE)$out] = NA

# data_B_d = data_B$C22-data_B$C5

# data_B_DK = quantile(data_B_d, 0.25, na.rm=T)
# data_B_HK = quantile(data_B_d, 0.75, na.rm=T)
# data_B_IQR = data_B_HK - data_B_DK
# data_B_DM = data_B_DK - 1.5*data_B_IQR
# data_B_HM = data_B_HK + 1.5*data_B_IQR

# data_B_d_bezOP = data_B_d
# data_B_d_bezOP[data_B_d<=data_B_DM|data_B_d>=data_B_HM] = NA

data_B_d = data %>% filter(vyrobce=="Bright") %>% pull(delta)
data_B_d_bezOP = data_B_d
data_B_d_bezOP[data_B_d_bezOP %in% boxplot(data_B_d, plot = FALSE)$out] = NA

sd(data_A_d_bezOP, na.rm=TRUE)
sd(data_B_d_bezOP, na.rm=TRUE)

par(mfrow = c(1, 1))   

boxplot(data_A_d, data_B_d, names=c("Amber","Bright"), ylab="pokles světelného toku (lm)", main="Boxploty světelnosti")

par(mfrow = c(2, 2))   

hist(data_A_d_bezOP,
    main="Amber",
    xlab="pokles světelného toku (lm)",
    ylab="četnost",
    xlim=c(-15,15),
    ylim=c(0,15))

qqnorm(data_A_d_bezOP,
        xlab="Teoretické kvantily",
        ylab="Výběrové kvantily",
        main="Amber")
qqline(data_A_d_bezOP)

hist(data_B_d_bezOP,
    main="Bright",
    xlab="pokles světelného toku (lm)",
    ylab="četnost",
    xlim=c(-15,15),
    ylim=c(0,15))

qqnorm(data_B_d_bezOP,
        xlab="Teoretické kvantily",
        ylab="Výběrové kvantily",
        main="Bright")
qqline(data_B_d_bezOP)


moments::skewness(data_A_d_bezOP, na.rm=T)
moments::skewness(data_B_d_bezOP, na.rm=T)

moments::kurtosis(data_A_d_bezOP, na.rm=T)-3
moments::kurtosis(data_B_d_bezOP, na.rm=T)-3

# test normality Shapirovovým - Wilkovovým testem
shapiro.test(data_A_d_bezOP)$p.value
shapiro.test(data_B_d_bezOP)$p.value

# test symetrie
lawstat::symmetry.test(data_A_d_bezOP, boot=FALSE)$p.value
lawstat::symmetry.test(data_B_d_bezOP, boot=FALSE)$p.value

mean(data_A_d_bezOP, na.rm=T)
mean(data_B_d_bezOP, na.rm=T)

t.test(data_A_d_bezOP, conf.level = 0.95, alternative = "greater")$conf.int
t.test(data_B_d_bezOP, conf.level = 0.95, alternative = "greater")$conf.int

t.test(data_A_d_bezOP, alternative="greater", conf.level=0.95)$p.value
t.test(data_B_d_bezOP, alternative="greater", conf.level=0.95)$p.value

var(data_A_d_bezOP,na.rm=T)
var(data_B_d_bezOP,na.rm=T)


prumer = mean(data_A_d_bezOP, na.rm=T) - mean(data_B_d_bezOP, na.rm=T)

# F-Test
var.test(x = data_A_d_bezOP, y = data_B_d_bezOP, ratio = 1, conf.level = 0.95, alternative = "two.sided")$p.value

# 95% oboustranný interval
t.test(x = data_A_d_bezOP, y = data_B_d_bezOP, mu = prumer, conf.level = 0.95, alternative = "two.sided")$conf.int

# Dvouvýběrový studentův T-test (p-hodnota)
t.test(x = data_A_d_bezOP, y = data_B_d_bezOP, mu = prumer, conf.level = 0.95, alternative = "two.sided")$p.value

# medián x0,1 - median()
# stř.hodnota je mean() - průměr