# spočítání pravděpodobnosti P(A) - věta o úplné pravděpodobnosti
uplna_pravdepodobnost = function(P_B, P_AB)
{   # uvažujeme P_B jako vektor hodnot P(B_i) a P_BA jako vektor hodnot P(A|B_i)
    P_A = 0
    for (i in 1:length(P_B))
    {
        P_A = P_A + P_B[i]*P_AB[i]
    }
    return(P_A)
}

# * Bayesova věta ####
# $P(B_k|A)=\frac{P(B_k)P(A|B_k)}{\sum_{i=1}^{n}P(B_i)P(A|B_i)}$


# spočítání podmíněné pravděpodobnosti P(B_k|A) - Bayesova věta
bayes = function(P_B, P_AB, k)
{   # uvažujeme P_B jako vektor hodnot P(B_i), P_BA jako vektor hodnot P(A|B_i)
    P_A = uplna_pravdepodobnost(P_B, P_AB)
    P_BkA = P_B[k]*P_AB[k]/P_A
    return(P_BkA)
} 

souhrn=function(x,p){
  EX = sum(x*p)
  EX2 = sum(x*x*p)  
  DX = EX2-EX^2
  sigma.X = sqrt(DX)
  # zápis výsledků do tabulky
  tab = rbind(EX, DX, sigma.X)
  tab.popis = c("str. hodnota","rozptyl","smer. odchylka")
  rownames(tab) = tab.popis
  return(tab)
}



# 1A
#       A   B   C
# spok  70  85  55
# nesp  30  15  45
# clk   30  38  32

0.3*0.3+0.15*0.38+0.45*0.32

# 1B
p = c(0.7,0.85,0.55)
c = c(0.3,0.38,0.32)
bayes(c, p, 2)

# 2A
xvalue = c(-2,1,3)
yvalue = c(0,1)
zero = c(1/3,1/6,0)
one = c(1/6,1/6,1/6)
sum(zero,one)

# 2B marginální pravděpodobnostní a distribuční funkce
# x -2  1   3
# p 3/6 2/6 1/6
x = c(zero[1]+one[1],zero[2]+one[2],zero[3]+one[3])
x
y = c(zero[1]+zero[2]+zero[3],one[1]+one[2]+one[3])
y

# 2C
# stř. hodnota = 
sum(xvalue*x)
sum(yvalue*y)

# směr. odchylka
sqrt(sum(xvalue*xvalue*x)-sum(xvalue*x)^2)
sqrt(sum(yvalue*yvalue*y)-sum(yvalue*y)^2)


souhrn(xvalue,x)
souhrn(yvalue,y)

# 2D sdružená dist. funkce
# F(0,5;1) = 1/3

# 3A pravděpodobnostní funkce

x = seq(from = -1, to = 150, by = 1) # body na ose x
FX = dexp(x,1/45) 
plot(x, FX, type='l', main="Distribucni funkce") 

# 3B P(x > 68) = 1-P(x<68)
1-pexp(68,1/45)

# 3C z 30 baterií bude alespoň polovina s výdrží nad 68 měsíců
n = 30
p = 1-pexp(68,1/45)
1-pbinom(15,n,p)

# 3D 40 náhodně vybraných baterií bude průměrna výdrž větší než 43 měsíců

n = 40
sigma = 45
x = n*43
mi = n*sigma
rozptyl = n*sigma**2
1 - pnorm(x,mi,sqrt(rozptyl))
