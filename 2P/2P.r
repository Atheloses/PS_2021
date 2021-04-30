pravd.f = function(x,p){
    plot(x, p, # plna kolecka - v skutecnych hodnotach
        ylab='p(x)',xaxt='n',pch=19,ylim=c(0,max(p)),main="Pravdepodobnostni funkce") 
    lines(c(min(x)-100,max(x)+100),c(0, 0))
    for(i in 1:length(x)){
        lines(c(min(x)-100,max(x)+100), c(p[i],p[i]),
              type = 'l', lty = 3, lwd=0.5) # horizontalni grid
        lines(c(x[i],x[i]), c(-0.1,1.1), 
              type = 'l', lty = 3, lwd=0.5) # vertikalni grid
    }
    par(new=TRUE) # ze chceme kreslit do jednoho grafu
    plot(x, p*0, # prazdna kolecka - tam kde je definovana nenulova hodnota
        ylab='p(x)', xaxt='n', ylim=c(0,max(p)))
    axis(1, at=x,labels=x) # nastaveni hodnot na X
    axis(4, at=p,labels=p, las=2, cex.axis=0.7, tck=-.01) # a Y
}


dist.f = function(x,p){
    F = cumsum(p)
    F_ext = c(0, F) # natahneme F o 0 na zacatku
    x_ext = c(x[1]-1, x, x[length(x)]+1) # a x z obou stran
       
    plot(x, F, ylab="F(x)", xaxt='n', ylim=c(0,1), # prazdna kolecka
         type='p', main="Distribucni funkce") 
    par(new=TRUE) # ze chceme kreslit do jednoho grafu
    plot(x, F_ext[1:(length(F_ext)-1)], # plna kolecka
         ylab="F(x)", xaxt='n', ylim=c(0,1), type='p', pch=19) 
    
    for(i in 1:(length(x_ext)-1)){
        lines(c(min(x)-100,max(x)+100), c(F_ext[i],F_ext[i]),
              type = 'l', lty = 3, lwd=0.5) # horizontalni grid
        lines(c(x_ext[i],x_ext[i]), c(-0.1,1.1), 
              type = 'l', lty = 3, lwd=0.5) # vertikalni grid
        lines(x_ext[i:(i+1)], c(F_ext[i],F_ext[i])) # graf - cary
    }
    axis(1, at=x,labels=x) # nastaveni hodnot na X
    axis(4, at=F,labels=F, las=2, cex.axis=0.7, tck=-.01) # a Y
    #return(F)
}

souhrn=function(x,p){
  EX = sum(x*p)
  EX2 = sum(x*x*p)  
  DX = EX2-EX^2
  sigma.X = sqrt(DX)
  modus = x[match(max(p),p)]
  # zapis vysledku do tabulky
  tab = rbind(EX, DX, sigma.X, modus)
  tab.popis = c("str. hodnota","rozptyl","smer. odchylka", "modus")
  rownames(tab) = tab.popis
  colnames(tab) = ""
  return(tab)
}


# 1A
# Pouzijeme Hypergeometrickou vetu, kde vybirame pocet uspechu v n zavislych pokusech
# X ... pocet projektu bez chyb mezi 4 vybranymi
# X ~ H(N = 9, M = 6, n = 4)

x = 0:4 #pocet projektu, ktere nas zajimaji
N = 9 #celkem 9 projektu
M = 6 #celkovy pocet projektu, ktere nas zajimaji
n = 4 #pocet projektu ve vyberu

# graf pravdepodobnostni funkce pro P(X = 0 az 4)
p = dhyper(x, M, N - M, n) # hodnoty pravdepodobnostni funkce pro x
#kontrola - melo by dat 1
#sum(p) == 1
#plot(x, p)

#jednotlive pravdepodobnosti p priradime k odpovidajicim hodnotam vytyhnutemu poctu spravnych projektu
pravdep = rbind(c("P(xi)",p))
colnames(pravdep) = c("xi",0:4)
rownames(pravdep) = ""
pravdep #tabulka pravdepodobnostni funkce
pravd.f(x, p) #vykresleni

#pravdepodobnosti prubezne scitame a priradime k nim omezeni
distrib = cbind(cumsum(p),c("x<=0","0<x<=1","0<x<=1","1<x<=2","x>3"))
colnames(distrib) = c("","")
rownames(distrib) = c("","","F(x) = ","","")
distrib #predpis distribucni funkce
dist.f(x,p) #vykresleni

# 1B
# stredni hodnota je soucet vsech hodnot vynasobenych jejich pravdepodobnosti
# rozptyl pomoci E(x^2) - E(x)^2
# smerodatna odchylka je odmocnina rozptylu
# modus je hodnota nejvetsi pravdepodobnosti
# vypocet modus by bylo treba nahradit y[matchAll(max(p),p)] misto y[match(max(p),p)], 
# ale chtel jsem to mit spustitelne bez knihoven (matchAll je asi v knihovne tuple?)
souhrn(x,p)

# 1C
# vybirame alespon jeden spatny projekt, takze 1 nebo 2 nebo 3 spravne (4 spatne neexistuji)
# P(x<=3)
sum(p[1:4])

# 1D
(4 - sum(x*p)) * 200000 # celkem 4 projekty a odecteme sanci na vytazeni spravnych projektu

# 1E
# vetsi nez 250 tisic bude v pripade, ze vybereme alespon dva spatne projekty
# P(x<3)
sum(p[1:3])


# 2A
# definujeme distribucni funkci
F = c(0, 0.3, 0.5, 0.9, 1)
y = c(1,10,100,1000)

# z distribucni funkce zjistime pravdepodobnosti pro omezeni
p = diff(F)
pravd.f(y, p) #vykreslime pravdepodobnosti funkci
dist.f(y, p) #vykreslime distribucni funkci

# 2B
# P(Y=100) 
p[match(100,y)] #dohledame hodnotu pro 100
# P(Y >= 10)
1-F[match(10,y)] #hledame vetsi nez, takze musime odecist pravdepodobnost 10 od jednicky
p[match(10,y)] + p[match(100,y)] + p[match(1000,y)] # nebo by jsme mohli secist zmeny vetsich nez

# 2C
souhrn(y, p) # podobne jako 1B

# 2D
R = log10(y) # prepocteme si omezeni podle logaritmu

# mohlo by dojit k nesetrizenym omezenim, takze je treba setridit
idx_sorted = order(R)
R = R[idx_sorted]
p_R = p[idx_sorted]

souhrn(R, p_R)

# 3A
# integral od -2/3 do 0 pro f(x)dx = 1 kde f(x) = c(6x+8)
# c(3*x^2+8*x) => c(0*0)-c(3*(-2/3)^2+8*(-2/3)) => c(0) - c(-4) => c*4 dame rovno jedne => c = 0.25
f = function(x){return(6*x+8)} # f(x) = 6x+8
dolniMez = -2/3
horniMez = 0
1/integrate(f, dolniMez, horniMez)$value

# kdybych pocital rucne, tak si vyberu dva krajni body -2/3 a 0 a vypocitam jejich hodnoty
# jelikoz jde o primku tak tyto dva body bych spojil a dal do uzavreneho intervalu
# zbytek grafu hustoty bude na nule do nekonecen s otevrenymi intervaly

f.dens = function(x){
    res = 6/4*x+2 # 1/4(6*x+8)
    res[x < -2/3] = 0 
    res[x > 0] = 0  
    return(res)
}

x = seq(from = -1, to = 0.5, by = 0.01) 
FX = f.dens(x) 
plot(x, FX, cex = 0.2, main="Graf hustoty") 

# 3B
# integral od -2/3 do 0 pro F(t) = f(x)dx
#       0                   t < -2/3
# F(t)  6/8*t^2 + 2*t + 1   t <-2/3;0>
#       1                   t > 0
# pocitam integral od -2/3 do t, kde t je mensi nez 0, pro f(x)dx
# 6/4*x+2
# 6/8*x^2+2*x
# 6/8*t^2+2*t - (6/8*(-2/3)^2+2*(-2/3)) => 6/8*t^2 + 2*t + 1 => dosadim do F(t)

F.dist = function(x){
    res = 6/8*x^2 + 2*x + 1
    res[x < -2/3] = 0 
    res[x > 0] = 1  
    return(res)
}

x = seq(from = -1, to = 0.5, by = 0.01) 
FX = F.dist(x)
plot(x, FX, type = 'l', main="Distribucni funkce")

# 3C
# vypocet se provadi integraci hustoty pravdepodobnosti v limitach danych zadanim pripadne oseknutou limitami hustoty
# integrace provedena v 3B, pouze dosazujeme (6/8*(-1/3)^2 + 2*(-1/3) + 1) - (6/8*(-2/3)^2 + 2*(-2/3) + 1)
# P(-1 <= X <= -1/3)
integrate(f.dens, -2/3, -1/3)$value

# P(X > -1/3)
# integrace provedena v 3B, pouze dosazujeme (6/8*(0)^2 + 2*(0) + 1) - (6/8*(-1/3)^2 + 2*(-1/3) + 1)
integrate(f.dens, -1/3, 0)$value 


# 3D
x_fx = function(x){
    fx = f.dens(x)
    return(x*fx)
} 
xx_fx = function(x){
    fx = f.dens(x) 
    return(x*x*fx)
} 

# pouzijeme vzorce E(x) = integral x*f(x)dx na celem nenulovem intervalu
# E(x^2) = integral x^2*f(x)dx, pro D(x) = E(x^2) - E(x)^2
E_X = integrate(x_fx, -2/3, 0)$value
E_XX = integrate(xx_fx, -2/3, 0)$value

D_X = E_XX - E_X^2
std_X = sqrt(D_X)

cbind(c("E(X)","D(X)","sigma x"),c(E_X,D_X,std_X))

# 3E 
# pro Y = 4-2x vyuzijeme vzorecku
# E(aX + b) = a*E(x)+b
# D(aX + b) = a^2*D(x)
# a hodnot, ktere jsme vypocitali v 3D
E_Y = 4 - 2*E_X # E(4-2x)
D_Y = (-2)^2*D_X # D(4-2x)
std_Y = sqrt(D_Y)

cbind(c("E(Y)","D(Y)","sigma y"),c(E_Y,D_Y,std_Y))


# 4A
# hustota se pocita derivaci distribucni funkce
# zderivujeme sin x => cos x
# f(x)  cos x <0,pi/2>
#       0 (-inf,0) nebo (pi/2,inf)
f = function(x){
    res = cos(x)     # x^2+2x+1
    res[x < 0] = 0 # 0 pro x<=0
    res[x > pi/2] = 0  # 1 pro x>1
    return(res)
}

x = seq(from = -0.5, to = 2, by = 0.01) 
FX = f(x) 
plot(x, FX, cex = 0.2, main="Graf hustoty") 


# 4B 
# median nalezi do <0,pi/2> a pocita se z distribucni funkce
F.dist = function(x){
    res = sin(x)     # x^2+2x+1
    res[x <= 0] = 0 # 0 pro x<=0
    res[x > pi/2] = 1  # 1 pro x>1
    return(res)
}

x = seq(from = -0.5, to = 2, by = 0.001) 
FX = F.dist(x) 
plot(x, FX, type='l', main="Pravdepodobnostni funkce a median") 
lines(c(-0.5, 2),c(0.5, 0.5))

asin(0.5)
x[FX >= 0.5][1]

# 4C 
# podobne jako median spocitame i pravdepodobnost 60%, akorat musime pocitat s 1-0.6
x = seq(from = -0.5, to = 2, by = 0.001) 
FX = F.dist(x) 
plot(x, FX, type='l', main="Pravdepodobnostni funkce a pravdepodobnost 60%") 
lines(c(-0.5, 2),c(0.4, 0.4))


asin(0.4)
x[FX >= 0.4][1]




#BONUS
F = c(0, 0.1, 0.3, 0.6, 1)
y = c(0,1,2,3)

# z distribucni funkce zjistime pravdepodobnosti pro omezeni
p = diff(F)
pravd.f(y, p) #vykreslime pravdepodobnosti funkci
dist.f(y, p) #vykreslime distribucni funkci

# 2B
# P(Y=100) 
p[match(2,y)] #dohledame hodnotu pro 100
# P(Y >= 10)
1-F[match(1,y)] #hledame vetsi nez, takze musime odecist pravdepodobnost 10 od jednicky

# 2A
F = c(0, 0.2, 0.4, 0.7, 1)
y = c(0,1,2,3)

p = diff(F)
pravd.f(y, p)
dist.f(y, p) 

# 2B
p[match(2,y)]
1-p[match(1,y)] 

# 2C
souhrn(y, p)

# 2D
R =  y-y^2 
idx_sorted = order(R)
R = R[idx_sorted]
p_R = p[idx_sorted]

#souhrn(R, p_R)
EX = sum(R*p_R)
EX
DX = sum(R*R*p_R)-EX^2
DX
sigma = sqrt(DX)
sigma





#A
#c(4x-3/2x^2)
f = function(x){return(4-3*x)} 
a = 0
b = 2/3
1/integrate(f, a, b)$value # obsah pod primkou je 1 a pocitame kolik je c

#vyberu si dva body v intervalu abych vedel kde jde primka hustoty
#2-3/2*0
#2-(3/2)*(2/3)

f.dens = function(x){
    res = 2 - 3/2*x
    res[x < 0] = 0 
    res[x > 2/3] = 0  
    return(res)
}

x = seq(from = -0.5, to = 1, by = 0.01) 
FX = f.dens(x) 
plot(x, FX, cex=0.2, main="Hustota") 


#B
#2-3/2*x #udelam integral od 0 do 2/3 s tckama - 3 hodnoty 0, x, 1
#       0               t < 0
# F(t)  2*t - 3/4*t^2   t <0;2/3>
#       1               t > 2/3
#2 - 3/2*x
#2x - 3/4*x^2
#2t - 3/4*t^2 - (0) => dosadim do F(t)

F.dist = function(x){
    res = 2*x - 3/4*x^2     # x^2
    res[x<=0] = 0 # 0 pro x<=0
    res[x>2/3] = 1  # 1 pro x>1
    return(res)
}

x = seq(from = -0.5, to = 1, by = 0.01) 
FX = F.dist(x) 
plot(x, FX, type = 'l', main="Distribucni funkce") # vykreslit jako caru



#C
f.dens = function(x){
  res = 2-(3/2)*x
  res[x < 0] = 0
  res[x > 2/3] = 0
  return(res)
}

# P(-1/3 ≤ X ≤ 1/3)
integrate(f.dens, -1/3, 1/3)$value

# P(X >0.3)
integrate(f.dens, 1/3, 2/3)$value # tohle nebude vzdy fungovat

#D
x_fx = function(x){
    fx = f.dens(x)
    return(x*fx)
} 
xx_fx = function(x){
    fx = f.dens(x) 
    return(x*x*fx)
} 

# integrujeme jen tam kde vime, ze je f(x) nenulova
E_X = integrate(x_fx, 0, 2/3)$value
E_XX = integrate(xx_fx, 0, 2/3)$value

D_X = E_XX - E_X^2
std_X = sqrt(D_X)

cbind(c("E(X)","D(X)","sigma x"),c(E_X,D_X,std_X))

#E Y = 2-4x
E_Y = 2 - 4*E_X # E(2-4x)
D_Y = (-4)^2*D_X # D(2-4x)
std_Y = sqrt(D_Y)

cbind(c("E(Y)","D(Y)","sigma y"),c(E_Y,D_Y,std_Y))






x = 0:4 #pocet projektu, ktere nas zajimaji
N = 100000 #celkem 9 projektu
M = 20000 #celkovy pocet projektu, ktere nas zajimaji
n = 4 #pocet projektu ve vyberu

# graf pravdepodobnostni funkce pro P(X = 0 az 4)
p = dhyper(x, M, N - M, n) # hodnoty pravdepodobnostni funkce pro x
#kontrola - melo by dat 1
#sum(p) == 1
#plot(x, p)

#jednotlive pravdepodobnosti p priradime k odpovidajicim hodnotam vytyhnutemu poctu spravnych projektu
pravdep = rbind(c("P(xi)",p))
colnames(pravdep) = c("xi",0:4)
rownames(pravdep) = ""
pravdep #tabulka pravdepodobnostni funkce
pravd.f(x, p) #vykresleni

#pravdepodobnosti prubezne scitame a priradime k nim omezeni
distrib = cbind(cumsum(p),c("x<=0","0<x<=1","0<x<=1","1<x<=2","x>3"))
colnames(distrib) = c("","")
rownames(distrib) = c("","","F(x) = ","","")
distrib #predpis distribucni funkce
dist.f(x,p) #vykresleni
