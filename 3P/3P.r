# 1A
# X... pocet stad na 1km^2
# X - pouzijeme Poissonovu vetu
lambda = 1/25
t = 100
lt = lambda*t # parametr Poissonova rozdeleni

# P(x <= 6)
# V oblasti 100km^2 bude nejvyse 6 stad
ppois(6, lt) # s pravdepodobnosti 88,93 %


# 1B
# P(1<=X<=7) = P(X <= 7) - P(X <= 0)
# V oblasti 100km^2 bude alespon 1 ale nejvyse 7 stad
ppois(7, lt) - ppois(0, lt) # s pravdepodobnosti 93,06 %


# 1C
# V oblasti 100km^2 bude alespon 5 stad, jestlize 2 stada byla nalezena
(1-ppois(4,lt)) / (1-ppois(1,lt)) # 0.4085801



# 1D
lambda = 1/25 
t = 50 
# P(X >= 1) = P(X > 0) = 1 - P(X <= 0)
# V oblasti 50km^2 bude alespon jedno stado
1 - ppois(0, lambda*t) # s pravdepodobnosti 86,47 %



# 2A
# X... pocet opravenych domacich ukolu za 1h
# X - pouzijeme Poissonovu vetu
lambda = 6 # cetnost vyskytu za hodinu
t = 8 # behem 8mi hodin
lt = lambda*t # parametr Poissonova rozdeleni

# P(x > 50) = 1 - P(X <= 49)
# Opravi vice jak 50 ukolu
1 - ppois(50, lt) # s 35,13% pravdepodobnosti

#Pravd fun
x = 20:80 
P_x = dpois(x-1, lt)
plot(x, P_x)
grid()

#Dist fun
x = 20:80 
F_x = ppois(x-1, lt)
plot(x, F_x, type='s')
grid()


# 2B
# Opravi 45 az 55 ukolu
ppois(55, lt) - ppois(45-1, lt) # s 54,67% sanci


# 2C
# Opravi 55 ukolu jestlize 40 opravil
(1 - ppois(54, lt)) / (1 - ppois(39, lt)) # s pravdepodobnosti 19,4 %


# 2D
# X ... pocet pokusu nez vybereme spatny ukol
# X ~ NB(k = 3,p = 0.2)
x = 17
k = 3
p = 0.2
# P(X > 15) = 1 - P(X <= 15)
# Narazi na tri spatne ukoly nejdrive za 17 pokusu
1-pnbinom(x - k, k, p) # s pravdepodobnosti 30,96 %



# 3A
# X ... doba do restartu ve dnech
## X ~ Exp(lambda), kde E(X)=1/lambda
lambda = 1/365

x = seq(from = 0, to = 2500, by = 1)
f_x = dexp(x-1, lambda)
plot(x, f_x, cex = 0.1, main="Hustota pravdepodobnosti po dnech")
grid()

F_x = pexp(x-1, lambda)
plot(x, F_x, type='s', main="Distribucni funkce po dnech")
grid()


# 3B
# Restart bude potreba nejdrive za 13 mesicu
1 - pexp(365/12*13, lambda) # s pravdepodobnosti 33,85 %
x = seq(from = 0, to = 2500, by = 1)
f_x = dexp(x-1, lambda)
plot(x, f_x, cex = 0.1, main="Hustota pravdepodobnosti a restart po 13 mesicich")
lines(c(0, 2500),c(dexp(365/12*13, lambda), dexp(365/12*13, lambda)))
lines(c(365/12*13, 365/12*13),c(0, max(f_x)))
grid()


# 3C
# Pravdepodobnost restartu po roce pouzivani v nasledujicich 14 dnech
pexp(365 + 365/52*2, lambda) - pexp(365, lambda) # je 1,38 %.


# 3D
# P(X<t)=0,9 -> F(t)=0,9 -> t… 90% kvantil
# Server bude treba restartovat s 90 % pravdepodobnosti
qexp(0.9, lambda) # po 840,4 dnech.



# 4A
# Systolicky krevni tlak, normalni rozdeleni se stredni hodnotou 112 a smerodatnou odchylkou 10
mu = 112
sigma = 10

# vykreslime si Hustotu pravdepodobnosti
x = seq(from = 70, to = 150, by = 0.1)
f_x = dnorm(x, mean=mu, sd=sigma)
plot(x, f_x, cex = 0.01, main="Hustota pravdepodobnosti")
grid()

# vykreslime si Distribucni funkci
F_x = pnorm(x, mean=mu, sd=sigma)
plot(x, F_x, type = 'l', main="Distribucni funkce")
grid()


# 4B
# Tlak mimo rozmezi 90 az 120g
1- (pnorm(120, mean=mu, sd=sigma) - pnorm(90, mean=mu, sd=sigma)) # s pravdepodobnosti 22,58 %

# Zaznaceni do grafu
# x = c(seq(from = 50, to = 90, by = 0.1),seq(from = 120, to = 180, by = 0.1))
# Vysrafovana oblast je pod krivkou v levo od hodnoty 90 a v pravo od hodnoty x=120 a y<0
x = seq(from = 70, to = 150, by = 0.1)
f_x = dnorm(x, mean=mu, sd=sigma)
plot(x, f_x, cex = 0.01, main="Hustota pravdepodobnosti")
lines(c(90,90),c(0,max(f_x)))
lines(c(120,120),c(0,max(f_x)))
grid()


# 4C
# Tlak nizsi nez 105mmHg
pnorm(105, mean=mu, sd=sigma) # s pravdepodobnosti 24,2 %
F_x = pnorm(x, mean=mu, sd=sigma)
plot(x, F_x, type = 'l', main="Distribucni funkce s tlakem")
lines(c(105,105),c(0,max(F_x)))
lines(c(70,150),c(pnorm(105, mean=mu, sd=sigma),pnorm(105, mean=mu, sd=sigma)))
grid()


# 4D
# 82. percentil
qnorm(0.82, mean=mu, sd=sigma) # je 121.15 mmHg
# Vysvetleni percentilu: 82% lidi ma hodnotu systolickeho krevniho tlaku do hranice 121.15 mmHg

