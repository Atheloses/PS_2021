source('D:\\_VSB\\S8\\PS\\Beres\\GitPS\\CV2\\kombinatorika.R')
#Kolik hesel o délce 15 znaků lze vygenerovat z velkých a malých písmen anglické abecedy, 
#má-li heslo obsahovat alespoň jedno velké písmeno a alespoň jedno malé písmeno? 

#Počet hesel o délce 15 znaků z 52 znaků, alespoň 2*26 znaků
#Vypočítáme kolika různými způsoby se dá sestavit heslo o 15 místech z 52 znaků
#od této hodnoty odečteme způsoby sestavení, které by podmínku alespoň dvou porušily

variace_opak(52,15)-2*variace_opak(26,15)


#Ve třídě je 12 nadějných studentů, z nichž je možno sestavit tříčlenné soutěžní družstvo. 
#Mezi těmito studenty jsou dvě dívky (Anička a Bára), které nejsou schopny spolupracovat, 
#a tudíž je nejde do družstva zařadit obě najednou. Kolika způsoby lze provést výběr členů soutěžního družstva?

#Vypočítáme kolika způsoby lze sestavit družstvo a odečteme kolika způsoby lze sestavit tým, ve kterém by byly dívky spolu
n = 12
k = 3
x = 2
kombinace(n,k)-kombinace(n-x,k-x)
kombinace(12,3)-kombinace(12-2,3-2)

#Z dopravních statistik vyplývá, že u 10 % řidičů, kteří způsobili dopravní nehodu, bylo prokázáno požití alkoholu. 
#V literatuře se uvádí, že riziko nehody se požitím alkoholu zvyšuje 7x. Na základě uvedených údajů odhadněte, kolik procent řidičů požilo před jízdou alkohol. 

#A = Alkohol
#N = Nehoda
#6/7*x=0.1
#x=1/10/6/7
#x=7/60



#Bayesova věta
P_O = c(0.1, 0.9)   #    P(O.),    P(O-)
P_PO = c(1/7, 6/7) # P(P.|O.), P(P.|O-)
bayes(P_B = P_O, P_AB = P_PO, k = 1) 


#Hladina vody v tankeru je kontrolována pomocí čtyř na sobě nezávislých spínačů stejného typu zapojených dle obrázku. Spínače mají být sepnuty při nízké hladině vody. 
#Je-li hladina vody dostatečná, spínače by měly být vypnuty. Každý ze spínačů je s pravděpodobností 10 % v opačném stavu, než by měl být. Ve chvíli, 
#kdy se propojí uzly A a B (tj. např. sepnou spínače 1 a 4), je vyhlášen poplach.  

#S jakou pravděpodobností kontrolní systém (viz obrázek) vyhlásí poplach v případě, že v tankeru je nízká hladina vody? 
#Rozdělíme obvod na paralelní bloky P1 a P2, kde P1 obsahuje spínače 1 a 2, P2 obsahuje 3 a 4. 
#V paralelních blocích se šance na chybu změnšuje, ale mezi paralelními bloky, které jsou zapojeny sériově, se šance na chybu zvětšuje

P1 = 0.1*0.1
P2 = 0.1*0.1
(1 - P1)*(1 - P2)*100

#S jakou pravděpodobností kontrolní systém (viz obrázek) vyhlásí falešný poplach, tj. poplach v případě, že v tankeru je dostatečná hladina vody?
#V tomto případě použijeme stejný výpočet, ale otočíme pravděpodobnost spínačů na správný stav
P1 = 0.9*0.9
P2 = 0.9*0.9
(1 - P1)*(1 - P2)*100




variace(n=3,k=2)
kombinace(n=5,k=3)-kombinace(n=3,k=2)
kombinace(n=3,k=3)
kombinace(n=2,k=2)
kombinace(n=3,k=2)*2
kombinace(n=12,k=3)
kombinace(n=11,k=3)
kombinace(n=11,k=3)*2


kombinace(n=3,k=3)
1
kombinace(n=1,k=1)
kombinace(n=4,k=3)
2
kombinace(n=2,k=1)
kombinace(n=5,k=3)
3
kombinace(n=3,k=1)
kombinace(n=6,k=3)
4
kombinace(n=4,k=1)
kombinace(n=7,k=3)
5
kombinace(n=5,k=1)

kombinace(n=4,k=2)
kombinace(n=4,k=2)
kombinace(n=4,k=1)
kombinace(n=4,k=3)


123
124
134
234

123
124
125
126
134
135
136
145
146
156
234
235
236
245
246
256
345
346
356
456


k = 18
v1 = variace_opak(n = 26, k = k)
v2 = variace_opak(n = 52, k = k)
v3 = variace_opak(n = 62, k = k)
v1
v2
v3/60/60/24/365/1000
v2/v1