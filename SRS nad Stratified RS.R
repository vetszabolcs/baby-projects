# Az 50.000 elemből álló populáció Magyarország 5 legnépesebb városának bizonyos, fiktív
# szempontok alapján szelektált felnőtt lakosságát jelképezi, illetve ezen lakosok súlyát, valamint,
# hogy dohányzik-e az adott lakos. A városok N értékei a teljes populáció 7000 és 14000 közötti
# véletlen számot kapták, vagyis az adott város elemszáma a lehető legkisebb népesség
# esetén is a teljes populáció legalább 14%-át, legfeljebb 28%-át adja.
# Mindkét változó létrehozása során törekedtem rá, hogy bizonyos mértékben tükrözzék
# a világban megfigyelhető értékeket. Ennek megfelelően, mivel a felnőtt emberek átlagos testsúlya -
# ha a nőkét és férfiakét egyaránt számításba vesszük -, jellemzően 60 és 90 közötti értéket vesz fel,
# megközelítőleg 10-es szórás mellett, a súlyra vonatkozó rng is 60 és 90 között generál átlagra vonatkozó
# érékeket, melyekhez 8 és 12 közé eső véletlen szórást rendel. (Mivel viszonylag nagy a szórás, valamint 
# a férfiak és nők adatai együtt vannak kezelve, a populációban lehetnek extra alacsony testsúllyal rendelkező 
# személyek is.) A dohányzók populációjának létrehozásakor is ezt az elvet követtem. Európában országonként 8% 
# és 30% közé tehető a dohányzók aránya, így a dohányozók adatait létrehozó rng - az érdekesség kedvéért 
# valamivel nagyobb - 0 és 0.4 közötti valószínűséggel vehet fel 1-et (dohányzik), tehát a dohányzók aránya 
# a különböző településeken 0% és 40% között lehet.
# Az így létrehozott populációból vettem egy 500 elemű egyszerű véletlen, illetve egy 1500 elemű arányosan rétegzett
# mintát. Mindkét mintában kiszámoltam a testsúly átlagára, a dohányzók arányára, a szórásra és a totálra vonatkozó 
# becsléseket valamint ezek 95%-os konfidencia intervallumait. A súlyra vonatkozó közelítőleges eloszlásokat mindkét 
# minta esetén hisztogrammok segítségével ábrázoltam a teljes minta és településenkénti bontás esetén is.
# Megj.: a mintavétel során ciklus segítségével hoztam létre globális objektumokat

# Populáció
# N = 50000

N <- 50000

# Az alpopulációk méretei:
popn <- NULL
while (sum(popn) != N){
  for (i in 1:5){
    popn[i] <- round(runif(1, 7000, 14000))
}}
popn

#Alpopulációk elnevezése, súly és dohányzó (1=igen, 0=nem) változók hozzárendelése az alpopulációkhoz
pop <- data.frame(város = rep(c("Budapest", "Debrecen", "Szeged", "Miskolc", "Pécs"), times = popn),
                  súly = c(rnorm(popn[1], runif(1, 60, 90), runif(1, 7, 13)),
                            rnorm(popn[2], runif(1, 60, 90), runif(1, 7, 13)),
                            rnorm(popn[3], runif(1, 60, 90), runif(1, 7, 13)),
                            rnorm(popn[4], runif(1, 60, 90), runif(1, 7, 13)),
                            rnorm(popn[5], runif(1, 60, 90), runif(1, 7, 13))),
              dohányzó = c(round(rbinom(popn[1], 1, runif(1,0,0.4))),
                           round(rbinom(popn[2], 1, runif(1,0,0.4))),
                           round(rbinom(popn[3], 1, runif(1,0,0.4))),
                           round(rbinom(popn[4], 1, runif(1,0,0.4))),
                           round(rbinom(popn[5], 1, runif(1,0,0.4)))))
              
summary(pop)


hist(pop$súly)

#Változók átlaga és szórása
átlag_súly <- mean(pop$súly)
átlag_súly
átlag_doh <- mean(pop$dohányzó)
átlag_doh
se_súly <- sqrt(var(pop$súly))
se_súly 
se_dohányzó <- N/(N-1)*átlag_doh*(1-átlag_doh)
se_dohányzó


#Az alpopulációk átlagai
#Súly
for(l in unique(pop$város)){
  print(paste(l, mean(pop$súly[pop$város==l]), sep = ": "))
}

#Dohányzók aránya
for(l in unique(pop$város)){
print(paste(l, ": ", round(mean(pop$dohányzó[pop$város==l])*100), "%" , sep = ""))
}

#Total becslések
tau_súly <- átlag_súly*N
tau_súly
tau_dohányzó <- átlag_doh*N
tau_dohányzó

#Totalra vonatkozó konfidencia intervallumok
qnorm(c(0.025, 0.975), mean = tau_súly, sd = se_súly)
qnorm(c(0.025, 0.975), mean = tau_dohányzó, sd = se_dohányzó) #dohányzók száma

#Egyszerű véletlen minta, n=500------------------------------------------------
SRS_n1 <- pop[sample(nrow(pop), 500),]

# Mintabeli átlag és szórás becslése
SRS_átlag_súly <- mean(SRS_n1$súly)
SRS_szórás_súly <- sqrt(var(SRS_n1$súly))
SRS_átlag_doh <- mean(SRS_n1$dohány)
SRS_szórás_doh <- sqrt(((N-500)/N) * (SRS_átlag_doh*(1-SRS_átlag_doh)/(500-1)))

# Becslések konfidencia intervallumai
qnorm(c(0.025, 0.975), mean = mean(SRS_n1$súly), sd = sqrt(var(SRS_n1$súly)))
qnorm(c(0.025, 0.975), mean = SRS_átlag_doh, sd = SRS_szórás_doh)

# Total becslések
SRS_tau_átlag_súly <- mean(SRS_n1$súly)*N
SRS_tau_átlag_súly
SRS_tau_szórás_súly <- sqrt(SRS_szórás_súly^2*N^2)
SRS_tau_szórás_súly

SRS_tau_átlag_doh <- mean(SRS_n1$dohányzó)*N
SRS_tau_átlag_doh #dohányzók száma
SRS_tau_szórás_doh <- sqrt(SRS_szórás_doh^2*N^2)
SRS_tau_szórás_doh

# Totalra vonatkozó CI
qnorm(c(0.025, 0.975), mean = SRS_tau_átlag_súly, sd = SRS_tau_szórás_súly)
qnorm(c(0.025, 0.975), mean = SRS_tau_átlag_doh, sd = SRS_tau_szórás_doh)


#Alpopulációk (közelítőleges) eloszlásai
par(mfrow = c(2,3))
hist(SRS_n1$súly[SRS_n1$város == "Pécs"], main = "Pécs")
hist(SRS_n1$súly[SRS_n1$város == "Miskolc"], main = "Miskolc")
hist(SRS_n1$súly[SRS_n1$város == "Budapest"], main = "Budapest")
hist(SRS_n1$súly[SRS_n1$város == "Szeged"], main = "Szeged")
hist(SRS_n1$súly[SRS_n1$város == "Debrecen"], main = "Debrecen")
hist(SRS_n1$súly, main = "Teljes minta")




#Arányosan rétegzett véletlen minta, n~1500-----------------------------------------

# Alpopulációk külön adattáblákként való elmentése:
for (o in unique(pop$város)){
  assign(o, pop[pop$város==o,])
}

# A rétegek méreteinek meghatározása:
stratum_n <- round(popn/sum(popn)*1500)
stratum_n

# A minta tényleges mérete az arányok figyelembevétele mellett:
n <- sum(stratum_n)
n


# Arányosan rétegzett mintavétel, a minták adattáblákként való elmentése
városok <- as.character(unique(pop$város))

for (i in 1:length(városok)){
  assign(x = paste("S_S", városok[i], sep = "_"), value = (get(városok[i])[sample(nrow(get(városok[i])), 
                                                                                  size = stratum_n[i]),]))
}

# A létrehozott minták összefűzése egy adattáblába
for (i in 1:length(városok)){ 
  cat(paste(paste("S_S", városok[i], sep = "_" ), ",", sep = ""))
}

Strat_s <- rbind(S_S_Budapest,S_S_Debrecen,S_S_Szeged,S_S_Miskolc,S_S_Pécs) 
summary(Strat_s)

# Becslések rétegenként
  #súly
Strat_s_súly_átlagok <- tapply(Strat_s$súly, Strat_s$város, mean)
Strat_s_súly_átlagok
Strat_s_súly_szórások <- tapply(Strat_s$súly, Strat_s$város, sd)
Strat_s_súly_szórások

CI <- function(x){qnorm(c(0.025, 0.975), mean(x), sd(x))}
Strat_s_súly_ci <- tapply(Strat_s$súly, Strat_s$város, CI)
Strat_s_súly_ci

# súly - total
tau <- function(x){mean(x)*N}
Strat_s_súly_tau_átlagok <- (tapply(Strat_s$súly, Strat_s$város, tau))
Strat_s_súly_tau_átlagok

tau_szórás <- function(x){sqrt(sd(x)^2*N^2)}
Strat_s_súly_tau_szórások <- tapply(Strat_s$súly, Strat_s$város, tau_szórás)
Strat_s_súly_tau_szórások

tau_ci <- function(x){qnorm(c(0.025, 0.975), mean(x)*N, sqrt(sd(x)^2*N^2))}
Strat_s_súly_tau_ci <- tapply(Strat_s$súly, Strat_s$város, tau_ci)
Strat_s_súly_tau_ci


# dohányzás
Strat_s_doh_átlagok <- tapply(Strat_s$dohányzó, Strat_s$város, mean)
Strat_s_doh_átlagok

szórás_dummy <- function(x){
  sqrt(((N-n)/N) * (mean(x)*(1-mean(x))/(n-1)))}

Strat_s_doh_szórások <- tapply(Strat_s$dohányzó, Strat_s$város, szórás_dummy)
Strat_s_doh_szórások

CI_dummy <- function(x){
  qnorm(c(0.025, 0.975), mean(x), sqrt(((N-n)/N) * (mean(x)*(1-mean(x))/(n-1))))}
Strat_s_doh_ci <- tapply(Strat_s$dohányzó, Strat_s$város, CI_dummy)
Strat_s_doh_ci

# dohányzás - total
Strat_s_doh_tau_átlagok <- (tapply(Strat_s$dohányzó, Strat_s$város, tau))
Strat_s_doh_tau_átlagok

tau_szórás_dummy <- function(x){sqrt(szórás_dummy(x)^2*N^2)}
Strat_s_doh_tau_szórások <- tapply(Strat_s$dohányzó, Strat_s$város, tau_szórás_dummy)
Strat_s_doh_tau_szórások

tau_ci_dummy <- function(x){qnorm(c(0.025, 0.975), mean(x)*N, sqrt(szórás_dummy(x)^2*N^2))}
Strat_s_doh_tau_ci <- tapply(Strat_s$dohányzó, Strat_s$város, tau_ci_dummy)
Strat_s_doh_tau_ci

# Becslések a teljes minta alapján
  # súly
Strat_s_súly_átlag <- mean(Strat_s_súly_átlagok)
Strat_s_súly_átlag
Strat_s_súly_szórás <- sd(Strat_s_súly_átlagok)
Strat_s_súly_szórás

Strat_s_súly_ci <- qnorm(c(0.025, 0.975), Strat_s_súly_átlag, Strat_s_súly_szórás)
Strat_s_súly_ci

#  súly - total
Strat_s_súly_tau_teljes_átlag <- mean(Strat_s_súly_átlag)*N
Strat_s_súly_tau_teljes_átlag

Strat_s_súly_tau_teljes_szórás <- sqrt(Strat_s_súly_szórás^2*N^2)
Strat_s_súly_tau_teljes_szórás

Strat_s_súly_tau_ci_teljes <- qnorm(c(0.025, 0.975), Strat_s_súly_tau_teljes_átlag,
                                    Strat_s_súly_tau_teljes_szórás)
Strat_s_súly_tau_ci_teljes

# Totalra vonatkozó CI az SRS-nél
qnorm(c(0.025, 0.975), mean = SRS_tau_átlag_súly, sd = SRS_tau_szórás_súly)

# dohányzás
Strat_s_doh_átlag <- mean(Strat_s_doh_átlagok)
Strat_s_doh_átlag
Strat_s_doh_szórás <- szórás_dummy(Strat_s_doh_szórások)
Strat_s_doh_szórás

Strat_s_doh_ci <- qnorm(c(0.025, 0.975), Strat_s_doh_átlag, Strat_s_doh_szórás)
Strat_s_doh_ci

#  dohányzás - total
Strat_s_doh_tau_teljes_átlag <- mean(Strat_s_doh_átlag)*N
Strat_s_doh_tau_teljes_átlag

Strat_s_doh_tau_teljes_szórás <- sqrt(Strat_s_doh_szórás^2*N^2)
Strat_s_doh_tau_teljes_szórás

Strat_s_doh_tau_ci_teljes <- qnorm(c(0.025, 0.975), Strat_s_doh_tau_teljes_átlag,
                                    Strat_s_doh_tau_teljes_szórás)
Strat_s_doh_tau_ci_teljes

# Totalra vonatkozó CI az SRS-nél
qnorm(c(0.025, 0.975), mean = SRS_tau_átlag_doh, sd = SRS_tau_szórás_doh)

#A súly (közelítőleges) eloszlása a mintában
par(mfrow = c(2,3))
hist(S_S_Pécs$súly, main = "Pécs")
hist(S_S_Miskolc$súly, main = "Miskolc")
hist(S_S_Budapest$súly, main = "Budapest")
hist(S_S_Szeged$súly, main = "Szeged")
hist(S_S_Debrecen$súly, main = "Debrecen")
hist(Strat_s$súly, main = "Teljes minta")


