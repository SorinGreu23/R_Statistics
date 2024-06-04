# B1

# Functia pentru a verifica daca un punct apartine torului
punct_tor = function(x1, x2, x3, R, r) {
  distanta_centru = sqrt(x1^2 + x2^2) - R
  return(distanta_centru^2 + x3^2 < r^2)
}

# Functia pentru a estima volumul torului utilizand Monte Carlo
estimare_volum_tor = function(R, r, n_samples) {
  # Generam puncte aleatorii in interiorul unui cub care contine torul
  x1 = runif(n_samples, -R - r, R + r)
  x2 = runif(n_samples, -R - r, R + r)
  x3 = runif(n_samples, -r, r)
  
  # Calculam numarul de puncte din interiorul torului
  inside = punct_tor(x1, x2, x3, R, r)
  nr_puncte = sum(inside)
  
  # Calculam volumul estimat
  volum_cub = (2 * (R + r))^3
  volum_tor = nr_puncte / n_samples * volum_cub
  
  return(volum_tor)
}

# Valorile pentru R si r
R = 10
r = 3

# Valorile exacte pentru volumul torului
volum_exact = 2 * pi^2 * R * r^2

# Estimarea volumului torului pentru diferite dimensiuni ale eșantionului
samples = c(10000, 20000, 50000)
estimari = sapply(samples, estimare_volum_tor, R = R, r = r)
erori_rel = abs(estimari - volum_exact) / volum_exact

# Afișarea rezultatelor
cat("Volumul exact al torului este:", volum_exact, "\n")
for (i in seq_along(samples)) {
  cat("Pentru esantionul de dimensiune", samples[i], ":\n")
  cat("  Volumul estimat:", estimari[i], "\n")
  cat("  Eroarea relativa:", erori_rel[i], "\n")
}

# ==============================================================================

# B2

# Functie pentru a verifica daca un punct apartine triunghiului T
punct_triunghi = function(x, y) {
  return(y >= 0 & y <= 6 - 3 * x & y <= 2 * x)
}

# Functie pentru a estima aria triunghiului T utilizand Monte Carlo
estimare_arie_triunghi = function(a, b, c, d, n_samples) {
  # Generam puncte aleatorii in interiorul dreptunghiului [a, b] × [c, d]
  x = runif(n_samples, a, b)
  y = runif(n_samples, c, d)
  
  # Calculam numarul de puncte din interiorul triunghiului
  inside = punct_triunghi(x, y)
  n_inside = sum(inside)
  
  # Calculam aria estimata
  arie_dreptunghi = (b - a) * (d - c)
  arie_triunghi = n_inside / n_samples * arie_dreptunghi
  
  return(arie_triunghi)
}

# Definim zona dreptunghiulara care include triunghiul T
a = 0; b = 2
c = 0; d = 6

# Estimam aria triunghiului T utilizand Monte Carlo
n_samples = 20000
arie_estimata = estimare_arie_triunghi(a, b, c, d, n_samples)

cat("Aria estimată a triunghiului T este:", arie_estimata, "\n")

# ==============================================================================

# B3

# a)
# Definirea functiei pentru integrala a
f_a = function(x) {
  return((2 * x - 1) / (x^2 - x - 6))
}

# Functie pentru a estima integrala folosind metoda Monte Carlo
estimare_integrala_a = function(n_samples) {
  x = runif(n_samples, -1, 1)
  f_values = f_a(x)
  return(mean(f_values) * (1 - (-1)))
}

# Estimarea integralei a
n_samples = 20000
estimare_a = estimare_integrala_a(n_samples)

# Valoarea exacta a integralei a
valoare_exacta_a = log(3) - log(2)

cat("Estimarea integralei a:", estimare_a, "\n")
cat("Valoarea exacta a integralei a:", valoare_exacta_a, "\n")
cat("Eroare relativa:", abs(estimare_a - valoare_exacta_a) / abs(valoare_exacta_a), "\n")

# ------------------------------------------------------------------------------

# b)
# Definirea functiei pentru integrala b
f_b = function(x) {
  return((x + 4) / (x - 3)^(1/3))
}

# Functie pentru a estima integrala folosind metoda Monte Carlo
estimare_integrala_b = function(n_samples) {
  x = runif(n_samples, 3, 11)
  f_values = f_b(x)
  return(mean(f_values) * (11 - 3))
}

# Estimarea integralei b
estimare_b = estimare_integrala_b(n_samples)

# Valoarea exacta a integralei b
valoare_exacta_b = 61.2

cat("Estimarea integralei b:", estimare_b, "\n")
cat("Valoarea exacta a integralei b:", valoare_exacta_b, "\n")
cat("Eroare relativa:", abs(estimare_b - valoare_exacta_b) / abs(valoare_exacta_b), "\n")

# ------------------------------------------------------------------------------

# c)
# Definirea functiei pentru integrala c
f_c = function(x) {
  return(x * exp(-x^2))
}

# Functie pentru a estima integrala folosind metoda Monte Carlo
estimare_integrala_c = function(n_samples) {
  u = runif(n_samples)
  x = -log(1 - u) / 2
  f_values = f_c(x)
  return(mean(f_values))
}

# Estimarea integralei c
estimare_c = estimare_integrala_c(n_samples)

# Valoarea exacta a integralei c
valoare_exacta_c = 0.5

cat("Estimarea integralei c:", estimare_c, "\n")
cat("Valoarea exacta a integralei c:", valoare_exacta_c, "\n")
cat("Eroare relativa:", abs(estimare_c - valoare_exacta_c) / abs(valoare_exacta_c), "\n")

# ==============================================================================

# B4

# Setam parametrii problemei
n = 1000
p = 0.25
q = 0.01
U0 = 10000
U_target = 15000

# Functie pentru a simula un an
simulare_an = function(U) {
  utilizatori_noi = rbinom(1, n, p)
  utilizatori_ramasi = rbinom(1, U, 1 - q)
  return(utilizatori_noi + utilizatori_ramasi)
}

# ------------------------------------------------------------------------------

# a) Estimarea numarului mediu de ani
estimare_ani = function(simulari) {
  ani_totali = vector()
  
  for (sim in 1:simulari) {
    U = U0
    ani = 0
    
    while (U < U_target) {
      U = simulare_an(U)
      ani = ani + 1
    }
    
    ani_totali = c(ani_totali, ani)
  }
  
  return(mean(ani_totali))
}

# ------------------------------------------------------------------------------

# b) Probabilitatea de a avea cel puțin 15000 utilizatori dupa 40 ani si 10 luni
probabilitate_40_ani = function(simulari) {
  succes = 0
  ani = 40 + 10 / 12
  
  for (sim in 1:simulari) {
    U = U0
    
    for (an in 1:ani) {
      U = simulare_an(U)
    }
    
    if (U >= U_target) {
      succes = succes + 1
    }
  }
  
  return(succes / simulari)
}

# ------------------------------------------------------------------------------

# c) Eroarea de estimare a probabilitatii
estimare_eroare = function(probabilitate, simulari) {
  return(sqrt(probabilitate * (1 - probabilitate) / simulari))
}

# Setam numarul de simulari
simulari = 10000

# Calculam estimarile
estimare_a = estimare_ani(simulari)
probabilitate_b = probabilitate_40_ani(simulari)
eroare_c = estimare_eroare(probabilitate_b, simulari)

cat("Estimarea numărului mediu de ani (a):", estimare_a, "\n")
cat("Probabilitatea de a avea cel puțin 15000 utilizatori după 40 ani și 10 luni (b):", probabilitate_b, "\n")
cat("Eroarea de estimare a probabilității (c):", eroare_c, "\n")
