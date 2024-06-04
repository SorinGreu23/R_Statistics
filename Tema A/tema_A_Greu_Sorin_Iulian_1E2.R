# Ex A1

# Valori aleatorii pentru lambda, p, n, m, k
lambda = 2.6; p = 0.3; n = 10; m = 10; k = 2

# a)
calcul_probabilitati = function(lambda, p, n, m, k) {
  distrib_poisson = dpois(k:m, lambda)
  distrib_geometrica = dgeom(k:m, p)
  distrib_binomiala = dbinom(k:m, n, p)
  
  cat("Dist. Poisson:", "\n", distrib_poisson, "\n\n", "Dist. Geometrica:", "\n", distrib_geometrica, "\n\n", "Dist. Binomial:", "\n", distrib_binomiala, "\n\n")
  return(list(poisson = distrib_poisson, geom = distrib_geometrica, binom = distrib_binomiala))
}

calcul_probabilitati(lambda, p, n, m, k)

# b)
grafic_probabilitati = function(lambda, p, n, m, k) {
  interval = k:m
  
  # Setez layout-ul graficului pentru a afisa 3 grafice simultan
  par(mfrow = c(3, 1))
  
  # Calculam probabilitatile
  lista_probabilitati = calcul_probabilitati(lambda, p, n, m, k)
  
  # Grafic Poisson
  barplot(lista_probabilitati$poisson, interval, main = 'Distributia Poisson', xlab = 'Valori', ylab = 'Probabilitati', col = 'lightblue')
  
  # Grafic Geometric
  barplot(lista_probabilitati$geom, interval, main = 'Distributia Geometrica', xlab = 'Valori', ylab = 'Probabilitati', col = 'lightgreen')
  
  # Grafic Binomial
  barplot(lista_probabilitati$binom, interval, main = 'Distributia Binomiala', xlab = 'Valori', ylab = 'Probabilitati', col = 'purple')
  
  # Resetam layout-ul graficului
  par(mfrow = c(1, 1))
}

grafic_probabilitati(lambda, p, n, m, k)

# c)
find_min_k0 = function(lambda) {
  k0 = 0
  prob_sum = 0
  
  # Iteram pentru a gasi cea mai mica valoare a lui k0 care satisface conditia
  while (prob_sum < 1 - 1e-6) { # 1 - 1e-6 =  1 - 10 ^ (- 6)
    prob_sum = prob_sum + dpois(k0, lambda)
    k0 = k0 + 1
  }
  return(k0 - 1)
}

# ==============================================================================

# A2

# a)
frecvente_note_PS = function(file_name) {
  # Citim notele din fisierul specificat
  note = read.csv(file_name, header = TRUE, sep = ",")
  
  # Construim cele doua esantioane din coloanele corespunzatoare
  esantion_P = note$P
  esantion_S = note$S
  
  # Calculam frecventele absolute pentru fiecare esantion
  frecvente_absolute_P = as.vector(table(esantion_P))
  frecvente_absolute_S = as.vector(table(esantion_S))
  
  # Calculam frecventele relative pentru fiecare esantion
  frecvente_relative_P = frecvente_absolute_P / length(esantion_P)
  frecvente_relative_S = frecvente_absolute_S / length(esantion_S)
  
  # Calculam media pentru fiecare esantion
  media_P = mean(esantion_P)
  media_S = mean(esantion_S)
  
  # Rezultatele pentru esantionul P
  cat(" Frecventele absolute pentru esantionul P:\n", frecvente_absolute_P, "\n\n")
  cat(" Frecventele relative pentru esantionul P:\n", frecvente_relative_P, "\n\n")
  
  # Rezultatele pentru esantionul S
  cat(" Frecventele absolute pentru esantionul S:\n", frecvente_absolute_S, "\n\n")
  cat(" Frecventele relative pentru esantionul S:\n", frecvente_relative_S, "\n\n")
  
  # Mediile pentru fiecare esantion
  cat(" Media esantionului P:", media_P, "\n\n")
  cat(" Media esantionului S:", media_S, "\n\n")
}

frecvente_note_PS("note_PS.csv")

# b)
elimina_valori_aberante = function(file_name, esantion_name) {
  # Citim datele din fisier
  date = read.csv(file_name, header = TRUE, sep = ",")
  
  # Extragem esantionul specificat
  esantion = date[[esantion_name]]
  
  # Calculam media si deviatia standard a esantionului
  m = mean(esantion)
  s = sd(esantion)
  
  # Detectam valorile aberante si le eliminam din esantion
  outliers = esantion_curatat = vector()
  j = k = 0
  for (i in 1:length(esantion)) {
    if (esantion[i] < m - 2 * s || esantion[i] > m + 2 * s) {
      j = j + 1
      outliers[j] = esantion[i]
    }
    else {
      k = k + 1
      esantion_curatat[k] = esantion[i]
    }
  }
  
  # Reprezentam grafic distributia frecventelor din esantionul curatat
  hist(esantion, breaks = seq(1, 10, by = 1), main = "Distributia frecventelor", xlab = "Intervale", ylab = "Frecventa", col = "lightblue")
  
  # Returnam esantionul 
  return(esantion_curatat)
}

elimina_valori_aberante("note_PS.csv", "P")
