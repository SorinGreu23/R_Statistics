# D1

zconfidence_interval = function(file_name, confidence_level) {
  date = read.csv(file_name, header = TRUE)
  
  n = length(date$probabilitati)
  sample_mean = mean(date$probabilitati)
  alpha = 1 - confidence_level
  sigma = sqrt(92.16)
  
  critical_z = qnorm(1 - alpha / 2, 0, 1)
  
  lower_bound = sample_mean - critical_z * sigma / sqrt(n)
  upper_bound = sample_mean + critical_z * sigma / sqrt(n)
  
  return(c(lower_bound, upper_bound))
}

cat("Interval de incredere de 95%:", zconfidence_interval("probabilitati.csv", 0.95), "\n")
cat("Interval de incredere de 99%:", zconfidence_interval("probabilitati.csv", 0.99), "\n")

# ==============================================================================

# D2

intervale_incredere = function(file_name, confidence_level) {
  data = read.csv(file_name)
  
  media_esantion = mean(data$statistica)
  deviatia_standard = sd(data$statistica)
  n = length(data$statistica)
  alpha = 1 - confidence_level
  
  critical_z = qnorm(1 - alpha / 2, 0, 1)
  marja_eroare = critical_z * deviatia_standard / sqrt(n)
  
  lower_bound = media_esantion - marja_eroare
  upper_bound = media_esantion + marja_eroare
  
  return(c(lower_bound, upper_bound))
}


cat("Interval de încredere de 95%:", intervale_incredere("statistica.csv", 0.95), "\n")
cat("Interval de încredere de 99%:", intervale_incredere("statistica.csv", 0.99), "\n")

# ==============================================================================

# D3

test_proportion = function(n, alpha, successes, p0) {
  p_prim = successes / n;
  z_score = (p_prim - p0) / sqrt(p0 * (1 - p0) / n)
  critical_z = qnorm(1 - alpha, 0, 1)
  
  cat("Statistica testului", alpha * 100, "%: z_score", z_score, "| critical_z: ", critical_z)
}

n = 100
successes = 86 # cel mult 14 nu pot rezolva temele
p0 = 0.85

cat(test_proportion(n, 0.01, successes, p0), "\n")
cat(test_proportion(n, 0.05, successes, p0), "\n")