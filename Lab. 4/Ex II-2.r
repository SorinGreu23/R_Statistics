# (λ = 3, N = 50000)
MC_improved_integration = function(N) {
  sum = 0;
  lambda = 3
  for(i in 1:N) {
    u = rexp(1, lambda);
    sum = sum + exp(-2 * u^2)/exp(-u*lambda);
  }
  return(sum/N);
}

N = 50000

MC_improved_integration(N)
