# C1

# a)
perm_aleatoare = function(n) {
  U = runif(n)
  perm = order(U)
  return(perm)
}

bytestrings_random = function(n, k) {
  bytestrings = replicate(n, sample(0:1, k, replace = TRUE))
  return(bytestrings)
}


set.seed(123)
n = 6
k = 4

print(perm_aleatoare(n))
print(bytestrings_random(n, k))

