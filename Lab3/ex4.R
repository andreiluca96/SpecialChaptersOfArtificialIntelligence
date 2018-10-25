CLT = function(n) {
  generated_sample = matrix(runif(1000 * n, min=-10, max=10), 1000, n)
  rowMeans(generated_sample)
}

hist(CLT(1000))