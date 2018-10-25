ex2_function = function(n) {
  input = c(1:n)
  
  for (p in seq(0, 1, by=0.1)) {
    distrib_binom = dbinom(input, n, p)
    file_name = paste('/Users/andrluc/Documents/Facultate/Master/SpecialChaptersOfArtificialIntelligence/Lab3/', p, '.png', sep = '')
    png(file = file_name)
    plot(distrib_binom)
    dev.off()
  }
}

ex2_function(100)
