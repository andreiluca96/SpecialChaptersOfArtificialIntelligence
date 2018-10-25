ex3_function = function() {
  input = c(-20:20)
  
  png(file = '/Users/andrluc/Documents/Facultate/Master/SpecialChaptersOfArtificialIntelligence/Lab3/d0.1.png')
  distrib_norm = dnorm(input, mean=0, sd=0.1)
  plot(input, distrib_norm)
  dev.off()
  
  png(file = '/Users/andrluc/Documents/Facultate/Master/SpecialChaptersOfArtificialIntelligence/Lab3/d1.png')
  distrib_norm = dnorm(input, mean=0, sd=1)
  plot(input, distrib_norm)
  dev.off()
  
  png(file = '/Users/andrluc/Documents/Facultate/Master/SpecialChaptersOfArtificialIntelligence/Lab3/d10.png')
  distrib_norm = dnorm(input, mean=0, sd=10)
  plot(input, distrib_norm)
  dev.off()
  
}

ex3_function()