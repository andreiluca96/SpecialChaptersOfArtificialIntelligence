# Ex 3

ex1 = function(m, a, b, xmin, xmax, sigma){
  errors = rnorm(m, sd = sigma * sigma) # N(0, sigma^2)
  x = runif(m, min = xmin, max = xmax) # distrib unif
  
  result = a + b * x + errors # simple regression
  
  list(x = x, y = result)
}

ex2 = function(obs) {
  coefficients = lm(obs$y~obs$x)
  
  a = coefficients$coef[1]
  b = coefficients$coef[2]
  
  confidence_intervals = confint(coefficients, level = 0.95)
  a_confidence_interval = confidence_intervals["(Intercept)", ]
  b_confidence_interval = confidence_intervals["obs$x", ]
  
  list(a, b, a_confidence_interval, b_confidence_interval)
}

# a
subpunct_a = function() {
  a = 3
  b = 5
  m = 100
  xmin = -200
  xmax = 200
  sigma = 1.5
  
  obs = ex1(m, a, b, xmin, xmax, sigma)
  
  estimated = ex2(obs)

  pdf("/Users/andrluc/Documents/Facultate/Master/SpecialChaptersOfArtificialIntelligence/Lab4/a.pdf")
  
  plot(obs$x, obs$y, pch = 19)
  abline(a, b,lwd=1,col="red")
  abline(estimated[[1]], estimated[[2]], lwd=1, col="blue")
  
  dev.off()
}
subpunct_a()

# b
subpunct_b = function() {
  a = 3
  b = 5
  m = 10
  xmin = -5
  xmax = 5
  sigma = 1
  
  obs = ex1(m, a, b, xmin, xmax, sigma)
  
  estimated = ex2(obs)
  
  pdf("/Users/andrluc/Documents/Facultate/Master/SpecialChaptersOfArtificialIntelligence/Lab4/b.pdf")
  
  plot(obs$x, obs$y, pch = 19)
  abline(a, b,lwd=1,col="red")
  abline(estimated[[1]], estimated[[2]], lwd=1, col="blue")
  
  dev.off()
}
subpunct_b()

# c
subpunct_c = function() {
  a = 3
  b = 5
  m = 10000
  xmin = -5
  xmax = 5
  sigma = 1
  
  obs = ex1(m, a, b, xmin, xmax, sigma)
  
  estimated = ex2(obs)
  
  pdf("/Users/andrluc/Documents/Facultate/Master/SpecialChaptersOfArtificialIntelligence/Lab4/c.pdf")
  
  plot(obs$x, obs$y, pch = 19)
  abline(a, b,lwd=1,col="red")
  abline(estimated[[1]], estimated[[2]], lwd=1, col="blue")
  
  dev.off()
}
subpunct_c()

# d
subpunct_d = function() {
  a = 3
  b = 5
  m = 10
  xmin = 5
  xmax = 5.2
  sigma = 1
  
  obs = ex1(m, a, b, xmin, xmax, sigma)
  
  estimated = ex2(obs)
  
  pdf("/Users/andrluc/Documents/Facultate/Master/SpecialChaptersOfArtificialIntelligence/Lab4/d.pdf")
  
  plot(obs$x, obs$y, pch = 19)
  abline(a, b,lwd=1,col="red")
  abline(estimated[[1]], estimated[[2]], lwd=1, col="blue")
  
  dev.off()
}
subpunct_d()

# e
subpunct_e = function() {
  a = 3
  b = 5
  m = 10000
  xmin = 5
  xmax = 5.2
  sigma = 1
  
  obs = ex1(m, a, b, xmin, xmax, sigma)
  
  estimated = ex2(obs)
  
  pdf("/Users/andrluc/Documents/Facultate/Master/SpecialChaptersOfArtificialIntelligence/Lab4/e.pdf")
  
  plot(obs$x, obs$y, pch = 19)
  abline(a, b,lwd=1,col="red")
  abline(estimated[[1]], estimated[[2]], lwd=1, col="blue")
  
  dev.off()
}
subpunct_e()

# f
subpunct_f = function() {
  a = 3
  b = 5
  m = 10000
  xmin = 5
  xmax = 5.2
  sigma = 1
  
  obs = ex1(m, a, b, xmin, xmax, sigma)
  
  estimated = ex2(obs)
  
  pdf("/Users/andrluc/Documents/Facultate/Master/SpecialChaptersOfArtificialIntelligence/Lab4/f.pdf")
  
  plot(obs$x, obs$y, pch = 19)
  abline(a, b,lwd=1,col="red")
  abline(estimated[[1]], estimated[[2]], lwd=1, col="blue")
  
  dev.off()
}
subpunct_f()
