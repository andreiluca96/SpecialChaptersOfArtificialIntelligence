# Ex 1

ex1 = function(m, a, b, xmin, xmax, sigma){
  errors = rnorm(m, sd = sigma * sigma) # N(0, sigma^2)
  x = runif(m, min = xmin, max = xmax) # distrib unif
  
  result = a + b * x + errors # simple regression
  
  list(x = x, y = result)
}

ex1(10, 10, 2, -10, 10, 1)