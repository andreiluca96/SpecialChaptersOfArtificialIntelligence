# Ex 2

# Ex 1

ex1 = function(m, a, b, xmin, xmax, sigma){
  errors = rnorm(m, sd = sigma * sigma) # N(0, sigma^2)
  x = runif(m, min = xmin, max = xmax) # distrib unif
  
  result = a + b * x + errors # simple regression
  
  list(x = x, y = result)
}

obs = ex1(10, 10, 2, -10, 10, 1)


ex2 = function(obs) {
  coefficients = lm(obs$y~obs$x)
  
  a = coefficients$coef[1]
  b = coefficients$coef[1]
  
  confidence_intervals = confint(coefficients, level = 0.95)
  a_confidence_interval = confidence_intervals["(Intercept)", ]
  b_confidence_interval = confidence_intervals["obs$x", ]
  
  list(a, b, a_confidence_interval, b_confidence_interval)
}

ex2(obs)
