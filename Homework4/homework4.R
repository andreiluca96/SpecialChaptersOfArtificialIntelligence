house_data <- read.table("/Users/andrluc/Documents/Facultate/Master/SpecialChaptersOfArtificialIntelligence/Homework4/house.dat.txt", 
                         header=TRUE)
sample_size = nrow(house_data)
column_names = colnames(house_data)

Y = house_data["PRICE"][, 1]
mean_y = mean(Y)

RSS_results = 1:13
RSS_solutions = list()

R_squared_results = 1:13
R_squared_solutions = list()

R_squared_adjusted_results = 1:13
R_squared_adjusted_solutions = list()

C_p_results = 1:13
C_p_solutions = list()

for (size in 1:13) {
  # generate combinations for the columns of size "size"
  combinations = combn(column_names[2: 14], size)
  combinations_count = ncol(combinations)
  
  RSS_min = 99999999
  R_squared_min = 99999999
  R_squared_adjusted_min = 99999999
  C_p_min = 99999999
  for (combination_index in 1:combinations_count) {
    # For each combination compute X
    
    # Fill the matrix with default values.
    X = matrix(1:((size + 1) * sample_size), nrow = sample_size, ncol = size + 1)
    
    # Fill the first column with 1 values.
    X[, 1] = rep(1, sample_size)
    
    # Complete the matrix with actual values.
    for (i in 1:size) {
      X[, i + 1] = house_data[combinations[, combination_index][i]][, 1]
    }
    
    # QR decomposition
    qr_result = qr(X)
    
    Q = qr.Q(qr_result)
    R = qr.R(qr_result)
    
    # Solution:
    # beta = R^{-1} * Q ^ t * y
    
    R_inverse = backsolve(x = diag(nrow(R)), r = R)
    Q_t = t(Q)
    
    beta = R_inverse %*% Q_t %*% Y
    
    # Estimated Y
    estimated_y = X %*% beta

    # RSS
    RSS = 0
    for (i in 1:sample_size) {
      RSS = RSS + (Y[i] - estimated_y[i]) ^ 2
    }
    
    if (RSS < RSS_min) {
      RSS_min = RSS
      RSS_sol = beta
    }
    
    # TSS
    TSS = 0
    for (i in 1:sample_size) {
      TSS = TSS + (Y[i] - mean_y) ^ 2
    }
    
    # R^2
    R_squared = 1 - RSS / TSS
    
    if (R_squared < R_squared_min) {
      R_squared_min = R_squared
      R_squared_min_sol = beta
    }
    
    # R^2 adjusted
    R_squared_adjusted = 1 - (1 - R_squared) * (sample_size - 1) / (sample_size - size - 1)
    
    if (R_squared_adjusted < R_squared_adjusted_min) {
      R_squared_adjusted_min = R_squared_adjusted
      R_squared_adjusted_min_sol = beta
    }
    
    # C_p
    C_p = (RSS / var(estimated_y) - (sample_size - 2 * size) - size) ^ 2
    
    if (C_p < C_p_min) {
      C_p_min = C_p
      C_p_min_sol = beta
    }
  }
  
  RSS_results[size] = RSS_min
  RSS_solutions[[size]] = RSS_sol
  
  R_squared_results[size] = R_squared_min
  R_squared_solutions[[size]] = R_squared_min_sol
  
  R_squared_adjusted_results[size] = R_squared_adjusted_min
  R_squared_adjusted_solutions[[size]] = R_squared_adjusted_min_sol
  
  C_p_results[size] = C_p_min
  C_p_solutions[[size]] = C_p_min_sol
}

barplot(RSS_results, main="Residual Sum of Squares Distribution", xlab = "Selected variable count", names.arg=1:13)
barplot(R_squared_results, main="R^2 Distribution", xlab = "Selected variable count", names.arg=1:13)
barplot(R_squared_adjusted_results, main="R^2 adjusted Distribution", xlab = "Selected variable count", names.arg=1:13)
barplot(C_p_results, main="C_p adjusted Distribution", xlab = "Selected variable count", names.arg=1:13)

print("RSS solution")
print(RSS_solutions[[which.min(RSS_results)]])

print("R^2 solution")
print(R_squared_solutions[[which.max(R_squared_results)]])

print("R^2 adjusted solution")
print(R_squared_adjusted_solutions[[which.min(abs(R_squared_adjusted_results))]])

print ("C_p solution")
print(C_p_solutions[[which.min(abs(C_p_results))]])