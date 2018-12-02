house_data <- read.table("/Users/andrluc/Documents/Facultate/Master/SpecialChaptersOfArtificialIntelligence/Homework4/house.dat.txt", 
                         header=TRUE)
sample_size = nrow(house_data)
column_names = colnames(house_data)

Y = house_data["PRICE"][, 1]

for (size in 1:13) {
  # generate combinations for the columns of size "size"
  combinations = combn(column_names[2: 14], size)
  combinations_count = ncol(combinations)

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
    
    print(beta)
  }
}
  