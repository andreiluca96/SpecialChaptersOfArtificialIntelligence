library(olsrr)

house_data <- read.table("/Users/andrluc/Documents/Facultate/Master/SpecialChaptersOfArtificialIntelligence/Homework4/house.dat.txt", 
                         header=TRUE)
sample_size = nrow(house_data)
column_names = colnames(house_data)

Y = house_data["PRICE"][, 1]
mean_y = mean(Y)

X = matrix(1:((13 + 1) * sample_size), nrow = sample_size, ncol = 13 + 1)
for (i in 1:13) {
  X[, i + 1] = house_data[column_names[i + 1]][, 1]
}

full_model <- lm(Y ~ X)
print(summary(full_model))
print(coef(summary(full_model))[, "Pr(>|t|)"])

S_2 = 1/11 * sum(full_model$residuals^2)

min_cp = 99999999
for (size in 1:6) {
  # generate combinations for the columns of size "size"
  combinations = combn(column_names[2: 14], size)
  combinations_count = ncol(combinations)
  
  R_squared_adjusted_min = 99999999
  for (combination_index in 1:combinations_count) {
    # For each combination compute X
    
    # Fill the matrix with default values.
    X = matrix(1:((size + 1) * sample_size), nrow = sample_size, ncol = size)
    
    # Complete the matrix with actual values.
    for (i in 1:size) {
      X[, i] = house_data[combinations[, combination_index][i]][, 1]
    }
  
    model <- lm(Y ~ X)
    
    c_p = sum(model$residuals^2) / S_2 - (sample_size - 2 * (size + 1))
    
    if (min_cp > abs(c_p - size - 1)) {
      min_cp = abs(c_p - size - 1)
      cp_predicted = predict(model)
    }
  }
}

# Forward
selected_variable = list()
ok = 1
alpha = 0.05
while (ok == 1) {
  ok = 0
  
  min_p_value = 999999
  for (i in 2:14) {
    new_selected_variable = selected_variable
    if (!column_names[i] %in% new_selected_variable) {
      new_selected_variable[[length(selected_variable) + 1]] = column_names[i]
      
      X = matrix(1:((size + 1) * sample_size), nrow = sample_size, ncol = length(new_selected_variable))
      for (j in 1:length(new_selected_variable)) {
        X[, j] = house_data[new_selected_variable[[j]]][, 1]
      }
      
      model = lm (Y ~ X)
      model_summary = summary(model)
      
      if (length(new_selected_variable) == 1) {
        column_name = "X"
      } else {
        column_name = paste("X", length(new_selected_variable), sep="")
      }
      
      p_value = coef(model_summary)[, "Pr(>|t|)"][column_name]
      
      if (min_p_value > p_value) {
        min_p_value = p_value
        best_selected_variable = new_selected_variable
        best_forward_model = model
      }
      if (p_value < alpha) {
        ok = 1
      }
    }
  }
  selected_variable = best_selected_variable
}

# Backwards
selected_variable = list()
for (i in 2:length(column_names)) {
  selected_variable[[i - 1]] = column_names[i]
}

ok = 1
alpha = 0.90
while (ok == 1) {
  ok = 0
  
  max_p_value = 0
  
  X = matrix(1:(length(selected_variable) * sample_size), nrow = sample_size, ncol = length(selected_variable))
  for (j in 1:length(selected_variable)) {
    X[, j] = house_data[selected_variable[[j]]][, 1]
  }
  
  model = lm(Y ~ X)
  p_values = coef(summary(model))[, "Pr(>|t|)"]
  
  max_p_value_index = which.max(p_values) - 1
  if (p_values[max_p_value_index + 1] > alpha) {
    ok = 1
    selected_variable[[max_p_value_index]] = NULL
  }
}
print(selected_variable)
best_backward_model = model

# Stepwise
selected_variable = list()
ok = 1
alpha1 = 0.05
alpha2 = 0.1
while (ok == 1) {
  ok = 0
  
  min_p_value = 999999
  for (i in 2:14) {
    new_selected_variable = selected_variable
    if (!column_names[i] %in% new_selected_variable) {
      new_selected_variable[[length(selected_variable) + 1]] = column_names[i]
      
      X = matrix(1:((size + 1) * sample_size), nrow = sample_size, ncol = length(new_selected_variable))
      for (j in 1:length(new_selected_variable)) {
        X[, j] = house_data[new_selected_variable[[j]]][, 1]
      }
      
      model = lm (Y ~ X)
      model_summary = summary(model)
      
      if (length(new_selected_variable) == 1) {
        column_name = "X"
      } else {
        column_name = paste("X", length(new_selected_variable), sep="")
      }
      
      p_value = coef(model_summary)[, "Pr(>|t|)"][column_name]
      
      if (min_p_value > p_value) {
        min_p_value = p_value
        best_selected_variable = new_selected_variable
      }
      if (p_value < alpha1) {
        ok = 1
      }
    }
  }
  selected_variable = best_selected_variable
  
  ok2 = 1
  while (ok2 == 1) {
    ok2 = 0
    
    max_p_value = 0
    
    X = matrix(1:(length(selected_variable) * sample_size), nrow = sample_size, ncol = length(selected_variable))
    for (j in 1:length(selected_variable)) {
      X[, j] = house_data[selected_variable[[j]]][, 1]
    }
    
    model = lm(Y ~ X)
    p_values = coef(summary(model))[, "Pr(>|t|)"]
    
    max_p_value_index = which.max(p_values) - 1
    if (p_values[max_p_value_index + 1] > alpha2) {
      ok2 = 1
      selected_variable[[max_p_value_index]] = NULL
    }
  }
}
best_stepwise_model = model

plot (Y, cp_predicted, col="green")
points(Y, predict(best_forward_model), col="red")
points(Y, predict(best_backward_model), col="blue")
points(Y, predict(best_stepwise_model) + 0.1, col="gray")
lines(abline(0,1))

legend(35, 85, legend=c("all_solutions", "forward", "backwards", "stepwise"),
       col=c("green", "red", "blue", "gray"), lty=1, cex=0.5)