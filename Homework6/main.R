table = read.table("/Users/andrluc/Documents/Facultate/Master/SpecialChaptersOfArtificialIntelligence/Homework6/swiss.dat", header = TRUE)

# Current variance table
var_table = var(table)
var_table

# The variance for each variable.
variable_variance = diag(var_table)
variable_variance

# Sorted variance list
reveresed_variable_variance = sort(variable_variance, decreasing = TRUE)
reveresed_variable_variance

# Cumulative sum of the reveresed_variable_variance
cumsum_reveresed_variable_variance = cumsum(reveresed_variable_variance)
cumsum_reveresed_variable_variance

percent_vars = 100 * cumsum_reveresed_variable_variance / sum(reveresed_variable_variance)

# PCA
pca = prcomp(table)
pca_table = pca$x

# PCA variance table
pca_var_table = var(pca_table)

# Sorted variance list
pca_reveresed_variable_variance = sort(pca_variable_variance, decreasing = TRUE)
pca_reveresed_variable_variance

# Cumulative sum of the reveresed_variable_variance
pca_cumsum_reveresed_variable_variance = cumsum(pca_reveresed_variable_variance)
pca_cumsum_reveresed_variable_variance

# % Cumulative sum of the reveresed_variable_variance
percent_vars_pca = 100 * pca_cumsum_reveresed_variable_variance/sum(pca_reveresed_variable_variance)

plot(c(1:6), percent_vars,type="l",col="red")
lines(c(1:6),percent_vars_pca,col="green")
legend(2.5, 55, legend=c("old variables", "pca"), col=c("red", "green"), lty=1, cex=0.8,
       title="Cumulative variance percent")

biplot(princomp(table), cex = 0.5, expand=2, xlim=c(-0.25, 0.25), ylim=c(-0.25, 0.25), arrow.len = 0 )