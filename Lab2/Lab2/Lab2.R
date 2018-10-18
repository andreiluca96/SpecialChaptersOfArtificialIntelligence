# ex2
x = c(1, 8, 2, 6, 3, 8, 8, 5, 5, 5)

# a
a = sum(x) / 10
print(a)

# b
for(i in 1:10) {
  print(log2(i))
}

# c
c = max(x) - min(x)
print(c)

# d
d = c(1:10)
for(i in 1:10) {
  d[i] = (x[i] - 5.1) / 2.514403
}
print(d)

# e
print(mean(d))
print(sd(d))

# ex3
factura = c(46, 33, 39, 37, 46, 30, 48, 32, 49, 35, 30, 48)

#suma
print(sum(factura))

#min
print(min(factura))

#max
print(max(factura))

# > 40
count = 0
for (i in 1:length(factura)) {
  if (factura[i] > 40) {
    count = count + 1;
  }
}
print(count)

# % (facturi > 40)
print(count / length(factura) * 100)


# ex4
vect = strsplit(readline("Add input:"), ",")

result = c(1:length(vect[[1]]))

for (i in 1:length(vect[[1]])) {
  result[i] = as.integer(vect[[1]][i]);
}

print(result)

# max
print(max(result))
# min
print(min(result))
# mean
print(mean(result))
# P50
print(median(result))
# sd
print(sd(result))
# sort
print(sort(result))
#standardization
standardized = c(1:length(result))
for(i in 1:length(result)) {
  standardized[i] = (result[i] - mean(result)) / sd(result)
}
print(standardized)
