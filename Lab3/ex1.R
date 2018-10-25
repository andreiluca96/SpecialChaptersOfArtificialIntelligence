# Title     : Ex 1
# Objective : Ex 1
# Created by: andrluc
# Created on: 10/25/18

pdf.options(encoding='ISOLatin2.enc')

power_of_two = function(y) {
    2 ^ y
}

ex1_function = function(a, b) {
    title = paste('f(x)=2ˆx, x in [', a, ' ,', b, ']')
    plot(power_of_two, xlim=c(a, b), col='red', lwd=2, main=title)
}

ex1_function(0, 10)