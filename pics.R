#Sim output plots

setwd("C:/Users/david/document/fmriSims")

#Sca Free Ravi method
load('out1')
load('out2')
#Erd Ren Ravi Method
load('out3')
load('out4')

#EM Out
load('simOut5')
load('simOut6')

dim(out1)
dim(out2)
dim(simOut5)
dim(simOut6)


#For the mislcassified ravi method, and out1,out3 is a matrix such that
#out1[i,j] is proportion of correct 0 edges in the i^th simulation (i = 1:50)
#for the j^th misclassification level (j ranges from 1:11, or 0 -> 10% misclass)

#out2, out4 is the proportion of correct non zero edges.

misclass = seq(0, 10, by = 1)
#we want a plot that plots each dot, and then draws a line between them
plot(NA, xlim = c(0, 10), ylim = c(0, 1), xlab = "misclass", main = "proportion of correct 0 edge", type = 'n')

for (j in 1:11) {
  points(rep(misclass[j], 50), out1[,j])
}

avg0S = colMeans(out1)
lines(misclass, avg0S)

#more appropriate ylim
misclass = seq(0, 10, by = 1)
#we want a plot that plots each dot, and then draws a line between them
plot(NA, xlim = c(0, 10), ylim = c(0.95, 1), xlab = "misclass", ylab = '', main = "proportion of correct 0 edge", type = 'n')
for (j in 1:11) {
  points(rep(misclass[j], 50), out1[,j])
}

avg0S = colMeans(out1)
lines(misclass, avg0S)



#now for non-zero edges
plot(NA, xlim = c(0, 10), ylim = c(0.8, 1), xlab = "misclass", main = "proportion of correct non-0 edge", type = 'n')
for (j in 1:11) {
  points(rep(misclass[j], 50), out2[,j])
}

avgn0S = colMeans(out2)
lines(misclass, avgn0S)







#Now for erdosRenyi

misclass = seq(0, 10, by = 1)
#we want a plot that plots each dot, and then draws a line between them
plot(NA, xlim = c(0, 10), ylim = c(0.85, 1), xlab = "misclass", ylab = '', main = "proportion of correct 0 edge", type = 'n')
for (j in 1:11) {
  points(rep(misclass[j], 50), out3[,j])
}

avg0S = colMeans(out3)
lines(misclass, avg0S)



#now for non-zero edges
plot(NA, xlim = c(0, 10), ylim = c(0.5, 1), xlab = "misclass", main = "proportion of correct non-0 edge", type = 'n')
for (j in 1:11) {
  points(rep(misclass[j], 50), out4[,j])
}

avgn0S = colMeans(out4)
lines(misclass, avgn0S)






#Okay, now the output for the EM
#simOut5[,,1] is how many fitted edges were not correct for the updated nodes
#simOut5[,,2] is how many updated edges were not correct for the updated nodes
#SimOut6[,,1] is sum of number of correctly identified edges
#simOut6[,,2] is sum of number of correctly identified edges.

misclass = seq(0, 20, by = 2)
plot(NA, xlim = c(-1, 21), ylim = c(0, 25), xlab = "misclass", ylab = '', main = "proportion of correct 0 edge", type = 'n')
for (j in 1:11) {
  points(rep(misclass[j], 100)+rnorm(100, sd = 0.5), simOut5[j,,1])
}

s51 = rowMeans(simOut5[,,1])
lines(misclass, s51)


#not sure why this is so different
misclass = seq(0, 20, by = 2)
plot(NA, xlim = c(-1, 21), ylim = c(0, 25), xlab = "misclass", ylab = '', main = "proportion of correct 0 edge", type = 'n')
for (j in 1:11) {
  points(rep(misclass[j], 100)+rnorm(100, sd = 0.5), simOut5[j,,2], pch = 24)
}

s52 = rowMeans(simOut5[,,2])
lines(misclass, s52)






#now let's do correctly identified edges
plot(NA, xlim = c(-1, 21), ylim = c(10, 60), xlab = "misclass", ylab = '', main = "proportion of correct 0 edge", type = 'n')
for (j in 1:11) {
  points(rep(misclass[j], 100)+rnorm(100, sd = 0.5), simOut6[j,,1])
}

s61 = rowMeans(simOut6[,,1])
lines(misclass, s61)


#now let's do correctly identified edges
for (j in 1:11) {
  points(rep(misclass[j], 100)+rnorm(100, sd = 0.5), simOut6[j,,2], col = "blue")
}

s62 = rowMeans(simOut6[,,2])
lines(misclass, s62)


