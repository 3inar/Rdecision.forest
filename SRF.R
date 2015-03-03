##############################################################################
## Stratified RF test implementation
##
## Author: Einar H
##############################################################################

# for calculating discriminant projection
require(MASS)

# some data to work with, will add 4 dimensions of noise to the iris data set
data(iris)
data = iris
samples = nrow(data)
random = cbind(rnorm(samples), rnorm(samples), rnorm(samples), rnorm(samples))
colnames(random) = c("R1", "R2", "R3", "R4")
data = cbind(random, data)

# step i) compute informativeness of each feature
lda.fit = lda(Species~., data=data)
weights.abs = abs(lda.fit$scaling[,1]) # use the weights from the best LD direction
weights.abs = weights.abs/sum(weights.abs) # normalize

plot(weights.abs, xlab=colnames(data)) # as expected the noise features (first 4) have low scores
axis(3, labels=names(weights.abs), at=1:length(weights.abs))

# step ii) define a cutoff for the "strong" group; Ye & al. use the mean of the weights
cutoff = mean(weights.abs)
strong = weights.abs > cutoff # will correctly identify the noise features

# step iii) generate bootstrap samples for your trees
# step iv)
