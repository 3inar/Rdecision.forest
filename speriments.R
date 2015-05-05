rm(list=ls()) # here as a debugging precaution
source('grow.R')

#some test data
data(iris)
labels <- which(names(iris) %in% c("Species"))
X <- iris[, -labels]
y <- iris[, labels]

decisionForest <- function(x, ...) UseMethod("decisionForest")

decisionForest.default <- function(x, y, ntrees=100, ...) {
   forest.obj <- list()
   trees = list()
   for (i in 1:ntrees) {
     print(i)
     trees[[length(trees) + 1]] <- newTree(x, y)
   }

   forest.obj$trees <- trees
   class(forest.obj) <- "decisionForest"
   return(forest.obj)
}

#Rprof()
#decisionForest(X, y)
#Rprof(NULL)
#summaryRprof("Rprof.out")
print(system.time(decisionForest(X, y)))
