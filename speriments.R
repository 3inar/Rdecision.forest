rm(list=ls()) # here as a debugging precaution
source('grow.R')

#some test data
data(iris)
labels <- which(names(iris) %in% c("Species"))
X <- iris[, -labels]
y <- iris[, labels]

# generic function for decision forests
decisionForest <- function(x, ...) UseMethod("decisionForest")

decisionForest.default <- function(x, y, ntrees=100, maxdepth=5, ...) {
   forest.obj <- list()
   trees <- list()
   for (i in 1:ntrees) {
     trees[[length(trees) + 1]] <- newTree(x, y, maxdepth=maxdepth)
   }

   forest.obj$trees <- trees
   length(forest.obj) <- ntrees
   class(forest.obj) <- "decisionForest"
   return(forest.obj)
}

predict.decisionForest <- function(object, newdata, type="class", ...) {
  # ddd
  if (type=="class") {
    for (t in 1:length(object)){
      # 1 predict in tree
      print(t)
    }
    # 2 combine predictions
  } else if (type=="prob") {
    stop("not yet implemented")
  } else {
    stop("type unrecognized")
  }
}

#Rprof()
#decisionForest(X, y)
#Rprof(NULL)
#summaryRprof("Rprof.out")
df = decisionForest(X, y)
predict(df, NULL)

# r = rep(0, 1000)
# res = rep(0,1000)
# for (i in 1:1000) {
#   res[i] = entropy(table(r))
#   r[i] = 1
# }
#ineq()
