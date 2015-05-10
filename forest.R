#rm(list=ls()) # here as a debugging precaution
source('trees.R')

# generic function for decision forests
decisionForest <- function(x, ...) UseMethod("decisionForest")

decisionForest.default <- function(x, y, ntrees=100, maxdepth=5, numphi=2, numtau=2, ...) {
   forest.obj <- list()
   trees <- list()
   for (i in 1:ntrees) {
     trees[[length(trees) + 1]] <- decisionTree(x, y, maxdepth=maxdepth, numphi=numphi, numtau=numtau, ...)
   }

   forest.obj$trees <- trees
   forest.obj$y <- y
   forest.obj$levels <- levels(y)
   length(forest.obj) <- ntrees
   class(forest.obj) <- "decisionForest"
   return(forest.obj)
}

levels.decisionForest <- function(obj) {
  return(levels(obj$y))
}

predict.decisionForest <- function(object, newdata, ...) {
  probs = matrix(0, nrow(newdata), length(levels(object$y)))

  for (t in 1:length(object)){
    # 1 predict in tree
    p = predict(object$trees[[t]], newdata)
    probs = probs + p
  }
  probs = probs/length(object)

  print(p)
  return(probs)
}


