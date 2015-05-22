#rm(list=ls()) # here as a debugging precaution
source('trees.R')

# generic function for decision forests
decisionForest <- function(x, ...) UseMethod("decisionForest")

decisionForest.default <- function(x, y, ntrees=100, maxdepth=5, numphi=2, numtau=2, trace=F, ...) {
   forest.obj <- list()
   trees <- list()
   for (i in 1:ntrees) {
     if (trace) print(paste0("tree num.: ", as.character(i), " of ", as.character(ntrees)))
     trees[[length(trees) + 1]] <- decisionTree(x, y, maxdepth=maxdepth, numphi=numphi, numtau=numtau, ...)
   }

   forest.obj$trees <- trees
   forest.obj$y <- y
   forest.obj$maxdepth <- maxdepth
   forest.obj$ntrees <- ntrees
   forest.obj$numphi <- numphi
   forest.obj$numtau <- numtau

   class(forest.obj) <- "decisionForest"
   return(forest.obj)
}

levels.decisionForest <- function(obj) {
  return(levels(obj$y))
}

predict.decisionForest <- function(object, newdata, classes=F, ...) {
  probs = matrix(0, nrow(newdata), length(levels(object$y)))

  for (t in 1:object$ntrees){
    # 1 predict in tree
    p = predict(object$trees[[t]], newdata)
    probs = probs + p
  }
  probs = probs/object$ntrees

  if (classes) {
    cls <- max.col(probs)
    cls <- as.factor(cls)
    levels(cls) <- levels(object$y)
    return(cls)
  }
  return(probs)
}


