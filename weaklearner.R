# helper function
impurity = function (labels, measure="gini") {
  # Impurity measures from Hastie & al. "Elements of Statistical Learning"
  # not implemented: misclassification error
  if (measure == "gini") {
    m = function (p.hat) { p.hat*(1-p.hat) }
  } else if (measure == "entropy") {
    m = function (p.hat) {-p.hat*log(p.hat) }
  } else {
    stop("no such impurity measure")
  }

  t = table(labels)
  measures = rep(1, length(t))

  for (i in 1:length(t)) {
    p.hat = t[i]/sum(t)
    measures[i] = m(p.hat)
  }

  sum(measures)
}

# helper function
evaluateSplit = function (A, B) {
  Na = length(A)/(length(A) + length(B))
  Nb = length(B)/(length(A) + length(B))
  impurity(A)*Na + impurity(B)*Nb
}

# generic function for axis-aligned learner
axisAligned <- function(x, ...) UseMethod("axisAligned")

# Prediction will return left:T/F
axisAligned.default <- function(x, y, numphi=1, numtau=1, ...) {
  classes = unique(y)
  lowestImpurity = .Machine$double.xmax  # biggest float there is
  splitval = NA
  splitvar = 0
  bestfitted = NA
  if (nrow(x) < 2)
    print("can't split only one point!")

  candidates = sample(1:ncol(x), numphi)
  for (predictor in candidates) {
    # pick  a handful of potential split points
    # NB that you can inject randomness into the model by adjusting the n.o
    # random splits you evaluate

    # if you're unlucky you might have picked a predictor that's constant,
    # this will mess you up big time
    while (min(x[, predictor]) == max(x[,predictor])) {
      predictor = sample(1:ncol(x), 1)
    }

    splits = runif(numtau, min(x[, predictor]), max(x[, predictor]))

    for (t in splits) {
      leftIdx = x[,predictor] < t

      split.impurity = evaluateSplit(y[leftIdx], y[!leftIdx])

      if (split.impurity < lowestImpurity) {
        splitval = t
        splitvar = predictor
        lowestImpurity = split.impurity
        bestfitted = leftIdx
      }
    }
  }
  suppressWarnings(if (is.na(bestfitted)) { print("trouble") })


  al = list(predictor=splitvar, threshold=splitval, impurity=lowestImpurity)
  al$fitted.val = bestfitted
  class(al) <- "axisAligned"
  return(al)
}
