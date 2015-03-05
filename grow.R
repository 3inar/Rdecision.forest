##############################################################################
## Functionality for growing a tree, this will be a super-bare-bones CART
##
## Author: Einar H
##############################################################################

# retuns the impurity of a node; labels are simply the labels in the node
impurity = function (labels, measure = "gini") {
  # Impurity measures from Hastie & al. "Elements of Statistical Learning"
  # not implemented: misclassification error
  if (measure == "gini") {
    m = function (p.hat) { p.hat*(1-p.hat) }
  } else if (measure == "entropy") {
    m = function (p.hat) {-p.hat*log(p.hat) }
  } else {
    stop("no such impurity measure")
  }

  classes = unique(labels)
  num.class = length(classes)
  measures = rep(1, num.class)

  for (i in 1:num.class) {
    p.hat = sum(labels==classes[i])/length(labels)
    measures[i] = m(p.hat)
  }

  sum(measures)
}

# small test to check whether the impurity measure acts as expected
# test = rep(0, 100)
# res = rep(-1, 100)
# for (i in 1:100) {
#   test[i] = 1
#   res[i] = impurity(test, measure="entropy")
# }
# plot(res)

# axis-aligned split f'n. Assumes the feature subspace has already been chosen
split.axis = function (data, labels) {
  classes = unique(labels)
  lowestImpurity = .Machine$double.xmax  # biggest float there is
  splitval = NA
  splitvar = 0

  for (predictor in 1:ncol(data)) {
    # pick  a handful of potential split points
    # NB that you can inject randomness into the model by adjusting the n.o
    # random splits you evaluate
    splits = runif(50, min(data[,predictor]), max(data[,predictor]))

    for (t in splits) {
      leftIdx = data[,predictor] < t
      impurity = evaluateSplit(labels[leftIdx], labels[!leftIdx])

      if (impurity < lowestImpurity) {
        splitval = t
        splitvar = predictor
        lowestImpurity = impurity
      }
    }
  }

  list(predictor=splitvar, threshold=splitval, impurity=lowestImpurity)
}

evaluateSplit = function (A, B) {
  impurity(A)/length(A) + impurity(B)/length(B)
}

growTree = function(X, Y) {
  tree = list(classes = sort(unique(Y)))
  class(tree) = "decision.tree"
  tree = growNode(X, Y, tree, 1)
  tree
}

leftChild = function(index) { 2*index  }
rightChild = function(index) { 2*index + 1 }
parent = function(index) { floor(index/2) }

growNode = function(X, Y, tree, index) {
  #minimumSize = 2 # stopping criterion
  node = list(index = index, terminal = F)
  class(node) = "tree.node"
  if (index >= 20) {
    node$terminal = T
    tree$nodes[[index]] = node
    return(tree)
  }

  tree$nodes[[index]] = node
  tree = growNode(X,Y,tree, leftChild(index))
  tree = growNode(X,Y,tree, rightChild(index))

  tree
}
