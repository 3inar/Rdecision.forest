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
    splits = runif(20, min(data[,predictor]), max(data[,predictor]))

    for (t in splits) {
      leftIdx = data[,predictor] < t
      if (sum(leftIdx) == 0 || sum(!leftIdx) == 0) { next }
      split.impurity = evaluateSplit(labels[leftIdx], labels[!leftIdx])

      if (split.impurity < lowestImpurity) {
        splitval = t
        splitvar = predictor
        lowestImpurity = split.impurity
      }
    }
  }

  list(predictor=splitvar, threshold=splitval, impurity=lowestImpurity)
}

evaluateSplit = function (A, B) {
  Na = length(A)/(length(A) + length(B))
  Nb = length(B)/(length(A) + length(B))
  impurity(A)*Na + impurity(B)*Nb
}

# the weights are used for stratified sampling of predictors
newTree = function(X, Y, max.depth=16) {
  if (max.depth > 25)
      stop("This implementation can't handle super-deep trees, max is 25")

  tree = new.env(parent=globalenv()) # this is a hack that allows pass by reference
  tree$classes = sort(unique(Y))
  tree$nodes=list()
  length(tree$nodes) = 2^max.depth
  tree$max.depth = max.depth

  class(tree) = "decision.tree"
  tree = growTree.recursive(X, Y, tree, 1)

  # make the tree into a solid pas-by-value object again
  ret = list(max.depth = tree$max.depth, classes=tree$classes, nodes=tree$nodes)
  return(ret)
}

leftChild = function(index) { 2*index  }
rightChild = function(index) { 2*index + 1 }
parent = function(index) { floor(index/2) }
depth = function(index) { floor(log2(index)) }

growTree.recursive = function(X, Y, tree, index) {
  minimumSize = 2 # stopping criterion
  node = list(index = index, terminal = F)

  hist = NULL
  for (class in tree$classes) {
    hist = c(hist, sum(Y==class))
  }
  #print(hist)
  node$histogram = hist
  class(node) = "tree.node"

  # various reasons to terminate
  if (length(Y) <= minimumSize + 1 || depth(index) == tree$max.depth || impurity(Y) == 0) {
    node$terminal = T
    tree$nodes[[index]] = node
    return(tree)
  }

  split = split.axis(X, Y)
  node$predictor = split$predictor
  node$threshold = split$threshold
  left = X[, node$predictor] < node$threshold

  tree$nodes[[index]] = node
  tree = growTree.recursive(X[left,], Y[left], tree, leftChild(index))
  tree = growTree.recursive(X[!left,], Y[!left], tree, rightChild(index))

  return(tree)
}

growTree.experimental = function(X, Y, tree, index) {
  minimumSize = 2 # stopping criterion
  node = list(index = index, terminal = F)
  n.internals = 2^tree$max.depth
  term.flag = length(Y) + 1
  subsets = Matrix(0, n.internals, length(Y)+1, sparse=T)
  weak.learners = Matrix(0, n.internals, 2, sparse=T)

  eval = 1
  subsets[1, ] = c(rep(T, length(Y)), 0)
  for (i in 1:n.internals) {
    #print(eval)
    if (eval == 0) break

    ss = as.logical(subsets[i, ])
    if (!any(ss)) {
      #print(i)
      next
    }
    ss = ss[-term.flag]
    X.current = X[ss, ]
    Y.current = Y[ss]


    # various reasons to terminate this branch
    if (sum(ss) <= minimumSize + 1 ||  depth(i) == tree$max.depth || impurity(Y.current) == 0 ) {
      subsets[i, term.flag] = T # mark node as terminal
      eval = eval-1
      next
    }

    split = split.axis(X.current, Y.current)
    weak.learners[i, ] = c(split$predictor, split$threshold)

    left = X[, split$predictor] < split$threshold
    right = !left
    subsets[leftChild(i), ] = c((left & ss), 0)
    subsets[rightChild(i), ] = c((right & ss), 0)
    eval = eval+1
  }

  return(tree)
}
