##############################################################################
## Functionality for growing a tree, this will be a super-bare-bones CART
##
## Author: Einar H
##############################################################################
source('weaklearner.R')

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

leftChild = function(index) { 2*index  }
rightChild = function(index) { 2*index + 1 }
parent = function(index) { floor(index/2) }
depth = function(index) { floor(log2(index)) }

decisionTree <- function(x, ...) UseMethod("decisionTree")

decisionTree.default = function(X, Y, maxdepth=16, ...) {
  if (maxdepth > 25)
      stop("This implementation can't handle super-deep trees, max is 25")

  tree = new.env(parent=globalenv()) # this is a hack that allows pass by reference
  tree$classes = sort(unique(Y))
  tree$nodes=list()
  length(tree$nodes) = 2^maxdepth
  tree$maxdepth = maxdepth

  tree = growTree.recursive(X, Y, tree, 1, ...)

  # make the tree into a solid pass-by-value object again
  ret = list(maxdepth = tree$maxdepth, classes=tree$classes, nodes=tree$nodes, y=Y)
  class(ret) = "decisionTree"
  return(ret)
}

predict.decisionTree <- function(object, newdata) {
  # find the correct leaf nodes 1st
  N = nrow(newdata)
  m = length(levels(object$y))
  probs = matrix(0, N, m)
  rownames(probs) <- rownames(newdata)
  colnames(probs) <- levels(object$y)

  metadata=list(bitmap=rep(T, N), nodelist=rep(-1, N))
  metadata=findLeaf.recursive(newdata, object, 1, metadata)

  indexes=metadata$nodelist
  for (idx in unique(indexes)) {
    n = object$nodes[[idx]]
    hist = table(n$y)

    if(length(hist) != length(levels(object$y)))
      print(hist)
    p <- hist/sum(hist)

    if (!all(probs[indexes==idx, ] == 0)) {
      print("sheit")
    }
    for (i in 1:length(p)) {
      probs[indexes == idx, i] <- p[i]
    }
    #debug
#     print(probs)
#     print(p)
#     break

    # debug
#     if(sum(p) > 1) {
#       print(hist)
#       print(sum(hist))
#     }
  }

  return(probs)
}

# returns leaf node index for each data pt
findLeaf.recursive <- function(x, tree, index, metadata) {
  # if no samples made it here, don't do anything
  if (!any(metadata$bitmap))
    return(metadata)

  this.node <- tree$nodes[[index]]
  if (this.node$terminal) {
    # mark all the samples that ended up in this leaf (TRUE in bitmap)
    # with the index of this leaf
    metadata$nodelist[metadata$bitmap] = index
    return(metadata)
  }

  left = predict(this.node$learner, x)
  bitmap.left = metadata$bitmap & left
  bitmap.right = metadata$bitmap & !left

  # recurse left!
  md.left = list(bitmap=bitmap.left, nodelist=metadata$nodelist)
  metadata = findLeaf.recursive(x, tree, leftChild(index), md.left)

  # recurse right!
  md.right = list(bitmap=bitmap.right, nodelist=metadata$nodelist)
  metadata = findLeaf.recursive(x, tree, rightChild(index), md.right)

  return (metadata)
}

growTree.recursive = function(X, Y, tree, index) {
  #minimumSize = 3 # stopping criterion
  node = list(index = index, terminal = F)

#   hist = NULL
#   for (class in tree$classes) {
#    hist = c(hist, sum(Y==class))
#   }
#   print(length(Y))
#   print(hist)
  #node$histogram = hist
  class(node) = "tree.node"

  # various reasons to terminate
  #if (length(Y) <= minimumSize || depth(index) == tree$max.depth || impurity(Y) == 0) {
  if (depth(index) == tree$maxdepth || impurity(Y) == 0) {
    node$terminal = T
    node$y = Y
    tree$nodes[[index]] = node
    return(tree)
  }

  learner = axisAligned(X, Y)
  left = fitted(learner)

  node$learner = learner
  tree$nodes[[index]] = node
  tree = growTree.recursive(X[left,], Y[left], tree, leftChild(index))
  tree = growTree.recursive(X[!left,], Y[!left], tree, rightChild(index))

  return(tree)
}

