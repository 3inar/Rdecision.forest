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

}
