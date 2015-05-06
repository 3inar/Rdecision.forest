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
