
#----1----------------------------
Lag <- function (x, shift = 1) {
  xLen <- length(x)
  if (shift == 0)
    return(x)
  ret <- as.vector(character(xLen), mode = storage.mode(x))
  attrib <- attributes(x)
  if (length(attrib$label))
    attrib$label <- paste(attrib$label, "lagged", shift, "observations")
  if (abs(shift) < xLen) {
    if (shift > 0)
      ret[-(1:shift)] <- x[1:(xLen - shift)]
    else ret[1:(xLen + shift)] <- x[(1 - shift):xLen]
  }
  attributes(ret) <- attrib
  return(ret)
}
#--2-------------------------------------
Cs<-function (...)
as.character(sys.call())[-1]
#--3------------------------------------
elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})
#--4----------------------------------
CasesSeries<-function (t, W, start = 1, end = length(t)) 
{
  LW = length(W)
  LL = W[LW]
  JL = (end - start + 1) - LL
  I = matrix(ncol = (LW + 1), nrow = JL)
  S = start - 1
  for (j in 1:JL) {
    for (i in 1:LW) I[j, i] = t[(S + LL - W[LW - i + 1] + j)]
    I[j, (LW + 1)] = t[(S + LL + j)]
  }
  D = data.frame(I)
  N = names(D)
  LN = length(N)
  for (i in 1:(LN - 1)) N[LN - i] <- paste("lag", W[i], sep = "")
  N[LN] = "y"
  names(D) <- N
  return(D)
}
#--4--------------------------------------------------------------
cl.ind <- function(cl)
{
  n <- length(cl)
  cl <- as.factor(cl)
  x <- matrix(0, n, length(levels(cl)) )
  x[(1:n) + n*(unclass(cl)-1)] <- 1
  dimnames(x) <- list(names(cl), levels(cl))
  x
}
#---5-----------------------------------------
decodeClassLabels<-function (x, valTrue = 1, valFalse = 0) 
{
  n <- length(x)
  x <- as.factor(x)
  res <- matrix(valFalse, n, length(levels(x)))
  res[(1:n) + n * (unclass(x) - 1)] <- valTrue
  dimnames(res) <- list(names(x), levels(x))
  res
}
#---6-----------------------------------------
encodeClassLabels<-function (x, method = "WTA", l = 0, h = 0) 
{
  apply(x, 1, function(y) 
    analyzeClassification(y, method,l, h))
}
#----7----------------------------------------
analyzeClassification<-function (y, method = "WTA", l = 0, h = 0) 
{
  classes <- length(y)
  resClass <- 0
  if (method == "402040") {
    candClass <- which(y >= h)
    if (length(candClass) == 1) {
      if (max(y[-candClass]) <= l) {
        resClass <- candClass
      }
    }
  }
  else if (method == "WTA") {
    candClass <- which(y == max(y))
    if (length(candClass) == 1) {
      if (y[candClass] > h) {
        if (max(y[-candClass]) < (max(y) - l)) {
          resClass <- candClass
        }
      }
    }
  }
  resClass
}
#---8-----------------------------------------
which.is.max<-function (x) 
{
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L) 
    sample(y, 1L)
  else y
  
}
#---9--------
#require(kohonen)
classvec2classmat <- function (yvec) 
{
  yvec <- factor(yvec)
  nclasses <- nlevels(yvec)
  outmat <- matrix(0, length(yvec), nclasses)
  dimnames(outmat) <- list(NULL, levels(yvec))
  for (i in 1:nclasses) outmat[which(as.integer(yvec) == i), i] <- 1
  outmat
}
#---10-------------------
classmat2classvec <- function(ymat, threshold=0.5)
{
  class.names <- dimnames(ymat)[[2]]
  if (is.null(class.names)) 
    class.names <- 1:ncol(ymat)
  classes <- apply(ymat, 1, function(x) which(x == max(x))[1])
  classes[apply(ymat, 1, max) < threshold] <- NA
  class.names[classes]
}


