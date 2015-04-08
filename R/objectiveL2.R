objectiveL2 <-
function(x,T,phi,y,lambda) {
  X  <- T %*% x
  OF <- norm( phi %*% X - y , c("F") ) ^ 2 + lambda * norm( X, c("F")) ^ 2
 return (OF)
}

