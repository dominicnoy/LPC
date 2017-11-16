#Matrix inverse
#compute inverse of invertible square-root of Lambda, called Y
matrix_inverse<-function(LAMBDA){
E <- eigen(LAMBDA)
V <- E$values
Q <- E$vectors
Y <- Q%*%diag(1/sqrt(V))%*%t(Q)
Y
}