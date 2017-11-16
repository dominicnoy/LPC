#off diagonal function
off_diag<-function(N){
  mat<-matrix(1, nrow=N, ncol=N)
  # A companion matrix that indicates how "off" a diagonal is:
  delta <- row(mat) - col(mat)
  # Set these to select on the "delta" matrix
  low <- -1
  high <- 1
  # Operate on the "mat" matrix
  mat[delta < low | delta > high] <-0
  diag(mat)<-0
  mat}