# Calculate mean centering (anomaly) matrix
# Input: design matrix
# Output: mean centered design matrix
# Calculation: X_anomaly = (I_n - 1_n %*% t(i_n)) %*% X
center_matrix <- function(x) {
  return(x - rep(1, nrow(x)) %*% t(colMeans(x)))
}

# center_colmeans <- function(x) {
#   xcenter = colMeans(x)
#   x - rep(xcenter, rep.int(nrow(x), ncol(x)))
# }

mat <- matrix()
center_matrix(mat)
v1 <- 
v2 <- 
mat <- cbind(c(1,2,3,4,5,6,7,8,9,10),
             c(2,4,6,8,10,12,14,16,18,20))
center_matrix(mat)
