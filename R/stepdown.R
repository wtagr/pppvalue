#' Single-step/Step-down adjusted p-values
#' 
#' Estimate the single-step/step-down adjusted p-values (internal function).
#' 
#' @param realvec A vector of observed data.
#' @param permat A matrix of simulated/permuted data, the \code{nrow} of which must be the same as the length of 
#' \code{realvec} and the \code{ncol} of which denotes the number of the simulation or permutation.
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{p1}{Estimated single-step adjusted p-values}
#' \item{p2 }{Estimated step-down adjusted p-values}
#' @author Meng Xu, Philip Reiss
#' @seealso \code{\link{pppvalue}}
#' @keywords internal
#' 
stepdown <-
function(realvec, permat) {
  allmat <- cbind(realvec, permat)
  ntest <- nrow(allmat)
  B <- ncol(allmat)
  rankmat <- allmat
  for (i in 1:ntest) rankmat[i, ] <- pmin(rank(allmat[i,]), rank(-allmat[i,]))
  maxk <- max(rankmat[,1])
  # print(rankmat[,1])
  # cat("max rank", maxk, '\n')
  rb <- apply(as.matrix(rankmat[,-1]),2,min)
  pk1 <- pk2.init <- c()
  for (k in 1:maxk) {
    pk1[k] <- (sum(rb<=k)+1) / B
    Sk <- which(rankmat[,1] >= k)
    rbs <- apply(as.matrix(rankmat[Sk,-1]),2,min)
    pk2.init[k] <- (sum(rbs<=k)+1) / B
  }
  # print(pk2.init)
  pk2 <- cummax(pk2.init)
  p1 <- p2 <- rep(NA, ntest)
  for (k in 1:maxk) {
    p1[rankmat[,1]==k] <- pk1[k]
    p2[rankmat[,1]==k] <- pk2[k]
  }
  data.frame(p1=p1, p2=p2)
}
