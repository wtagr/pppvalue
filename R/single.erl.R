#' ERL-adjusted p-values
#' 
#' Estimate the ERL adjusted p-values (internal function).
#' 
#' @param realvec A vector of observed data.
#' @param permat A matrix of simulated/permuted data, the \code{nrow} of which must be the same as the length of 
#' \code{realvec} and the \code{ncol} of which denotes the number of the simulation or permutation.
#' @return Estimated ERL-adjusted p-values.
#' @author Meng Xu
#' @seealso \code{\link{pppvalue}}
#' @keywords internal
#' 
single.erl <-
function(realvec, permat) {
  allmat <- cbind(realvec, permat)
  ntest <- nrow(allmat)
  B <- ncol(allmat)
  loranks <- t(apply(allmat, 1, rank)) #ties.method = "max"
  hiranks <- B + 1 - loranks
  rankmat <- pmin(loranks, hiranks)
  
  sortranks <- apply(rankmat, 2, sort)
  lexo_values <- do.call("order", split(sortranks, row(sortranks)))
  newrk<-1:B
  erlrk<-newrk[order(lexo_values)]
  
  maxk <- B-1
  env.erl<-matrix(NA,maxk,ntest)
  for (k in 1:maxk) {
    rm<- which(erlrk<=k)
    upb <- apply(as.matrix(allmat[,-rm]),1,min)
    lowb <- apply(as.matrix(allmat[,-rm]),1,max)
    env.erl[k,]<-allmat[,1]>=upb&allmat[,1]<=lowb
  }
  env.erl<-rbind(rep(TRUE,ntest),env.erl)
  # res<-(apply(env.erl,2, function(x) which(duplicated(x)==F)[2])-1)/B
  res=colSums(env.erl)/B
  res[is.na(res)]=1
  
  p.raw=c()
  for(i in 1:ntest) p.raw[i]<-sum(rankmat[i,]<=rankmat[i,1])/B
  
  attr(res,"p.erl")=erlrk[1]/B
  attr(res, "p.raw")=p.raw
  return(res)
}
