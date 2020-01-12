#' Pointwise Adjusted Permutation P-values (PPPvalue)
#' 
#' Perform three distribution-free adjustments for pointwise p-values based on envelope/graphical tests. The current version is made available for testing.
#' 
#' There are three distribution-free p-value adjustments offered in this package: 
#' \itemize{
#' \item \code{'single-step'}: the single-step adjusted p-values with respect to the GET-package;
#' \item \code{'step-down'}: the step-down adjusted p-values with respect to the step-down 
#' procedure (Westfall and Young, 1993; Ge et al., 2003; Romano and Wolf, 2016);
#' \item \code{'ERL'}: the adjusted p-values based on envelopes of the extreme rank length (ERL).}
#' Please see Xu and Reiss (2019) for more details.
#' @param realvec A vector of observed data.
#' @param permat A matrix of simulated/permuted data, the \code{nrow} of which must be the same as the length of 
#' \code{realvec} and the \code{ncol} of which denotes the number of the simulation or permutation.
#' @param method A charactor option specifying three adjustments or all of these, for \code{'single-step'}, \code{'step-down'},
#'  \code{'ERL'} and \code{'all'}. The default is \code{'all'}. See details.
#'  
#' @return Estimated pointwise adjusted p-values.
#' @note 
#' \itemize{
#' \item \code{'ERL'} and \code{'all'} also offer the pointwise raw p-values w.r.t ranks and the p-value of GET in the default setting;
#' \item The \code{rank} adopts the default tie.method;
#' \item These adjustments would not consider the ties situations currently.}
#' @author Meng Xu \email{mxu@@campus.haifa.ac.il}, Philip Reiss
#' @export
#' @references 
#' Xu, M., and Reiss, P. T. (2019). Distribution-Free Pointwise Adjusted P-Values for Functional Hypotheses. \url{https://arxiv.org/abs/1912.00360}.
#' @seealso The plot function \code{\link{pppvalue.plot}}. Other useful resources on Global Evelope Test refer to the GET package (\code{\link[GET]{global_envelope_test}}).
#' @examples
#' \dontrun{
#' set.seed(123)
#' library(pppvalue)
#' obs <- rnorm(20,mean=2, sd=2)
#' perms <- matrix(rnorm(20*999),20)
#' pvals <- pppvalue(obs, perms)
#' pppvalue.plot(pvals)
#' }
pppvalue <-
function(realvec, permat, method = "all"){
  if (method %in% c("step-down","single-step")){
    res1 <- stepdown(realvec, permat)
    if (method == "step-down") {
      res=res1$p2
      attr(res,"method") = "step-down"
      return(res)
    } else {
      res=res1$p1
      attr(res,"method") = "single-step"
      return(res)
      }
  }else if (method == "ERL") {
    res2 <- single.erl(realvec, permat)
    attr(res2,"method") = "ERL"
    return(res2)
  }else if (method == "all"){
    res1 <- stepdown(realvec, permat)
    res2 <- single.erl(realvec, permat)
    res <- rbind(res1$p1,res1$p2,res2)
    rownames(res)<-c("single-step","step-down","ERL")
    attr(res,"p.raw") = attr(res2,"p.raw")
    attr(res,"p.erl") = attr(res2,"p.erl")
    attr(res,"method") = "all"
    return(res)
  }else{stop("The argument of method must be 'single-step', 'step-down', 'ERL' or 'all'.")}
}
