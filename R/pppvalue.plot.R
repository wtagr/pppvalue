#' Plot Pointwise Adjusted Permutation P-values (PPPvalue)
#' 
#' Plot the object of the function \code{pppvalue} basically.
#' 
#' @param pvalues Output of \code{pppvalue} function.
#' @param xind A vector of the coordinates of points.
#' @param ylab A title for the y axis.
#' @param color The color of points or lines appearing in the plot.
#' @param legend logical; if \code{TRUE} the legend is shown and its position currently is fixed at \code{"bottomright"}.
#' @param type What type of plot should be drawn. \code{"o"} is default.
#' @param lty The line types for lines appearing in the legend.
#' @param pch The plotting symbols appearing in the plot.
#' @param ... Other arguments to be passed to \code{plot} function
#' @return Figure of pointwise adjusted p-values
#' @author Meng Xu \email{mxu@@campus.haifa.ac.il}
#' @export
#' @import grDevices graphics
#' @references 
#' Xu, M., and Reiss, P. T. (2019). Distribution-Free Pointwise Adjusted P-Values for Functional Hypotheses. \url{https://arxiv.org/abs/1912.00360}.
#' @seealso \code{\link{pppvalue}}
#' @examples
#' \dontrun{
#' set.seed(123)
#' library(pppvalue)
#' obs <- rnorm(20,mean=2, sd=2)
#' perms <- matrix(rnorm(20*999),20)
#' pvals <- pppvalue(obs, perms)
#' pppvalue.plot(pvals)
#' }
pppvalue.plot <-
  function(pvalues, xind = NULL, ylab = NULL, color = NULL, type = NULL, lty = NULL,
           pch = NULL, legend = TRUE, ...){
  method = attr(pvalues, "method")
  args=list(...)
  type = if(is.null(type)) "o" else type
  if (method %in% c("single-step", "step-down", "ERL")){
    col = if (is.null(color)) 1 else color
    ylab = if (is.null(ylab)) paste(method, "adjusted p-values") else ylab
    lty = if(is.null(lty)) 1 else lty
    pch = if(is.null(pch)) 16 else pch
    if (is.null(xind)) {
      plot(pvalues, type = type, pch = pch, ylab = ylab, lty=lty,
           col = col, ...)
      if (legend) legend("bottomright", pch = pch,legend = method, lty = lty, col = col)
    } else {
        plot(x = xind, y = pvalues, type = type, pch = pch, ylab = ylab,  lty=lty,
             col = col, ...)
      if (legend) legend("bottomright", legend = method, pch = pch, lty = lty, col = col)
      }
  }else if (method == 'all'){
    pvalues = t(pvalues)
    ylab = if (is.null(ylab)) paste( "adjusted p-values") else ylab
    col = if (is.null(color)) c(1,gray(0.3),gray(0.7)) else color
    legend.txt = c("single-step", "step-down","ERL")
    lty = if (is.null(lty)) 1 else lty
    pch = if(is.null(pch)) 16:18 else pch
    if (is.null(xind)) {
      matplot(pvalues, type = type, pch = pch, ylab = ylab, lty=lty,
              col = col, ...)
      if (legend) legend("bottomright",legend = legend.txt, pch = pch, lty=lty, col = col)
    } else {
      matplot(x = xind, y = pvalues, type = type, pch = pch, ylab = ylab, lty=lty, 
              col = col,...)
      if (legend) legend("bottomright",legend = legend.txt, pch = pch, lty=lty,, col = col)
    }
  }else {stop("The argument of pvalues must be the output of the pppvalue function.")}
  }

