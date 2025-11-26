#' Bootstrap Confidence Interval
#'
#' Calculates bootstrap confidence intervals and displays a histogram.
#'
#' @param iter Number of bootstrap iterations (default 10000)
#' @param x Numeric vector of sample data
#' @param fun Statistic to calculate (default "mean")
#' @param alpha Significance level (default 0.05)
#' @param cx Text size for plot labels (default 1.5)
#' @param ... Additional arguments passed to hist()
#'
#' @return List with confidence interval (ci), function used (fun), and data (x)
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30, mean=50, sd=10)
#' myboot2(x=x, fun="mean")
#'
#' @importFrom stats quantile
#' @importFrom graphics segments text
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){
  n=length(x)

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))

  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))
}
