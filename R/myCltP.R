
#' Creates a Poisson distribution across a number of iteration of size n to show how approximations become more accurate and n increases.
#'
#' @param n size of samples
#' @param iter number of sample
#' @param lambda lambda to use for the Poission distribution
#' @param ... Any additional parameters to override
#'
#' @returns A 1,1,2,3 Plot of the Sampling mean, the rel freq of y values, and the probability distribution of y
#' @export
#'
#' @examples mycltp(n=2,iter=10000, lambda = 4)
mycltp=function(n,iter,lambda=10,...){

  ## r-random sample from the Poisson
  y=stats::rpois(n*iter,lambda=lambda)
  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w=apply(data,2,mean)
  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param=hist(w,plot=FALSE)
  ## Since the histogram will be a density plot we will find the max density

  ymax=max(param$density)
  ## To be on the safe side we will add 10% more to this
  ymax=1.1*ymax

  ## Make a suitable layout for graphing
  layout(matrix(c(1,1,2,3),nrow=2,ncol=2, byrow=TRUE))

  ## Now we can make the histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax), col=rainbow(max(w)),
       main=paste("Histogram of sample mean","\n", "sample size= ",n," iter=",iter," lambda=",lambda,sep=""),
       xlab="Sample mean",...)
  ## add a density curve made from the sample distribution
  #lines(density(w),col="Blue",lwd=3) # add a density plot
  ## Add a theoretical normal curve
  curve(stats::dnorm(x,mean=lambda,sd=sqrt(lambda/n)),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve

  # Now make a new plot
  # Since y is discrete we should use a barplot
  barplot(table(y)/(n*iter),col=rainbow(max(y)), main="Barplot of sampled y", ylab ="Rel. Freq",xlab="y" )
  x=0:max(y)
  plot(x,stats::dpois(x,lambda=lambda),type="h",lwd=5,col=rainbow(max(y)),
       main="Probability function for Poisson", ylab="Probability",xlab="y")
}
