#' @export
#' @return returns the Correlation, as well as bootstrap estimation of correlation(BootstrapCor), standard error(StandardError), bias(Bias) and confidence interval(CI).
#' @title Bootstrap Estimator
#' @usage bootstrap(n=200, x, y, plot=FALSE)
#'
#' @keywords bootstrap estimation correlation standard error bias confidence interval
#'
#' @description This function takes 3 arguments to estimate the correlation between them, as well as the bootstrap estimation of the correlation, standard error, bias and confidence interval, using these arguments.
#'
#' @import graphics stats
#' @param n is a positive integer that specifies how many bootstrap replicates should be used for estimation.
#' @param x is a vector of numeric values to be used with y to estimate correlation and bootstrap estimates.
#' @param y is a vector of numeric values to be used with x to estimate correlation and bootstrap estimates. y has to be of the same length as x.
#' @param plot is a boolean value that specifies whether a plot should be created or not
#' @author Emil H. Andersen \cr
#' Department of Mathematics and Computer Science (IMADA) \cr
#' University of Southern Denmark, Denmark \cr
#' \email{emila14@student.sdu.dk} \cr
#'
#' @examples
#' #Add examples for the function to explain to others how they can be used
#' bootstrap(n=1000, x=rnorm(30),y=rnorm(30), plot = TRUE)

bootstrap <- function(n=200,x,y, plot=FALSE){
  #Syntax Processing
  #============================================================
  xIsVector <- TRUE #Assume that x is a vector until it is not
  yIsVector <- TRUE #same for y
  if(is.matrix(n) || is.list(n) || length(n)!=1){ #Check that n is a single value
    stop("'n' has to be a positive integer; length=1, non-list, non-matrix")
  } else if(!is.numeric(n)){ #checks if n is a number
    stop("'n' has to be a positive integer")
  } else if(n != abs(round(n))){ #checks if n is a natural number
    stop("'n' has to be a positive integer")
  }
  #Now we check that x is a vector, which requires us to check that it is not a
  #matrix, or list. We also check that all content in x is numeric.
  if(!is.vector(x) || is.matrix(x) || is.list(x) || !is.numeric(x)){
    stop("'x' has to be a vector of numbers.(Non list, non matrix)")
    xIsVector <- FALSE
  }
  #We check the same for y
  if(!is.vector(y) || is.matrix(y) || is.list(y) || !is.numeric(y)){
    stop("'y' has to be a vector of numbers.(Non list, non matrix)")
    yIsVector <- FALSE
  }
  if(xIsVector == TRUE && yIsVector == TRUE){ #If either x or y isnt a vector, we cant check this
    if(length(x) != length(y)){ #We check if x and y has same length
      stop("'x' and 'y' has to be of the same length.")
    }
  }
  if(!is.logical(plot) || length(plot) != 1){ #We check that plot is logical and a single value
    stop("'plot' has to be a TRUE or FALSE statement.")
  }
  #------------------------------------------------------------
  correlation <- cor(x,y) #First get the true correlation
  N <- length(x) #we set N
  storage <- numeric(n) #We make space for n correlation estimates
  for (i in 1:n){
    j <- sample(1:N, size = N, replace = TRUE) #This sample is just a list of "slots" we want to take from in x and y
    xSample <- x[j] #Sample from x
    ySample <- y[j] #Sample from y
    storage[i] <- cor(xSample,ySample) #Estimate the correlation
  }
  BootstrapCor <- mean(storage) #We get the mean of the estimates
  se <- sd(storage) #standard deviation of correlations is in fact standard error
  bias <- mean(storage) - correlation #Estimate bias
  alpha <- 0.05 #alpha is set for a 95% confidence interval
  value <- round(qnorm(1-alpha/2),digits=2) #"value" is z
  CI <- c(BootstrapCor - value*se, BootstrapCor + value*se) #Confidence interval given in a vector
  #We save a list of returned values to easily work with if user is interested in specific values
  returnedList <- list("TrueCor" = correlation, "BSCor" = BootstrapCor, "SE" = se, "Bias" = bias, "CI" = CI)
  if(plot == TRUE){ #If user wants a histogram.
    #Breaks are set in the smallest possible interval to optimize presentation
    hist(storage, breaks=seq(round(min(storage),digits=2)-0.01,round(max(storage),digits=2)+0.01,0.01), main="Histogram of correlations", xlab="correlations")
    abline(v = correlation, col="red") #True correlation
    abline(v = CI[1], col="blue", lty=2) #Confidence interval 1
    abline(v = CI[2], col="blue", lty=2) #Confidence interval 2
  }
  return(returnedList) #Finally, the list is returned.
}
