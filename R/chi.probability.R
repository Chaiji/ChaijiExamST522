#' @export
#' @return returns the probability that the chi-squared distribution is larger than the value of x.
#' @title Chi Probability
#' @usage chi.probability(x,df = 1,n = 100)
#'
#' @keywords distribution chi monte carlo simulation estimation matrix
#'
#' @description This function takes 3 arguments to estimate the probability that the chi-squared distribution is larger than an input value x.
#'
#' @param x specifies the value that should be evaluted by chi.probability
#' @param df is a positive integer that specifies the degrees of freedom in the chi-squared distribution.
#' @param n is a positive integer that specifies how many random variables that is used for the evaluation.
#' @author Emil H. Andersen \cr
#' Department of Mathematics and Computer Science (IMADA) \cr
#' University of Southern Denmark, Denmark \cr
#' \email{emila14@student.sdu.dk} \cr
#'
#' @examples
#' #Add examples for the function to explain to others how they can be used
#' chi.probability(5,1,100)

chi.probability <- function(x,df = 1,n = 100){
  #Syntax Processing
  #============================================================
  if(is.matrix(x) || is.list(x) || length(x)!=1){ #Check that x is a single value
    stop("'x' has to be an integer; length=1, non-list, non-matrix")
  } else if(!is.numeric(x)){ #checks if x is a number
    stop("'x' has to be a number")
  }
  if(is.matrix(df) || is.list(df) || length(df)!=1){ #Check that df is a single value
    stop("'df' has to be an integer; length=1, non-list, non-matrix")
  } else if(!is.numeric(df)){ #checks if df is a number
    stop("'df' has to be a positive integer")
  } else if(df != abs(round(df))){ #checks if df is a positive integer
    stop("'df' has to be a positive integer")
  }
  if(is.matrix(n) || is.list(n) || length(n)!=1){ #Check that n is a single value
    stop("'n' has to be an integer; length=1, non-list, non-matrix")
  } else if(!is.numeric(n)){ #checks if n is a number
    stop("'n' has to be a positive integer")
  } else if(n != abs(round(n))){ #checks if n is a positive integer
    stop("'n' has to be a positive integer")
  }
  #------------------------------------------------------------


  for (i in 1:length(x)){
    variables <- rnorm(n*df) #Generate the random variables for the matrix
    matrixNumbers <- matrix(variables, c(n,df)) #Generate the matrix
    chi <- numeric() #initialise chi
    for(i in 1:n){ #Sum up each row in the matrix to get the chi values.
      chi[i] <- sum(matrixNumbers[i,]^2)
    }
  cdf <- ecdf(chi) #Use the empirical cumulative distribution function
  a <- cdf(x) #Now we look at the specific value for x, i.e. P(chi^2 < x)
  return(1-a) #Return the probability 1-P(chi^2 < x) = P(chi^2 > x)

  }
}
