#' Validity and Reliability check.
#'
#' @description This function calculates the item-rest correlation.
#'
#' @param data A data set/ matrix  (\emph{see} \strong{Details}).
#' @param alpha An alpha value (\emph{see} \strong{Details}). 
#' @param total A single numeric value of the index column (\emph{see} \strong{Details}).
#'
#' @details The data set is a data frame/ matrix \emph{n x k}. The row is
#' the name of the respondent as many as \emph{n}, while the column is 
#' the variables (\emph{k}). The alpha value is set between 0.0001 and 
#' 0.20, the default is 0.05. If the \code{total} input is \code{NULL},
#' it means that the total score will be calculated first, 
#' the column index of the total score can be also stated otherwise.
#' The index of the column is a numeric value with a length of one.
#' It has to be between 1 and (\emph{k}).
#'
#' @return Function returns a data frame with \emph{k} row and four columns.
#' the columns indicate the item-rest correlation, correlation threshold,
#' p value, and validity and reliability conclusion.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @importFrom stats cor.test
#' @importFrom stats qt
#' 
#' @examples
#' #data simulation of 10 person 5 variables
#' set.seed(1)
#' dat <- matrix(sample(1:7,10*5, replace = TRUE), 10,5)
#' valid(dat)
#' 
#' @export

valid <- function(data, alpha=0.05, total=NULL) {
  if((inherits(data, "matrix")||is.data.frame(data))==FALSE)
    stop ("The data must be in matrix or data frame format.")
  
  if(alpha<0.0001||alpha>0.20)
    stop("Alpha must be in between 0.0001 and 0.20")
  
  n <- nrow(data)
  k <- ncol(data)
  
  if(is.null(colnames(data))) {
    colnames(data) <- 1:k
  }
  totskor <- apply(data, 1, sum)
  val <- matrix(0,k,4)
  for (i in 1:k){
    a <- cor.test(data[,i],totskor)
    val[i,1] <- round(a$estimate,3)
    val[i,3] <- round(a$p.value,3)
    val[i,4] <- ifelse (a$p.value<0.05, TRUE, FALSE)
  }
  rcal <- round(sqrt(qt(1-alpha,n-2)^2/(qt(1-alpha,n-2)+(n-2))),3)
  val[,2] <- rep(rcal,k)
  rownames(val) <- colnames(data)
  colnames(val) <- c("cor.est", "cor.thres","p.val", "valid")
  valdf <- as.data.frame(val)
  valdf$valid <- ifelse (valdf$valid==1, "Yes", "No")  
  
  if(is.null(total)) {
    res <- valdf
  } else {
    if(length(total)!=1||total<1||total>k)
      stop ("Please, provide the total score column in a single number of 
            column in between 1 and the number of total column.")
    res <- valdf[-total,]
  }
  return(res)
}
