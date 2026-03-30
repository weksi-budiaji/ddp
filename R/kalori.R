#' Calory calculation
#'
#' @description This function calculates the total calory of each responden.
#'
#' @param data A data set of (\emph{n x 218})  (\emph{see} \strong{Details}).
#' @param output A desirable output, the default is "all" 
#' (\emph{see} \strong{Details}).
#'
#' @details The data set is an \emph{n x 218} data frame. The first column is
#' the name of the respondent. The rest columns are types of food. The type of
#' food can be listed as in the data simulation (\emph{see} in the data example
#' of \code{simulasi} or \code{vignette("ddp")}).
#' 
#' The \code{output} argument has "all" as the 
#' default, meaning that all of the calories are yielded. They are
#' energy, protein, fat, and carbohydrate. Single calory can be produced
#' by writing the output argument with "protein" for the calory of protein,
#' for example. The possible inputs for \code{output} argument are
#' "all", "energi", "protein", "lemak" for fat, and "karbohidrat".  
#'
#' @return Function returns a matrix of \emph{n x 4} for "all" and \emph{n x 1} 
#' for other "output" arguments.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @references BKP, Kementan. 2017. Aplikasi Harmonisasi Analisis PPH Data
#' Susenas 2017. Badan Ketahanan Pangan Kementrian Pertanian.
#'
#' @examples
#' #data simulation of 10 person
#' set.seed(2020)
#' n <- 10
#' matsim <- matrix(0, n, 218)
#' datsim <- as.data.frame(matsim)
#' datsim$V1 <- LETTERS[1:n]
#' 
#' #calory for boiled rice
#' datsim$V2 <- rnorm(n, 200, 50)
#' #calory for boiled egg
#' datsim$V73 <- rnorm(n, 60, 5)
#' #calory for fresh milk
#' datsim$V79 <- rnorm(n, 100, 10)
#' #calory for tomato
#' datsim$V93 <- rnorm(n, 19, 2)
#' #caloty for pineapple
#' datsim$V134 <- rnorm(n, 20, 2)
#' 
#' kalori(datsim)
#' 
#' @export

kalori <- function(data, output = "all") {
  
  komposisi <- standar[,-c(1,6)]
  datmat <- as.matrix(data[,-1])
  
  berat <- matrix(standar[,1],nrow = nrow(standar), ncol=nrow(datmat))
  
  koefisien <- t(datmat)/berat
  
  n <- nrow(data)
  hasil <- matrix(0, n, 4)
  colnames(hasil) <- colnames(standar)[-c(1,6)]
  rownames(hasil) <- 1:n
  for (i in 1:n) {
    for (j in 1:4) {
      hasil[i,j] <- sum(koefisien[,i]*komposisi[,j])
    }
  }
  
  satuan <- c("all", "energi", "protein","lemak", "karbohidrat")
  output <- match.arg(output, satuan)
  hasil <- switch(output,
                  all = hasil,
                  energi = hasil[,1,drop=FALSE],
                  protein = hasil[,2,drop=FALSE],
                  lemak = hasil[,3,drop=FALSE],
                  karbohidrat= hasil[,4,drop=FALSE])
  return(hasil)
}