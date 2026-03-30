#' Desirable dietary pattern calculation
#'
#' @description This function calculates the desirable dietary pattern (DDP).
#'
#' @param data A data set of (\emph{n x 218})  (\emph{see} \strong{Details}).
#' @param wilayah An origin of the responden residence. 
#' (\emph{see} \strong{Details}).
#' @param baseline A baseline value of personal calory required.
#'
#' @details The data set is an \emph{n x 218} data frame. The first column is
#' the name of the respondent. \code{wilayah} argument has "Indonesia" as the 
#' default, meaning that the DPP are calculated based on the national (Indonesia)
#' baseline. The other possible inputs for \code{wilayah} are "Aceh", "Sumut",
#' "Sumbar", "Riau", "KepRiau", "Jambi", "Sumsel", "Babel", "Bengkulu",
#' "Lampung", "Jakarta", "Jabar", "Banten", "Jateng", "DIY", "Jatim", "Bali",
#' "NTB", "NTT", "Kalbar", "Kalteng", "Kalsel", "Kaltim", "Kalut", "Sulut",
#' "Sulteng", "Sultra", "Sulsel", "Gorontalo", "Sulbar", "Maluku", "Malut",
#' "Papua", "Papbar". For \code{baseline} argument, it is 2000 as the default 
#' value because the minimal calory required in Indonesia is 2000 calory.
#'
#' @return Function returns a vector with \emph{n} length indicates the  
#' index/ indices of the DDP per peson.
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
#' skorpph(datsim)
#' 
#' @export

skorpph <- function(data, wilayah = "Indonesia", baseline = 2000) {
  
  komposisi <- standar[,-c(1,6)]
  datmat <- as.matrix(data[,-1])
  
  berat <- matrix(standar[,1],nrow = nrow(standar), ncol=nrow(datmat))
  koefisien <- t(datmat)/berat
  
  n <- nrow(data)
  hasil <- matrix(0, 217, n)
  for (i in 1:n) {
    hasil[,i] <- koefisien[,i]*komposisi[,1] 
  }
  colnames(hasil) <- 1:n
  rownames(hasil) <- 1:217
  hasil <- as.data.frame(hasil)
  hslkategori <- split(hasil, standar$Kategori)
  listhasil <- lapply(hslkategori, colSums)
  mathasil <- matrix(unlist(listhasil), ncol = n, byrow = TRUE)
  tot <- colSums(mathasil)
  skoraktual <- skorake <- matrix(0, 9, n)
  
  idx <- rownames(bobot) == wilayah
  if(sum(idx)!= 1)
    stop(paste("wilayah = '", wilayah, 
               "' is incorrect, use 'Indonesia' instead or see ?skorpph"))
  
  nilaibobot <- as.numeric(bobot[idx,])
  
  for (i in 1:n) {
    skoraktual[,i] <- 100*nilaibobot*mathasil[,i]/tot[i]
    skorake[,i] <- 100*nilaibobot*mathasil[,i]/baseline
  }
  
  nilaimaksimal <- as.numeric(maksimal[idx,])
  
  nilaipph <- matrix(0,9,n)
  for(i in 1:9) {
    for (j in 1:n) {
      if ((skoraktual[i,j] > nilaimaksimal[i])|(skorake[i,j] > nilaimaksimal[i])) {
        nilaipph[i,j] <- nilaimaksimal[i]
      } else {
        if(skoraktual[i,j] > skorake[i,j]) {
          nilaipph[i,j] <- skoraktual[i,j]
        } else {
          nilaipph[i,j] <- skorake[i,j]
        }
      }
    }
  }
  pphfinal <- colSums(nilaipph)
  return(pphfinal)
}
