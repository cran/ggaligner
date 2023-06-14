#' Convert string to a character vector
#'
#' convert string that can be DNA or protein sequence to a character vector
#'
#' @usage Seqtochar(x)
#'
#' @param x a sequence of DNA or Protein
#'
#' @export
#'
#' @examples Seqtochar("ATGACATAAT")
#'
#' @author Mohamed Soudy \email{Mohmedsoudy2009@gmail.com}
#'
#' @returns this function is mainly used by ggaligner function to convert the input sequence to character vector
#'
Seqtochar <- function(x) {
  unlist(strsplit(x, split = ""))
}
