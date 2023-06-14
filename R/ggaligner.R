#' Visualize the alignment object from msa package
#'
#' Generating a plot for msa object of DNA or Protein sequence
#'
#' @usage ggaligner(alignment,start=1,end=10,color="Clustal",font="helvetical",label_font = 12)
#'
#' @param alignment alignment object returned from msa package
#'
#' @param start start position of the desired alignment region
#'
#' @param end end position of the desired alignment region
#'
#' @param color color scheme to use ex: Clustal, Chemistry_AA, Shapely_AA
#'
#' @param font font family to use ex: helvetical, TimesNewRoman
#'
#' @param label_font font size of sequence names
#'
#' @return An enhanced plot for the alignment using ggplot2 and ggmsa packages
#'
#' @export
#'
#' @author Mohamed Soudy \email{Mohmedsoudy2009@gmail.com}
#'
ggaligner <- function(alignment, start = 1, end = 10, color = "Clustal", font = "helvetical", label_font = 12)
{
  Alignment.frame <- data.frame(Seq = as.character(alignment))

  Seqtochar <- function(x) {
    unlist(strsplit(x, split = ""))
  }
  msa.frame <- apply(Alignment.frame, 1 , Seqtochar)
  colnames(msa.frame) <- alignment@unmasked@ranges@NAMES
  rownames(msa.frame) <- seq(1:nrow(msa.frame))

  if (end > dim(msa.frame)[1])
    end <- dim(msa.frame)[1]

  Align.range <- msa.frame[start:end,]
  Align.data <- melt(t(Align.range))

  colnames(Align.data) <- c("name", "position", "character")
  Align.data$character <- as.character(Align.data$character)

  msa_plot <- ggplot() + geom_msa(Align.data, seq_name = T, color = color, char_width = 0.9, font = font) +
    xlab("") +  ylab("") + scale_x_continuous(breaks=c(start, ceiling(start+end/2), end)) +
    theme(axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          axis.text.y = element_text(face = "bold", size = label_font),
          axis.text.x = element_text(face = "bold", size = 10))

  return(msa_plot)
}
