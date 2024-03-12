#' @name neo_kcc_legend
#'
#' @description Grab the legend of KCC. to get only existing KCC
#'
#' @param selected.kcc A vector (factor) of KCC to be selected.
#' @param kcc_df A dataframe for the long legend. Useful when `long.legend` is TRUE.
#' @param long.legend If TRUE, concatenate the KCC with its description (ex: "BSk Arid, steppe, cold"). Default: FALSE. 
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A grob.
#'
#' @examples
#'
#'                    
#' @export
neo_kcc_legend <- function(selected.kcc = NA,
                           kcc_df = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/koppen.tsv",
                           long.legend = FALSE,
                           verbose = TRUE){
  `%>%` <- dplyr::`%>%`
  source("R/config.R")
  climate_df <- data.frame(
    kcc = names(kcc_colors),
    color = kcc_colors
  )
  if(is.factor(selected.kcc)){
    climate_df <- climate_df[climate_df$kcc %in% selected.kcc, ]
  }
  if(long.legend){
    koppen.tsv <- read.csv2(kcc_df, sep = "\t")
    climate_df <- merge(climate_df, koppen.tsv, by.x = "kcc", by.y = "code", all.x = TRUE)
    climate_df$kcc <- paste(climate_df$kcc, climate_df$value)
    climate_df$color <- climate_df$color.x
    climate_df <- climate_df[ , c("kcc", "color")]
  }
  # Plot
  gout <- ggplot(climate_df, aes(x = kcc, y = 1, fill = factor(kcc))) + 
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = climate_df$color, name = 'classes') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::labs(x = NULL, y = NULL) + 
    ggplot2::theme_void()
  # gout
  tmp <- ggplot2::ggplot_gtable(ggplot_build(gout))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  # return(gout)
}