#' Creates a KCC map of the research area
#' 
#' @name neo_kcc_sankey
#' 
#' @description Creates a Sankey diagram. To run after a `neo_kcc_extract()`
#'
#' @param df_cc A dataframe.
#' @param col.req Columns of the KCC.
#' @param selected.per To subset on periods (ex: LM) or with the whole dataset (NA).
#'
#' @return A ggplot
#'
#' @examples
#'
#' @export
neo_kcc_sankey <- function(df_cc = NA,
                           col.req = NA,
                           kcc_colors = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/koppen.tsv",
                           selected.per = NA,
                           verbose = TRUE){
  # df_cc_ <- as.data.frame(df_cc)
  `%>%` <- dplyr::`%>%`
  if(inherits(df_cc, "sf")){
    df <- sf::st_set_geometry(df_cc, NULL)
  }
  # for(per in selected.per){
  # per <- "LM"
  if(!is.na(selected.per)){
    df <- df[df$Period %in% selected.per, ]
  }
  if(!all(col.req %in% colnames(df))){
    stop("Field names in 'col.req' are not present in the dataframe")
  }
  df <- df[ , which(names(df) %in% col.req)]
  # df <- na.omit(df)
  df.sank <- df %>%
    ggsankey::make_long(colnames(df))
  # df.sank
  if(verbose){
    print(paste0("*read KCC colors"))
  }
  koppen_colors <- read.table(kcc_colors, sep = "\t", header = TRUE)
  kcc_color_map <- setNames(koppen_colors$color, koppen_colors$code)
  period.names <- paste0(selected.per, collapse = "-")
  tit <- paste0(period.names, " KCC changes in Kyears (n = ", nrow(df), ")")
  gout <- ggplot2::ggplot(df.sank, ggplot2::aes(x = x,
                                                next_x = next_x,
                                                node = node,
                                                next_node = next_node,
                                                fill = factor(node),
                                                label = node)) + 
    ggsankey::geom_sankey(flow.alpha = 0.5,
                          node.color = "black",
                          show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = kcc_color_map) +
    ggsankey::geom_sankey_label(size = 3, color = "black", fill= "white", hjust = -0.5) +
    ggplot2::labs(caption = "@neonet") +
    ggplot2::labs(title = tit) +
    ggplot2::theme_bw()
  if(verbose){
    print(paste0("*Sankey plot has been created"))
  }
  return(gout)
}