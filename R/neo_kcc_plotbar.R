#' @name neo_kcc_extract
#'
#' @description Extract Koppen Climate Classes (KCC) from one or different KCC SpatRaster (`kcc_file`) using the median calibrated date form a `sf` dataframe of dates (`df.c14`). For example KCC 6k (i.e. -6000) entails dates having their medians between -6500 and -5500 calBC. Creates a stacked plotbar of the different radiocarbon dates sums by KCC.
#' 
#' @param df_cc .
#' @param col.req  .
#' @param root.path The path to the `kcc.file` parent folder.
#' @param kcc.file Koppen Climate Classes (KCC) SpatRaster (i.e. GeoTiffs) generated with the `xxx()` function.
#' @param kcc_colors .
#' @param colname.period The name of the `df_cc` dataframe column of the periods
#' @param selected.per Selected periods.
#' @param export .
#' @param outDir .
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return
#'
#' @examples
#'
#' neo_kcc_plotbar(df_cc = df_cc, 
#'                 col.req = col.req,
#'                 selected.per = c("LM", "MM"),
#'                 export = TRUE,
#'                 outDir = "C:/Rprojects/neonet/results/")
#' @export
neo_kcc_plotbar <- function(df_cc = NA,
                            col.req = NA,
                            kcc.file = c("koppen_6k.tif", "koppen_7k.tif", "koppen_8k.tif",
                                         "koppen_9k.tif", "koppen_10k.tif", "koppen_11k.tif"),
                            # kcc_colors = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/koppen.tsv",
                            colname.period = "Period",
                            selected.per = NA,
                            present = 2000,
                            title = NA,
                            legend.show = FALSE,
                            export = FALSE,
                            outDir = NA){
  # df_cc_ <- as.data.frame(df_cc)
  source("R/config.R")
  df <- sf::st_set_geometry(df_cc, NULL)
  # for(per in selected.per){
  # per <- "LM"
  df <- df[df[[colname.period]] %in% selected.per, ]
  df <- df[ , which(names(df) %in% col.req)]
  # library(tidyverse)
  df_long <- df %>%
    tidyr::pivot_longer(cols = starts_with("koppen_"), names_to = "koppen_type", values_to = "value") %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(koppen_type, value) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(koppen_type) %>%
    dplyr::mutate(percentage = count / sum(count) * 100) %>%
    dplyr::ungroup()
  # # reorder df_long
  df_long$koppen_type <- factor(df_long$koppen_type)
  kcc <- DescTools::SplitPath(kcc.file)$filename
  df_long$koppen_type <- factor(df_long$koppen_type, levels = rev(kcc))
  df_long <- df_long[order(df_long$koppen_type), ]
  # plot
  period.names <- paste0(selected.per, collapse = "-")
  # TODO: by site?
  tit <- paste0(period.names)
  used.periods <- paste0(selected.per, collapse = ", ")
  caption <- paste("Koppen Climate Classes in ka BP",
                   "| BP =", present, 
                    "| periods:", used.periods, "| n =", nrow(df), "dates)")
  gout <- ggplot2::ggplot(df_long, ggplot2::aes(x = koppen_type, y = percentage, fill = factor(value))) +
    ggplot2::geom_bar(stat = "identity", position = "fill") +
    # scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_manual(values = kcc_colors, name = 'classes') +
    ggplot2::labs(x = "Koppen Classification", 
                  y = "%", 
                  title = title,
                  caption = caption) +
    ggplot2::theme_minimal()
  if(!legend.show){
    gout <- gout +
      ggplot2::theme(legend.position = "none",
                     axis.title.x = ggplot2::element_blank())
  }
  if(export){
    fileOut <- paste0(outDir, period.names, "_kcc_stacked.png")
    ggplot2::ggsave(filename = fileOut,
                    gout,
                    width = 14, height = 10)
    print(paste0(fileOut, " has been exported"))
  }
  else{
    return(gout)
  }
}
