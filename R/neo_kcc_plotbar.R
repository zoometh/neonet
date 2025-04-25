#' @name neo_kcc_extract
#'
#' @description Extract Koppen Climate Classes (KCC) from one or different KCC SpatRaster (`kcc_file`) using the weighted median of calibrated radiocarbon dates form a `sf` dataframe of dates (`df.c14`). For example KCC 6k (i.e. -6000) encompasses dates having their medians between -6500 and -5500 calBC. 
#' 
#' @param df_cc A sf dataframe or a dataframe.
#' @param col.req A vector of column names having KCC values (ex: `c("koppen_6k", "koppen_7k")`.
#' @param root.path The path to the `kcc.file` parent folder.
#' @param kcc.file Koppen Climate Classes (KCC) SpatRaster (i.e. GeoTiffs) generated with the `xxx()` function.
#' @param kcc_colors A character named vector of KCC codes as names (ex: 'Af') and colors (ex: `#0000FF`) as values. This vector comes from the `config.R` file.
#' @param colname.period The name of the `df_cc` dataframe column of the periods. Default: `Period`.
#' @param selected.per A vector of selected periods to subset the stacked barplot by periods (ex: `c("LM", "MM")`).
#' @param legend.show Show the KCC legend. Default: FALSE.
#' @param counts.show Show the sites' count on the stacked plots. Default: TRUE.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return Creates a ggplot stacked plotbars of radiocarbon dates sums by KCC (koppen_7k, koppen_8k, etc.).
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
                            ref.period = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/periods.tsv",
                            selected.per = NA,
                            present = 2000,
                            title = NA,
                            legend.show = FALSE,
                            counts.show = TRUE,
                            counts.show.size = 2,
                            verbose = TRUE){
  # df_cc_ <- as.data.frame(df_cc)
  `%>%` <- dplyr::`%>%`
  source("R/config.R")
  if(inherits(df_cc, "sf")){
    if(verbose){
      print(paste0("Reads a 'sf' dataframe"))
    }
    df <- sf::st_set_geometry(df_cc, NULL)
  }
  if(is.data.frame(df_cc) & !inherits(df_cc, "sf")){
    if(verbose){
      print(paste0("Reads a simple dataframe"))
    }
    df <- df_cc
  }
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
  ## caption
  periods.colors <- read.csv(ref.period, sep = "\t")
  periods.colors <- periods.colors[periods.colors$period %in% selected.per, ]
  capt <- c()
  for(i in 1:nrow(periods.colors)){
    period <- periods.colors[i, "period"]
    color <- periods.colors[i, "color"]
    capt <- c(capt, paste0("<span style='color: ", color, ";'>", period, "</span>"))
  }
  capt <- paste0(capt, collapse = ", ")
  caption <- paste("Number of dates by Koppen Climate Classes in ka BP",
                   # "| BP =", present, 
                   "| periods:", capt, "| n =", nrow(df), "dates")
  kcc_colors.sub <- kcc_colors[names(kcc_colors) %in% unique(df_long$value)]
  df_long <- na.omit(df_long)
  if(verbose){
    print(paste0("Create the stacked plot bar for: ", selected.per))
  }
  gout <- ggplot2::ggplot(df_long, 
                          ggplot2::aes(x = koppen_type, y = percentage, fill = factor(value))) +
    ggplot2::geom_bar(stat = "identity", position = "fill") +
    ggplot2::scale_fill_manual(values = kcc_colors.sub, name = 'classes') +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(
      title = title,
      caption = caption) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank()) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                  plot.caption = ggtext::element_markdown())
  if(counts.show){
    gout <- gout +
      # show count when > n 
      ggplot2::geom_text(ggplot2::aes(label = as.character(count)),
                        #ggplot2::aes(label = ifelse(count > 1, as.character(count), "")), 
                         position = ggplot2::position_fill(vjust = 0.5), 
                         size = counts.show.size, 
                         color = "black")
  }
  # #################################################################
  # ## try to add jittered points 
  # # normilising
  # df_normalized <- df_long %>%
  #   dplyr::group_by(koppen_type) %>%
  #   dplyr::mutate(normalized_percentage = percentage / max(percentage))
  # # plotting
  # ggplot(df_normalized, aes(x = koppen_type, fill = factor(value))) +
  #   geom_bar(aes(y = percentage / sum(percentage)), stat = "identity", position = "fill") +
  #   ggplot2::geom_jitter(ggplot2::aes(y = normalized_percentage, color = factor(value)),
  #                        position = ggplot2::position_jitter(width = 0.2), size = 2, alpha = 0.6) +
  #   scale_y_continuous(labels = scales::percent_format()) +
  #   scale_fill_manual(values = kcc_colors.sub, name = 'classes') +
  #   labs(title = "Normalized Data on Filled Bar Plot", y = "Percentage", x = "Köppen Type") +
  #   theme_minimal()
  # #######################################################
  if(!legend.show){
    gout <- gout +
      ggplot2::theme(legend.position = "none",
                     axis.title.x = ggplot2::element_blank())
  }
  if(verbose){
    print(paste0("Export the plot"))
  }
  return(gout)
}
