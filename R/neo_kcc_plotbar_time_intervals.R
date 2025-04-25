#' @name neo_kcc_plotbar_time_intervals
#'
#' @description All climates of the time slice are simmed in a stacked histogram. Each date (w-median) has a KCC and belongs to a time slice (ex: 6000-5900 BC). 
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
neo_kcc_plotbar_time_intervals <- function(df_cc = NA,
                                           time.interval = c(6100, 5000),
                                           time.bin = 100,
                                           counts.show = TRUE,
                                           counts.show.size = 2,
                                           verbose = TRUE){
  # df_cc_ <- as.data.frame(df_cc)
  `%>%` <- dplyr::`%>%`
  source("R/config.R")
  # First define your 100-year bins
  breaks <- seq(-time.interval[1], -time.interval[2], by = time.bin)
  labels <- paste0(abs(breaks[-length(breaks)]), "–", abs(breaks[-1]))
  # Cut the 'median' column into those bins
  df_cc <- df_cc %>%
    dplyr::mutate(time_bin = cut(median, breaks = breaks, right = FALSE, labels = labels))
  
  # Gather the koppen columns into long format
  df_long <- df_cc %>%
    tidyr::pivot_longer(cols = starts_with("koppen_"),
                        names_to = "koppen_period",
                        values_to = "climate") %>%
    dplyr::filter(!is.na(climate))  # remove NA climates
  # Count the climate occurrences grouped by time_bin and climate
  result <- df_long %>%
    dplyr::group_by(time_bin, climate) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")
  kcc_colors.sub <- kcc_colors[names(kcc_colors) %in% unique(result$climate)]
  # Plot stacked bar chart
  # print(head(result))
  capt <- paste0(sum(result$count), " dates having a KCC | ",
                 nrow(df_cc) - sum(result$count), " dates without KCC | ",
                 nrow(df_cc), " dates in total")
  gout <- ggplot2::ggplot(result, 
                          ggplot2::aes(x = time_bin, y = count, fill = factor(climate))) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = kcc_colors.sub, name = 'classes') +
    # ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(
      # title = "Sum of Climate Classes by Time Span",
      caption = capt,
      x ="Time spans (BC)",
      y = "Number of dates") +
    # ggplot2::xlab("Dose (mg)") + ggplot2::ylab("Teeth length")+
    ggplot2::theme_minimal() +
    ggplot2::theme(
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.caption = ggtext::element_markdown())
  if(counts.show){
    totals <- result %>%
      dplyr::group_by(time_bin) %>%
      dplyr::summarise(total = sum(count), .groups = "drop")
    gout <- gout +
      # show count when > n 
      ggplot2::geom_text(ggplot2::aes(label = as.character(count)),
                         #ggplot2::aes(label = ifelse(count > 1, as.character(count), "")), 
                         position = ggplot2::position_stack(vjust = 0.5), 
                         size = counts.show.size, 
                         color = "black") +
      ggplot2::geom_text(data = totals, ggplot2::aes(x = time_bin, y = total, label = total),
                         vjust = -0.5, size = counts.show.size + 1, inherit.aes = FALSE)
  }
  return(gout)
}
