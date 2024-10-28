#' @name neo_dbs_info_dates_datatable
#'
#' @description Creates a datatable (DT) of dates with their Koppen Climate Classes with filters, sort, etc. functions. Usefull to check a radiocarbon dataset with their KCC.
#'
#' @param df.c14 A df (URL) with medians calculated and KCC climates belonging to one KCC period (ex: 8ky BP). For example: "https://raw.githubusercontent.com/zoometh/neonet/master/doc/talks/2024-simep/img/isochrones-barriere-Italy-EN-kcc.tsv"
#' @param kcc_df A dataframe for the long legend.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A datatable (DT)
#' 
#'
#' @examples
#'
#' dt.out <- neo_dbs_info_dates_datatable()
#' dt.out
#'
#' @export
neo_dbs_info_dates_datatable <- function(df.c14 = NA,
                                         kcc_df = "https://raw.githubusercontent.com/zoometh/neonet/master/inst/extdata/koppen.tsv",
                                         verbose = TRUE){
  # library(DT)
  # library(dplyr)
  `%>%` <- dplyr::`%>%` # used to not load dplyr 
  climate_df <- read.table(df.c14, sep = "\t", header = TRUE) # TODO: chnage to `read.csv2()`
  kcc_colors  <- read.csv2(kcc_df, sep = "\t")
  kcc_colors <- kcc_colors[ , c("code", "color")]
  kcc_colors <- rbind(kcc_colors, c(NA, "#FFFFFF"))
  climate_df <- merge(climate_df, kcc_colors, by = "code", all.x = TRUE)
  climate_df$lon <- climate_df$lat <- NULL
  climate_df <- climate_df[, c("idf", "site", "median", "period", "code", "sourcedb", "color")]
  # Identify the index of the 'color' column (for JavaScript, this needs 0-based indexing)
  color_col_index <- which(colnames(climate_df) == "color") - 1
  dt.out <- climate_df %>%
    DT::datatable(
      width = "100%",
      rownames = FALSE,
      options = list(
        lengthMenu = list(c(7, 10, 50, -1), 
                          c('7', '10', '50', 'All')),
        paging = TRUE,
        columnDefs = list(list(visible = FALSE, targets = color_col_index)),  # Hide the 'color' column
        initComplete = htmlwidgets::JS(
          "function(settings, json) {",
          paste0("$(this.api().table().container()).css({'font-size': '", "12pt", "'});"),
          "}")
      )
    ) %>% 
    DT::formatStyle(
      'color',  # The column used to determine the color
      target = 'row',  # Apply the color to the entire row
      backgroundColor = DT::styleEqual(
        unique(climate_df$color),  # Unique color values in your 'color' column
        unique(climate_df$color)   # Matching those values to the same background color
      )
    )
  return(dt.out)
}