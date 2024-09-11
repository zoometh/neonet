#' @name neo_dbs_coord_dates
#'
#' @description Update the coordinates of sites.
#'
#' @param df A dataframe of dates
#' @param c14.corrected.coords Path to a TSV having corrected coordinates. 
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A dataframe
#' 
#'
#' @examples
#'
#'
#' @export
neo_dbs_coord_dates <- function(df = NA,
                                c14.corrected.coords = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/c14_corrected_coordinates.tsv"){
  df.cor.coords <- read.csv2(c14.corrected.coords, sep = "\t", header = TRUE)
  subset(df_filtered, SiteName == 'Sabha')
}

