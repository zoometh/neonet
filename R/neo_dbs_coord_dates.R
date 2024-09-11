#' @name neo_dbs_coord_dates
#'
#' @description Update the coordinates of sites.
#'
#' @param df A dataframe of dates
#' @param SiteName The column of the site name in df. Default: SiteName.
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
                                SiteName = "SiteName",
                                c14.corrected.coords = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/c14_corrected_coordinates.tsv",
                                verbose = TRUE){
  df.cor.coords <- read.csv2(c14.corrected.coords, sep = "\t", header = TRUE)
  for(i in seq(1, nrow(df.cor.coords))){
    site <- df.cor.coords[i, "site"]
    if(verbose){
      print(paste0("Read new coordinates for the site: ", site))
    }
  }
  subset(samp_df, SiteName == 'Sabha')
}

