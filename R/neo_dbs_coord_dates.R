#' @name neo_dbs_coord_dates
#'
#' @description Update the coordinates of sites.
#'
#' @param df A dataframe of dates
#' @param SiteName The column for site names in both dataframe. Default: SiteName.
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
  # df <- samp_df
  df <- df[!is.na(df[[SiteName]]), ]
  df.cor.coords <- read.csv2(c14.corrected.coords, sep = "\t", header = TRUE)
  for(i in seq(1, nrow(df.cor.coords))){
    site <- df.cor.coords[i, SiteName]
    lon <- as.numeric(df.cor.coords[i, "lon"])
    lat <- as.numeric(df.cor.coords[i, "lat"])
    if(verbose){
      print(paste0("Read new coordinates for: ", site))
    }
    df[df[ , SiteName] == site, "lon"] <- lon
    df[df[ , SiteName] == site, "lat"] <- lat
    # test
    # subset(df, SiteName == site)
    if(verbose){
      print(paste0("    - coordinates updated"))
    }
  }
  return(df)
}

