#' @name neo_dbs_coord_dates
#'
#' @description Update the coordinates of sites. To avoid site location duplicates (ex: Dja’de in the BDA has the coordinates: 38.1983	36.6638, but these ccordinates are 38.18	36.65 in CALPAL), grabs the first coordinates found for a given site, isolated this coordinates, and match it with site having the same name.
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
  # df <- df.c14.djade
  df <- df[!is.na(df[[SiteName]]), ]
  df.cor.coords <- read.csv2(c14.corrected.coords, sep = "\t", header = TRUE)
  for(i in seq(1, nrow(df.cor.coords))){
    site <- df.cor.coords[i, SiteName]
    lon <- as.numeric(df.cor.coords[i, "lon"])
    lat <- as.numeric(df.cor.coords[i, "lat"])
    # if(verbose){
    #   print(paste0("Read new coordinates for: ", site))
    # }
    df[df[ , SiteName] == site, "lon"] <- lon
    df[df[ , SiteName] == site, "lat"] <- lat
    # test
    # subset(df, SiteName == site)
  }
  if(verbose){
    print(paste0("    - coordinates corrected using the TSV file"))
  }
  # one set of coordinates by site / ie same coordinates for same sites
  df.coords <- data.frame(SiteName = character(),
                          lon = numeric(),
                          lat = numeric())
  SiteNames <- unique(df[[SiteName]])
  for(sn in SiteNames){
    # sn <- "Dja'de"
    first_occurrence <- subset(df, SiteName == sn)[1, ]
    first_occurrence <- first_occurrence[, c("SiteName", "lon", "lat")]
    df.coords <- rbind(df.coords, first_occurrence)
  }
  # rm old coordinates
  df$lon <- df$lat <- NULL
  # merge new coordinates
  df <- merge(df, df.coords, by = "SiteName", all.x = TRUE)
  if(verbose){
    print(paste0("    - coordinates standardized by site (one set of coordinates by site)"))
  }
  return(df)
}

