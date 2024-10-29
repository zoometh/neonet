#' @name neo_dbs_sitename_dates
#'
#' @description Avoid site names mispelling (ex: Franchthi from one database, and Franchthi Cave from another database) by performing a left join to replace SiteName in df.c14 with SiteName from sitenames when there's a match on AlternativeNames
#'
#' @param df.c14 a dataset of dates
#' @param sitenames.equiv A TSV file listing the equivalences between site names.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A dataframe of dates
#'
#' @examples
#' 
#' df.c14 <- neo_dbs_sitename_dates(df.c14)
#'
#'
#' @export
neo_dbs_sitename_dates <- function(df.c14 = NA,
                                   sitenames.equiv = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/c14_corrected_sitenames.tsv",
                                   verbose = TRUE){
  sitenames <- read.csv2(sitenames.equiv, sep = "\t")
  `%>%` <- dplyr::`%>%`
  # Perform a left join to replace SiteName in df.c14 with SiteName from sitenames when there's a match on AlternativeNames
  df <- df.c14 %>%
    dplyr::left_join(sitenames, by = c("SiteName" = "AlternativeNames")) %>%
    dplyr::mutate(SiteName = dplyr::coalesce(SiteName.y, SiteName)) %>%
    dplyr::select(-SiteName.y)
  if(verbose){
    corrected.sitenames <- setdiff(df.c14$SiteName, df$SiteName)
    print(paste0("These SiteNames have been corrected: '", paste0(corrected.sitenames, collapse = ", "), "'"))
    print(paste(length(unique(df.c14$SiteName)), " site names --> ", length(unique(df$SiteName)), " site names"))
  }
  return(df)
}