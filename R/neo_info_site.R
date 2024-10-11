#' Gives info on a site (nb of different Periods having a date, etc.)
#' 
#' @name neo_info_site
#' 
#' @description Info on dates by sites
#'
#' @param df.c14 A sf object. If NA, will create the KCC map without dates
#' @param count.dates.by.per When TRUE (default): find sites having different periods represented, for example "EN", "MN", "LN", to illustrate the different moments (respectively: early farmers, long-distance trade, copper industry) or "EM", "MM", "LM". For example "Baume de Montclus" has all the Mesolithic represented, while "Franchthi Cave" has all the Neolithic represented
#'
#' @return A dataframe
#'
#' @examples
#' 
#' site.by.per.count <- neo_info_site(head(df.c14, 250))
#' head(site.by.per.count)
#' 
#' 
#'
#' @export
neo_info_site <- function(df.c14 = NA,
                          count.dates.by.per = TRUE,
                          verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  if(count.dates.by.per){
    if(verbose){
      print(paste0("Counts the number of Periods having a date"))
    }
    site.by.per <- df.c14 %>%
      dplyr::group_by(SiteName, Period) %>%
      dplyr::summarise(Count = dplyr::n(), .groups = 'drop')
    site.by.per.count <- as.data.frame(table(site.by.per$SiteName))
  }
  return(site.by.per.count)
  # to see the period, run, for example:
  # site.by.per[site.by.per$SiteName == "Franchthi Cave", ]
}