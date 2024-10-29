#' @name neo_dbs_rm_duplicated_dates
#'
#' @description Remove duplicated dates on specified fields
#'
#' @param df.c14 a dataset of dates
#' @param duplicated.on.fields Fields on which duplicated will be identified.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A dataframe of dates
#'
#' @examples
#' 
#' df.c14 <- neo_dbs_rm_duplicated_dates(df.c14)
#'
#'
#' @export
neo_dbs_rm_duplicated_dates <- function(df.c14 = NA,
                                        duplicated.on.fields = c("SiteName", "median", "Period"),
                                        verbose = TRUE){
  # df <- unique(df.c14[ , duplicated.on.fields ] )
  `%>%` <- dplyr::`%>%`
  df <- df.c14 %>%
    dplyr::distinct(across(all_of(duplicated.on.fields)), .keep_all = TRUE)
  if(verbose){
    rm.dates <- nrow(df.c14) - nrow(df)
    print(paste0(rm.dates, " duplicated on fields '", paste0(duplicated.on.fields, collapse = ", "), "' have been removed"))
  }
  return(df)
}


