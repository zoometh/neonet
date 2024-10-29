#' @name neo_dbs_labcode_dates
#'
#' @description Avoid Labcode mispelling (ex: 	Hd-16784---768 from one database, and Hd-16784-768 from another database) by performing a left join to replace LabCode in df.c14 with LabCode from labcodes when there's a match on AlternativeLabCode
#'
#' @param df.c14 a dataset of dates
#' @param sitenames.equiv A TSV file listing the equivalences between LabCodes.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A dataframe of dates
#'
#' @examples
#' 
#' df.c14 <- neo_dbs_labcode_dates(df.c14)
#'
#'
#' @export
neo_dbs_labcode_dates <- function(df.c14 = NA,
                                   sitenames.equiv = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/c14_corrected_labcodes.tsv",
                                   verbose = TRUE){
  sitenames <- read.csv2(sitenames.equiv, sep = "\t")
  `%>%` <- dplyr::`%>%`
  # Perform a left join to replace LabCode in df.c14 with LabCode from sitenames when there's a match on AlternativeLabCode
  df <- df.c14 %>%
    dplyr::left_join(sitenames, by = c("LabCode" = "AlternativeLabCode")) %>%
    dplyr::mutate(LabCode = dplyr::coalesce(LabCode.y, LabCode)) %>%
    dplyr::select(-LabCode.y)
  if(verbose){
    corrected.sitenames <- setdiff(df.c14$LabCode, df$LabCode)
    print(paste0("These LabCodes have been corrected: '", paste0(corrected.sitenames, collapse = ", "), "'"))
  }
  return(df)
}