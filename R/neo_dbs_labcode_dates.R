#' @name neo_dbs_labcode_dates
#'
#' @description Avoid Labcode mispelling (ex: 	Hd-16784---768 from one database, and Hd-16784-768 from another database) by performing a left join to replace LabCode in df.c14 with LabCode from labcodes when there's a match on AlternativeLabCode. Avoid also uppercase vs lowercase mismatches.
#'
#' @param df.c14 a dataset of dates
#' @param labcodes.equiv A TSV file listing the equivalences between LabCodes.
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
                                   labcodes.equiv = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/c14_corrected_labcodes.tsv",
                                   verbose = TRUE){
  labcodes <- read.csv2(labcodes.equiv, sep = "\t")
  `%>%` <- dplyr::`%>%`
  # Perform a left join to replace LabCode in df.c14 with LabCode from labcodes when there's a match on AlternativeLabCode
  df <- df.c14 %>%
    dplyr::left_join(labcodes, by = c("LabCode" = "AlternativeLabCode")) %>%
    dplyr::mutate(LabCode = dplyr::coalesce(LabCode.y, LabCode)) %>%
    dplyr::select(-LabCode.y)
  if(verbose){
    corrected.labcodes <- setdiff(df.c14$LabCode, df$LabCode)
    print(paste0("These LabCodes have been corrected: '", paste0(corrected.labcodes, collapse = ", "), "' (mispelling)"))
    print(paste(length(unique(df.c14$LabCode)), " labcodes --> ", length(unique(df$LabCode)), " labcodes"))
  }
  # Lower- Upper-case
  df.2 <- df
  df.2$LabCodeX <- tolower(df.2$LabCode)
  df.2 <- df.2[!duplicated(df.2$LabCodeX), ]
  if(verbose){
    corrected.labcodes <- setdiff(df$LabCode, df.2$LabCode)
    print(paste0("These LabCodes have been corrected (10 firsts): '", paste0(head(corrected.labcodes, 10), collapse = ", "), "' (uppercase vs lowercase)"))
    print(paste(length(unique(df$LabCode)), " labcodes --> ", length(unique(df.2$LabCode)), " labcodes"))
  }
  df.2$LabCodeX <- NULL
  return(df.2)
}