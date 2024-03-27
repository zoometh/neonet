#' @name neo_dbs_rm_date
#'
#' @description Remove unaccurate/abberant dates. The latter are listed in a TSV file. Not always there is enough evidences to discard these dates, dates having a "-" as a prefix in their database name (ex: "-bda") are escaped ( 
#'
#' @param df.c14 A dataframe. 
#' @param c14.to.remove The list of dates to remove in a dataframe TSV format.
#' @param selected.cols When verbose, will display these columns.
#' @param escape.pattern Dates having this pattern in the database they are coming from won't be removed. Default: "^-" (ex: "-bda")
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A dataframe
#' 
#'
#' @examples
#'
#' df_filtered <- neo_dbs_rm_date(df.c14)
#'
#' @export
neo_dbs_rm_date <- function(df.c14 = NA, 
                            c14.to.remove = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/c14_aberrant_dates.tsv",
                            selected.cols = c("sourcedb", "LabCode", "SiteName", "median", "db_period"),
                            escape.pattern = "^-",
                            verbose = TRUE){
  df.to.rm <- read.table(c14.to.remove, sep = "\t", header = TRUE)
  df.to.rm <- df.to.rm[!grepl(escape.pattern, df.to.rm$sourcedb), ]
  if(verbose){
    print(paste0(nrow(df.to.rm), " dates to be removed:"))
    print(df.to.rm[ , selected.cols])
  }
  df <- dplyr::anti_join(df.c14, df.to.rm, 
                         by = c("sourcedb", "LabCode"))
  return(df)
}

