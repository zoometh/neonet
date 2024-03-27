#' @name neo_dbs_info_date
#'
#' @description Return info on a date from its LabCode. This function is usefull once dbs have been parsed (neo_dbs_parse) and aligned (neo_dbs_align) to spot outliers (i.e. abberant dates)
#'
#' @param LabCode The LabCode of the date to check. 
#' @param columns The list of columns to display.
#'
#' @return Print on the screen, return a dataframe.
#' 
#'
#' @examples
#'
#' df_filtered <- neo_dbs_rm_dates(df.c14)
#'
#' @export
neo_dbs_info_date <- function(LabCode = NA, 
                               columns = c("sourcedb", "LabCode", "SiteName", "median", "db_period", "db_culture")){
  # 
  if(inherits(df.c14, "sf")){
    df.out <- na.omit(sf::st_set_geometry(df.c14[df.c14$LabCode == LabCode, columns], NULL))[1,]
    a.date <- as.character(df.out)
  }
  if(is.data.frame(df.c14)){
    df.out <- na.omit(df.c14[df.c14$LabCode == LabCode, columns])[1, ]
    a.date <- as.character(df.out)
  }
  cat(paste(a.date, collapse = "\t"), "\n")
  return(df.out)
}