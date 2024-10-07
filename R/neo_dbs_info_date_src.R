#' @name neo_dbs_info_date_src
#'
#' @description Once the LabCode's database origin is sourced, print this LabCode infos
#'
#' @param db The database name.
#' @param LabCode The LabCode of the date to check. 
#'
#' @return Print on the screen.
#'
#' @examples
#'
#' df_filtered <- neo_dbs_rm_dates(df.c14)
#'
#' @export
neo_dbs_info_date_src <- function(db = NA, 
                                  LabCode = NA,
                                  print.res = TRUE){
  if(db == "neonetatl"){db <- "neonet"}
  df <- c14bazAAR::get_c14data(db)
  if(!is.na(LabCode)){
    df <- as.data.frame(df[df$labnr == LabCode, ])
  }
  df <- df[!is.na(df$labnr), ]
  if(print.res){
    print(df)
  }
}