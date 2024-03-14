#' @name neo_dbs_create_ref
#'
#' @description Create a reference XLSX file used to map external dbs to Neonet periods (class) (..., LM, EN, ...). The XLSX file can be read later by the function `neo_dbs_align()`.
#'
#' @param df.all.res A vector of radiocarbon datasets listed in c14bazAAR.
#' @param root.path .
#' @param culture.period The name of the columns recording the cultural assessments. Default: `c("period", "culture")`. 
#' @param overwrite Whether to overwrite a potential file having the same name, or not. Default: FALSE.
#' @param verbose if TRUE (default) then display different messages.
#' 
#' @details
#' 
#'
#' @return Creates an XLSX file.
#'
#' @examples
#'
#'                    
neo_dbs_create_ref <- function(df.all.res = NA, 
                               root.path = NA,
                               culture.period = c("period", "culture"),
                               outFile = "df_ref_per.xlsx",
                               overwrite = FALSE,
                               verbose = TRUE){
  print(unique(df.all.res$sourcedb))
  df.ref.per <- df.all.res[, culture.period]
  df.ref.per <- df.ref.per[!duplicated(df.ref.per), ]
  openxlsx::write.xlsx(df.ref.per, paste0(root.path, "/", outFile), overwrite = overwrite)
  if(verbose){
    print(paste0(outFile, " has been exported"))
  }
}