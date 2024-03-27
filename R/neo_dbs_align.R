#' @name neo_dbs_align
#'
#' @description Aligns culture and periods of radiocarbon dates coming from different c14bazAAR dbs to the Neonet classes (..., MM, LM, EN, MN, ...) using a mapping table created by the function `neo_dbs_create_ref()`
#'
#' @param df A dataframe created with the `neo_parse_db()` function.
#' @param mapping.field The name of the LabCode field used to remove duplicates. Default: "labnr". 
#' @param mapping.file A mapping file with one-to-one correspondences. Default: `ref_table_per.xlsx`
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A dataframe of standardized radiocarbon dates.
#'
#' @examples
#'
#'                    
#' @export
neo_dbs_align <- function(df = NA,
                          mapping.field = "labnr",
                          mapping.file = "C:/Rprojects/neonet/doc/ref_table_per.xlsx",
                          verbose = TRUE){
  `%>%` <- dplyr::`%>%`
  # load the config file
  source("R/config.R")
  # read the ref tab
  # TODO: read a file hosted on GitHub
  df_ref_per <- openxlsx::read.xlsx(mapping.file)
  df_ref_per$class <- toupper(df_ref_per$class)
  df_ref_per <- df_ref_per[!is.na(df_ref_per$class), ]
  if(verbose){
    print(paste0("There are ", nrow(df_ref_per), " having cultures / periods ",
                 "having neonet equivalences (column 'class')"))
  }
  df_ref_per$period_culture <- paste0(df_ref_per$period, "/", df_ref_per$culture)
  df$period_culture <- paste0(df$period, "/", df$culture)
  df.classes <- merge(df, df_ref_per, by = "period_culture")
  # remove duplicates. TODO: prioritise duplicates according to the db they are coming from (ex: neonet)
  if(verbose){
    print(paste0("  - remove duplicated radiocarbon dates on '", mapping.field, "'"))
  }
  df.classes <- df.classes[!duplicated(df.classes[[mapping.field]]), ]
  # map colnames to neonet format
  df.classes <- df.classes %>%
    dplyr::rename(!!!setNames(rename_c14bazAAR, names(rename_c14bazAAR))) %>%
    dplyr::select(names(rename_c14bazAAR))
  if(verbose){
    print(paste0("  - columns have been mapped!"))
    print(paste0("  - n = ", nrow(df.classes), " radiocarbon dates"))
  }
  return(df.classes)
}