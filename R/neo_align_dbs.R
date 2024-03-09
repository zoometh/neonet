#' @name neo_align_dbs
#'
#' @description Aligns culture and periods of radiocarbon dates coming from different c14bazAAR dbs to the Neonet classes (..., MM, LM, EN, MN, ...) using a mapping table 
#'
#' @param df.all.res A dataframe created with the `neo_parse_db()` function.
#' @param mapping.field The name of the LabCode field used to remove duplicates. Default: "labnr". 
#' @param mapping.file A mapping file with one-to-one correspondences. Default: ref_table_per.xlsx
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A dataframe of standardized radiocarbon dates.
#'
#' @examples
#'
#' what.db <- c("calpal", "medafricarbon", "agrichange", "bda", "calpal", "radon", "katsianis")
#' when <- c(-9000, -4000)
#' where <- sf::st_read(where.roi,
#'                      quiet = TRUE)
#' col.c14baz <- c("sourcedb", "site", "labnr", "c14age", "c14std", "period", "culture", "lon", "lat")
#' df <- neo_parse_db(l.dbs = what.db,
#'                    col.c14baz = col.c14baz,
#'                    chr.interval.uncalBC = when,
#'                    roi = where)
#'                    
#' @export
neo_align_dbs <- function(df.all.res = NA,
                          mapping.field = "labnr",
                          mapping.file = "C:/Rprojects/neonet/doc/ref_table_per.xlsx",
                          verbose = TRUE){
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
  df.all.res$period_culture <- paste0(df.all.res$period, "/", df.all.res$culture)
  df.classes <- merge(df.all.res, df_ref_per, by = "period_culture")
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