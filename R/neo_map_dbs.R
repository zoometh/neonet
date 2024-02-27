neo_map_dbs <- function(df.all.res = NA,
                        infile = "C:/Rprojects/neonet/doc/ref_table_per.xlsx"){
  # maps culture and periods from dates coming from different dbs to the Neonet classes (..MM, LM, EN, MN, ...)# These one-to-one equivalence are listed in an XLSX file
  # read the ref tab
  df_ref_per <- openxlsx::read.xlsx(infile)
  df_ref_per$class <- toupper(df_ref_per$class)
  df_ref_per <- df_ref_per[!is.na(df_ref_per$class), ]
  df_ref_per$period_culture <- paste0(df_ref_per$period, "/", df_ref_per$culture)
  df.all.res$period_culture <- paste0(df.all.res$period, "/", df.all.res$culture)
  df.classes <- merge(df.all.res, df_ref_per, by = "period_culture")
  # remove duplicates. TODO: prioritise duplicates according to the db they are coming from (ex: neonet)
  df.classes <- df.classes[!duplicated(df.classes$labnr), ]
  # map colnames to neonet format
  df.classes <- df.classes %>%
    dplyr::rename(!!!setNames(rename_c14bazAAR, names(rename_c14bazAAR))) %>%
    dplyr::select(names(rename_c14bazAAR))
  return(df.classes)
}