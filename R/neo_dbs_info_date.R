#' @name neo_dbs_info_date
#'
#' @description Return info on a date from its LabCode from the NeoNet dataset. This function is useful once different databases have been parsed (`neo_dbs_parse()`) and aligned (`neo_dbs_align()`) in order to spot outliers (i.e. abberant dates)
#'
#' @param LabCode The LabCode of the date to check. 
#' @param columns The list of columns to display.
#' @param format The output format. Default "aberrant_dates" 
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
                              df.c14 = NA,
                              columns = c("sourcedb", "LabCode", "SiteName", "median", "db_period", "db_culture"),
                              aberrant_dates = "aberrant_dates",
                              verbose = TRUE){
  `%>%` <- dplyr::`%>%` # used to not load dplyr 
  if(inherits(df.c14, "sf")){
    if(verbose){
      print(paste0("Reads a 'sf' dataframe"))
    }
    # df.out <- na.omit(sf::st_set_geometry(df.c14[df.c14$LabCode == LabCode, columns], NULL))[1,]
    df.out <- sf::st_set_geometry(df.c14[df.c14$LabCode == LabCode, columns], NULL)
    df.out <- df.out[!is.na(df.out$LabCode), ][1, ]
    a.date <- as.character(df.out)
  }
  if(!inherits(df.c14, "sf")){
    if(verbose){
      print(paste0("Reads a dataframe"))
    }
    # df.out <- na.omit(df.c14[df.c14$LabCode == LabCode, columns])[1, ]
    df.out <- df.c14[df.c14$LabCode == LabCode, columns]
    df.out <- df.out[!is.na(df.out$LabCode), ][1, ]
    a.date <- as.character(df.out)
  }
  df.out.cat <- sf::st_drop_geometry(df.out)
  if(aberrant_dates == "aberrant_dates"){
    if(verbose){
      print(paste0("Layout for 'aberrant dates' format:"))
    }
    df.out.cat$comments <- NA # to match the 'aberrant dates' TSV format
    df.out.cat$median <- round(df.out.cat$median, 0)
    df.out.cat <- df.out.cat %>%
      dplyr::mutate(across(everything(), ~ifelse(is.na(.), "None", .)))
    for(i in seq(1, nrow(df.out.cat))){
      # print(df.out.cat[i, ])
      cat(paste(df.out.cat[i, ], collapse = "\t"), "\n")
    }
  } else {
    print(df.out.cat)
  }
  return(df.out)
}
