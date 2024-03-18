#' @name neo_parse_dbs
#'
#' @description Collect radiocarbon dates form a list of dbs parsed by the c14bazAAR package, creates missing columns (period or culture), can filter on chronology (time interval) and spatial location (roi)
#'
#' @param l.dbs A vector of radiocarbon datasets listed in c14bazAAR.
#' @param col.c14baz A vector of fieldnames to collect from the `l.dbs` datasets.
#' @param present A date for the present, to calibrate from BP (1950). Default: 1950.
#' @param chr.interval.uncalBC A vector of two BC dates of chronological bounds to subset the radiocarbon dates selection.
#' @param roi A `sf` polygon to subset the radiocarbon dates selection.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A dataframe of standardized radiocarbon dates.
#'
#' @examples
#'
#' where <- sf::st_read(where.roi,
#'                      quiet = TRUE)
#' df <- neo_dbs_parse(l.dbs = c("calpal", "medafricarbon", "agrichange", "bda", "calpal", "radon", "katsianis"),
#'                     col.c14baz = c("sourcedb", "site", "labnr", "c14age", "c14std", "period", "culture", "lon", "lat"),
#'                     chr.interval.uncalBC = c(-9000, -4000),
#'                     roi = where)
#'                    
#' @export
neo_dbs_parse <- function(l.dbs = c("neonet"), 
                          col.c14baz = c("sourcedb", "site", "labnr", "c14age", "c14std", "period", "culture", "lon", "lat"),
                          present = 1950, 
                          chr.interval.uncalBC = NA, 
                          roi = NA,
                          verbose = TRUE){
  `%>%` <- dplyr::`%>%`
  # empty df
  df.all <- setNames(data.frame(matrix(ncol = length(col.c14baz), nrow = 0)),
                     col.c14baz)
  for(selected.db in l.dbs){
    # selected.db <- l.dbs[i]
    # selected.db <- "neonet"
    if(verbose){
      print(paste0("*read: ", selected.db))
    }
    options(timeout = 600)
    df <- c14bazAAR::get_c14data(selected.db)
    if(verbose){
      print(paste0("  n = ", nrow(df)))
    }
    # colnames(df)
    ## filters
    is.not.both <- !("culture" %in% colnames(df) & "period" %in% colnames(df))
    is.not.period <- !("period" %in% colnames(df))
    is.not.culture <- !("culture" %in% colnames(df))
    df_selected <- df
    if(is.not.both & is.not.culture){
      df_selected$culture <- NA 
    }
    if(is.not.both & is.not.period){
      df_selected$period <- NA 
    }
    df_selected <- df_selected[ , col.c14baz]
    df_selected <- df_selected %>%
      dplyr::filter(!(is.na("period") & is.na("culture")))
    # head(df_selected)
    df_selected$c14age_uncalBC <- df_selected$c14age - present
    df_selected$c14age_uncalBC <- - df_selected$c14age_uncalBC# data
    # chrono
    if(is.numeric(chr.interval.uncalBC)){
      chr.sup <- df_selected$c14age_uncalBC > chr.interval.uncalBC[1]
      chr.inf <- df_selected$c14age_uncalBC < chr.interval.uncalBC[2]
      df_selected <- df_selected[chr.sup & chr.inf, ]
    }
    # spatial
    df_selected <- df_selected[!(df_selected$lon == "" & df_selected$lat == ""), ]
    df_selected <- df_selected[!is.na(df_selected$lon) & !is.na(df_selected$lat), ]
    if(inherits(roi, "sf")){
      df_sf <- sf::st_as_sf(df_selected, coords = c("lon", "lat"), crs = 4326)
      inside <- sf::st_within(df_sf, roi, sparse = FALSE)
      df_selected <- df_selected[inside, ]
    }
    df_selected <- df_selected[, col.c14baz]
    df.all <- rbind(df.all, df_selected)
  }
  return(df.all)
}