#' @name neo_dbs_3rdpart_parse
#'
#' @description Collect radiocarbon dates from an external dataset
#'
#' @param l.dbs A vector of radiocarbon datasets listed in c14bazAAR.
#' @param col.c14baz A vector of field names to collect from the `l.dbs` datasets.
#' @param present A date for the present, to calibrate from BP (1950). Default: 1950.
#' @param chr.interval.uncalBC A vector of two BC dates of chronological bounds to subset the radiocarbon dates selection.
#' @param roi A `sf` polygon to subset the radiocarbon dates selection.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A dataframe of standardized radiocarbon dates.
#'
#' @examples
#'
#'                    
#' @export
neo_dbs_3rdpart_parse <- function(file.path = "C:/Rprojects/neonet/doc/references/brami15/db_data/12520_2014_193_MOESM1_ESM.xlsx",
                                  sourcedb = "brami15",
                                  db_period = "EN",
                                  Period = "EN",
                                  db_culture = NA,
                                  colors = NA,
                                  drop.sitenames.equiv.coords = FALSE,
                                  sitenames.equiv = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/c14_corrected_sitenames.geojson",
                                  col.neonet = c("SiteName", "LabCode", "C14Age", "C14SD"),
                                  col.3rdpart = c("Site", "Lab.no.", "Date.BP", "Interval.BP"),
                                  text.to.rm = c('\\(1\\)|\\(2\\)|\\(3\\)'),
                                  present = 1950, 
                                  chr.interval.uncalBC = NA, 
                                  roi = NA,
                                  verbose = TRUE){
  `%>%` <- dplyr::`%>%`
  if (length(col.3rdpart) != length(col.neonet)){
    stop("'col.neonet' and 'col.3rdpart' should have the same length for the mapping")
  }
  c14_3rdpart <- openxlsx::read.xlsx(file.path)
  # rename columns
  for(i in seq(1, length(colnames(c14_3rdpart)))){
    if(colnames(c14_3rdpart)[i] %in% col.3rdpart){
      idx <- match(colnames(c14_3rdpart)[i], col.3rdpart)
      colnames(c14_3rdpart)[i] <- col.neonet[idx]
    }
  }
  # clean extra spaces
  c14_3rdpart$SiteName <- trimws(c14_3rdpart$SiteName)
  # drop all non used columns
  c14_3rdpart <- c14_3rdpart[ , col.neonet]
  # remove bad text pattern in the whole dataset
  c14_3rdpart <- c14_3rdpart.clean %>%
    dplyr::mutate(across(everything(), ~ gsub(text.to.rm, "", .)))
  c14_3rdpart <- na.omit(c14_3rdpart)
  # head(c14_3rdpart)
  source("R/neo_calib.R")
  df.c14 <- neo_calib(c14_3rdpart,
                      stat.mean = TRUE) # mean is useful?
  # clean extra spaces
  df.c14$SiteName  <- trimws(df.c14$SiteName)
  # Add supp data
  df.c14$sourcedb <- sourcedb
  df.c14$db_period <- db_period
  df.c14$db_culture <- db_culture
  df.c14$Period <- Period
  df.c14$colors <- colors
  df.c14$mean <- NULL
  
  sitenames <- sf::st_read(sitenames.equiv)
  sitenames$AlternativeNames <- paste0(sitenames$AlternativeNames, " | ", sitenames$SiteName)
  sitenames <- sitenames %>%
    tidyr::separate_rows(AlternativeNames, sep = "\\|")  # Use double escape for the pipe character
  # clean extra spaces
  sitenames$AlternativeNames <- trimws(sitenames$AlternativeNames)
  coordinates <- as.data.frame(sf::st_coordinates(sitenames))
  colnames(coordinates) <- c('lon', 'lat')
  sitenames <- sf::st_drop_geometry(sitenames)
  sitenames <- cbind(sitenames, coordinates)
  sitenames <- sitenames[!duplicated(sitenames), ]
  # sitenames <- sitenames[!is.na(sitenames$AlternativeNames), ]
  
  # Perform a left join to replace SiteName in df.c14 with SiteName from sitenames when there's a match on AlternativeNames
  df <- df.c14 %>%
    dplyr::left_join(sitenames, by = c("SiteName" = "AlternativeNames"), relationship = "many-to-many") %>%
    dplyr::mutate(SiteName = dplyr::coalesce(SiteName.y, SiteName)) %>%
    dplyr::select(-SiteName.y)
  
  # 
  # source("R/neo_dbs_sitename_dates.R")
  # df.c14.coords <- neo_dbs_sitename_dates(df.c14,
  #                                         drop.sitenames.equiv.coords = drop.sitenames.equiv.coords)
  # 
  # site.coordinates <- sf::st_read("https://raw.githubusercontent.com/zoometh/neonet/refs/heads/main/inst/extdata/c14_corrected_sitenames.geojson")
  # 
  
  if(verbose){
    print("Reorder the columns")
  }
  # df.c14 <- df.c14 %>%
  #   dplyr::select(everything(), tpq, taq)
  df.c14.reodered <- df %>% 
    dplyr::relocate(sourcedb, .before = SiteName) %>%
    dplyr::relocate(db_period, .after = C14SD) %>%
    dplyr::relocate(db_culture, .after = db_period) %>%
    dplyr::relocate(Period, .after = db_culture) %>%
    dplyr::relocate(lon, .after = Period) %>%
    dplyr::relocate(lat, .after = lon) %>%
    dplyr::relocate(colors, .after = lat) %>%
    dplyr::relocate(median, .after = colors)
  # head(df.c14.reodered)
  # head(samp_df)
 
  return(df.c14.reodered)
}