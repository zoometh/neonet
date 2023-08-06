#' @name neo_subset
#'
#' @description subset the dataset according to NeoNet span of time and periods
#'
#' @param df.c14 the original XLSX with neonet columns (SiteName, Period, etc.)
#' @param ref.bp time interval
#' @param ref.period period referenced in NeoNet (and colors). A TSV file.
#' @param ref.spat selected ROI. A GeoJSON file.
#' @param rm.C14Age if TRUE remove dates not within `ref.bp` time interval. Default TRUE
#' @param rm.Period if TRUE will remove dates having period not listed in the ref.period file. Default: FALSE.
#' @param rm.Spatial if TRUE will remove dates not within the ROI. Default: FALSE, since the GeoJSON file of the ROI could be inaccurate (specially for the coastline)
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return df.c14 with checked values
#'
#' @examples
#'
#' # read dataset
#' data.c14 <- paste0(path.data, "NeoNet_atl_ELR (1).xlsx")
#' df.c14 <- openxlsx::read.xlsx(data.c14)
#' df.c14 <- df.c14[df.c14$Country == "France", ]
#' df.c14 <- as.data.frame(apply(df.c14, 2, trimws))
#'
#' # call function
#' neo_subset(df.c14)
#'
#'
#' @export
neo_subset <- function(df.c14,
                       ref.c14age = c(9000, 5000),
                       ref.period = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/periods.tsv",
                       ref.spat = "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/wsh_atl.geojson",
                       rm.C14Age = TRUE,
                       rm.Period = FALSE,
                       rm.Spatial = FALSE,
                       verbose = TRUE,
                       verbose.C14Age = TRUE,
                       verbose.Period = TRUE,
                       verbose.Coords = TRUE){
  all.dates <- nrow(df.c14)
  c14.err.time <- c("Country", "SiteName", "Period", "C14Age", "C14SD", "bib_url")
  c14.err.spat <- c("Country", "SiteName", "Period", "Latitude", "Longitude", "bib_url")
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  df.c14 <- as.data.frame(apply(df.c14, 2, trimws))
  if(verbose){print(paste0("nb dates usable for NeoNet dataset: ", nrow(df.c14)))}
  if(verbose){print(".. C14Age")}
  df.c14[ , "C14Age"] <- as.numeric(df.c14[ , "C14Age"])
  df.c14[ , "C14SD"] <- as.numeric(df.c14[ , "C14SD"])
  if(verbose){
    nb.before <- nrow(df.c14)
    C14Age.na <- df.c14[is.na(df.c14[ , "C14Age"]) | is.na(df.c14[ , "C14SD"]), ]
    # print("NA values in the C14Age column:")
    # print(table(is.na(df.c14[ , "C14Age"])))
    # print("NA values in the C14SD column:")
    # print(table(is.na(df.c14[ , "C14SD"])))
    df.c14 <- df.c14[!is.na(df.c14[ , "C14Age"]) & !is.na(df.c14[ , "C14SD"]), ]
    print(paste0("removed ", nb.before - nrow(df.c14), " date(s) having  NA in C14Age or C14SD"))
  }
  if(verbose.C14Age){print(C14Age.na[ , c14.err.time])}
  if(verbose){
    print(paste0("nb dates usable for NeoNet dataset: ", nrow(df.c14)))
  }
  C14Age.too.old <- df.c14[df.c14$C14Age > ref.c14age[1], ]
  C14Age.too.recent <- df.c14[df.c14$C14Age < ref.c14age[2], ]
  C14.not.in.time.span <- rbind(C14Age.too.old, C14Age.too.recent)
  nb.before <- nrow(df.c14)
  df.c14 <- df.c14[df.c14$C14Age <= ref.c14age[1] & df.c14$C14Age >= ref.c14age[2], ]
  if(verbose){
    print(paste0("removed ", nb.before - nrow(df.c14), " date(s) not between ", ref.c14age[1], " and ", ref.c14age[2],""))
  }
  if(verbose.C14Age){print(C14.not.in.time.span[order(C14.not.in.time.span$C14Age), c14.err.time])}
  if(verbose){print(".. Periods")}
  periods.colors <- read.csv(ref.period, sep = "\t")
  not.ref <- setdiff(df.c14$Period, periods.colors$period)
  # if(verbose){
  #   print("these periods will be removed")
  #   cat(sort(not.ref), sep = ", ")
  # }
  if(rm.Period){
    df.c14 <- df.c14[!(df.c14$Period %in% not.ref), ]
    if(verbose){
      print("these periods have been removed")
      cat(sort(not.ref), sep = ", ")
      print(paste0("nb dates usable for NeoNet dataset: ", nrow(df.c14)))
    }
  }
  if(verbose){print(".. Spatial coordinates")}
  sf::sf_use_s2(FALSE)
  roi <- sf::st_read(ref.spat, quiet = TRUE)
  sf::st_crs(roi) <- 4326
  df.c14 <- sf::st_as_sf(df.c14, coords = c("Longitude", "Latitude"))
  sf::st_crs(df.c14) <- 4326
  
  df.c14 <- df.c14 %>% dplyr::mutate(
    intersection = as.integer(sf::st_intersects(df.c14, roi)))
  # area = if_else(is.na(intersection), '', ws_roi.shp$cat1[intersection])
  # )
  df.c14.outside.roi <- df.c14[is.na(df.c14$intersection), ]
  if(nrow(df.c14.outside.roi) > 0){
    df.c14.outside.roi[ , c("Longitude", "Latitude")] <- sf::st_coordinates(df.c14.outside.roi)
    if(verbose.Coords){
      # convert bbox to sf
      mbr.outside <- sf::st_as_sf(
        sf::st_as_sfc(
          sf::st_bbox(df.c14.outside.roi)
        )
      )
      # class(mbr.outside)
      roi.samp <- sf::st_intersection(roi, mbr.outside)
      map.out <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = roi.samp) +
        ggplot2::geom_sf(data = df.c14.outside.roi, fill = 'red') +
        ggplot2::theme_bw()
      print(map.out)
      df.c14.outside.roi <- as.data.frame(df.c14.outside.roi)
      print(df.c14.outside.roi[order(df.c14.outside.roi$SiteName), c14.err.spat])
    }
  }

  # df.c14.outside.roi[ , c("Longitude", "Latitude")] <- sf::st_coordinates(df.c14.outside.roi)
  # save only intersection = 1
  if(rm.Spatial){
    df.c14 <- df.c14[!is.na(df.c14$intersection), ]
  }
  df.c14[ , c("Longitude", "Latitude")] <- sf::st_coordinates(df.c14)
  df.c14$geometry <- df.c14$intersection  <- NULL
  if(verbose){
    print("Intersection with ROI has been done")
    print(paste0("nb dates usable for NeoNet dataset: ", nrow(df.c14), " (initially: ", all.dates, " dates)"))
  }
  return(df.c14)
}

# # read dataset
# data.c14 <- paste0(path.data, "NeoNet_atl_ELR (1).xlsx")
# df.c14 <- openxlsx::read.xlsx(data.c14)
# df.c14 <- df.c14[df.c14$Country == "France", ]
# df.c14 <- as.data.frame(apply(df.c14, 2, trimws))
#
# # call function
# df.c14 <- neo_subset(df.c14)
