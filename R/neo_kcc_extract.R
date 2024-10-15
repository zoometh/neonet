#' @name neo_kcc_extract
#'
#' @description Extract Koppen Climate Classes (KCC) from one or different KCC SpatRaster (`kcc_file`) using the weighted median of calibrated radiocarbon date form a `sf` dataframe of dates (`df.c14`). For example KCC 6k (i.e. -6000) entails dates having their medians between -6500 and -5500 calBC.
#' 
#' @param df.c14 A `sf` dataframe of dates.
#' @param present KCC are always in cal BP with present = 2000.
#' @param root.path The path to the `kcc.file` parent folder.
#' @param kcc.file Koppen Climate Classes (KCC) SpatRaster (i.e. GeoTiffs) generated with the `xxx()` function.
#' @param kcc.step The time interval btw KCC. This interval will be divided by two to create discretisation centered around a date. Default: 1000
#' @param ref.period period referenced in NeoNet (and colors). A TSV file.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return An `sf` object
#'
#' @examples
#'
#' df.c14 <- neo_calib(df.c14)
#' df.c14 <- sf::st_as_sf(df.c14, coords = c("lon", "lat"), crs = 4326)
#' kcc.file <- c("koppen_6k.tif", "koppen_7k.tif", "koppen_8k.tif",
#'               "koppen_9k.tif", "koppen_10k.tif", "koppen_11k.tif")
#' df_cc <- neo_kcc_extract(df.c14 = df.c14, kcc.file = kcc.file)
#'
#' @export
neo_kcc_extract <- function(df.c14 = NA,
                            present = 2000,
                            root.path = "C:/Rprojects/neonet/doc/data/clim/",
                            labcode.col = "LabCode",
                            kcc.file = c("koppen_6k.tif", "koppen_7k.tif", "koppen_8k.tif",
                                         "koppen_9k.tif", "koppen_10k.tif", "koppen_11k.tif"),
                            kcc.step = 1000,
                            verbose = TRUE){
  # 
  for(ky in kcc.file){
    # ky <- kcc.file[3]
    if(verbose){
      print(paste0("*read: ", ky, " (BP)"))
    }
    kcc <- paste0(root.path, ky)
    cc.ky <- DescTools::SplitPath(ky)$filename
    # convert 6k to 6000
    cc.ky.int <- sub(".*_", "", cc.ky)
    cc.ky.int.bp <- -as.integer(gsub("k", "000", cc.ky.int))
    cc.ky.int.bc <- cc.ky.int.bp + present
    # TODO: more simply.. write a round()
    cc.ky.int.interval <- c(cc.ky.int.bc - (kcc.step)/2, 
                            cc.ky.int.bc + (kcc.step)/2)
    ################################################
    if(verbose){
      print(paste0("  sample calibrated dates having their median within ",
                   cc.ky.int.interval[1], " and ", cc.ky.int.interval[2], " (BC)"))
    }
    df.c14.exist.in.ky <- subset(df.c14, median >= cc.ky.int.interval[1] & median <= cc.ky.int.interval[2])
    # head(df.c14.exist.in.ky)
    if(verbose){
      print(paste0("  nb of dates/kcc: ", nrow(df.c14.exist.in.ky)))
    }
    kcc_geo <- terra::rast(kcc)
    
    # print(class(df.c14.exist.in.ky))
    
    sf.c14.exist.in.ky <- sf::st_as_sf(df.c14.exist.in.ky, coords = c("lon", "lat"), crs = 4326)
    
    # print(class(sf.c14.exist.in.ky))
    
    df_cc <- terra::extract(kcc_geo, sf.c14.exist.in.ky)
    df.c14.exist.in.ky[[cc.ky]] <- df_cc[["code"]]
    # merge on LabCode
    df.c14.exist.in.ky <- df.c14.exist.in.ky[, c(labcode.col, cc.ky)]
    
    # print(nrow(df.c14.exist.in.ky))
    
    df.c14.exist.in.ky <- sf::st_set_geometry(df.c14.exist.in.ky, NULL)
    df.c14 <- merge(df.c14, df.c14.exist.in.ky, by = labcode.col, all.x = TRUE)
    # df_cc <- df_cc %>%
    #   dplyr::rename(!!cc.ky := code)
    # append a column
    # df.c14[[cc.ky]] <- df_cc[["code"]]
    # df.c14 <- cbind(df.c14, df_cc[[cc.ky]])
  }
  return(df.c14)
}