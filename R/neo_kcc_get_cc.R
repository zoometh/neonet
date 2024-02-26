# extract CC from one or mainy KCC SpatRaster (kcc_file) and a sf dataframe of dates (df.c14)
# kcc.step is the interval btw KCC. This interval will be divied by two to create discretisation centered around a date. For example KCC 6k = -6000 entails dates from -6500 to -5500 cal BC
# KCC are always in cal BP with present = 2000
neo_kcc_get_cc <- function(root.path = "C:/Rprojects/neonet/doc/data/clim/",
                           present = 2000,
                           kcc.file = c("koppen_6k.tif", "koppen_7k.tif", "koppen_8k.tif",
                                        "koppen_9k.tif", "koppen_10k.tif", "koppen_11k.tif"),
                           kcc.step = 1000,
                           df.c14 = NA){
  # 
  for(ky in kcc.file){
    # ky <- kcc.file[3]
    print(paste0("*read: ", ky, " (BP)"))
    kcc <- paste0(root.path, ky)
    cc.ky <- DescTools::SplitPath(ky)$filename
    # convert 6k to 6000
    cc.ky.int <- sub(".*_", "", cc.ky)
    cc.ky.int.bp <- -as.integer(gsub("k", "000", cc.ky.int))
    cc.ky.int.bc <- cc.ky.int.bp + present
    cc.ky.int.interval <- c(cc.ky.int.bc - (kcc.step)/2, 
                            cc.ky.int.bc + (kcc.step)/2)
    print(paste0("  sample calibrated dates having their median within ",
                 cc.ky.int.interval[1], " and ", cc.ky.int.interval[2], " (BC)"))
    df.c14.exist.in.ky <- subset(df.c14, median >= cc.ky.int.interval[1] & median <= cc.ky.int.interval[2])
    head(df.c14.exist.in.ky)
    print(paste0("  nb of dates/kcc: ", nrow(df.c14.exist.in.ky)))
    kcc_geo <- terra::rast(kcc)
    sf.c14.exist.in.ky <- sf::st_as_sf(df.c14.exist.in.ky, coords = c("lon", "lat"), crs = 4326)
    df_cc <- terra::extract(kcc_geo, sf.c14.exist.in.ky)
    df.c14.exist.in.ky[[cc.ky]] <- df_cc[["code"]]
    # merge on LabCode
    df.c14.exist.in.ky <- df.c14.exist.in.ky[, c("LabCode", cc.ky)]
    df.c14.exist.in.ky <- sf::st_set_geometry(df.c14.exist.in.ky, NULL)
    df.c14 <- merge(df.c14, df.c14.exist.in.ky, by = "LabCode", all.x = TRUE)
    # df_cc <- df_cc %>%
    #   dplyr::rename(!!cc.ky := code)
    # append a column
    # df.c14[[cc.ky]] <- df_cc[["code"]]
    # df.c14 <- cbind(df.c14, df_cc[[cc.ky]])
  }
  return(df.c14)
}





# kcc = "C:/Rprojects/neonet/doc/data/clim/koeppen_7k.tif"