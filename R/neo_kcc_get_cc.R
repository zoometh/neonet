# extract CC from one or mainy KCC SpatRaster (kcc_file) and a sf dataframe of dates (df.c14)

neo_kcc_get_cc <- function(root.path = "C:/Rprojects/neonet/doc/data/clim/",
                           kcc_file = c("koeppen_6k.tif", "koeppen_7k.tif", "koeppen_8k.tif"),
                           df.c14 = NA){
  for(ky in kcc_file){
    kcc <- paste0(root.path, ky)
    kcc_geo <- terra::rast(kcc)
    df_cc <- terra::extract(kcc_geo, df.c14)
    print(paste0("*read: ", ky))
    print(paste0("  nb of dates/kcc: ", nrow(df_cc)))
    cc.ky <- DescTools::SplitPath(ky)$filename
    # df_cc <- df_cc %>%
    #   dplyr::rename(!!cc.ky := code)
    df.c14[[cc.ky]] <- df_cc[["code"]]
    # df.c14 <- cbind(df.c14, df_cc[[cc.ky]])
  }
  return(df.c14)
}



xxx <- head(df.c14, 12)
df_cc <- neo_kcc_get_cc(df.c14 = xxx)
neo_kcc_sankey(df_cc, col.req = names(renaming_vector))

# kcc = "C:/Rprojects/neonet/doc/data/clim/koeppen_7k.tif"