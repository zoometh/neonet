#' @name neo_kcc_legend
#'
#' @description Grab the legend of KCC to get only existing KCC. Convert this legend into a grob
#'
#' @param df_cc A dataframe with KCC classes previously extracted with `neo_kcc_extract()` .
#' @param kcc.file A path to a Koppen map (Tif). KCC values will be extracted directly from this map. Useful when called by the function `neo_isochr`. Default NA.
#' @param where An sf dataframe to clip the Koppen map (`kcc.file` argument). Useful when called by the function `neo_isochr`. Default NA.
#' @param kcc_df A dataframe for the long legend. Useful when `long.legend` is TRUE.
#' @param long.legend If TRUE, concatenate the KCC with its description (ex: "BSk Arid, steppe, cold"). Default: FALSE. 
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A ggplot.
#'
#' @examples
#'
#' kcc.legend <- neo_kcc_legend(df_cc = df_cc, 
#'                              long.legend = TRUE)
#'                    
#' @export
neo_kcc_legend <- function(df_cc = NA,
                           kcc.file = NA,
                           where = NA,
                           kcc_df = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/koppen.tsv",
                           long.legend = FALSE,
                           verbose = TRUE){
  `%>%` <- dplyr::`%>%`
  source("R/config.R")
  if(inherits(df_cc, "sf")){
    if(verbose){
      print(paste0("Reads a 'sf' dataframe"))
    }
    df <- sf::st_set_geometry(df_cc, NULL)
    df <- df[, col.req]
    selected.kcc <- na.omit(unique(unlist(df)))
  }
  if(!inherits(df_cc, "sf")){
    if(verbose){
      print(paste0("Select KCC directly on the map"))
    }
    # where <- sf::st_read(where,
    #                      quiet = TRUE)
    # 
    kcc_geo <- terra::rast(kcc.file)
    # print(class(kcc_geo))
    # print("XXXXX")
    # if(!is.na(where)){
    clipped_raster <- terra::mask(kcc_geo, where)
    # kcc_geo <- terra::mask(kcc_geo, where)
    # print(class(clipped_raster))
    # }
    values_vector <- terra::extract(clipped_raster, where)
    # values_vector <- terra::extract(kcc_geo, where)
    # print(class(kcc_geo))
    # values_vector <- terra::extract(kcc_geo)
    # values_vector <- terra::values(clipped_raster)  # This extracts all pixel values
    selected.kcc <- na.omit(values_vector$code)
    
    # df <- sf::st_set_geometry(df_cc, NULL)
    # df <- df[, col.req]
    
    # selected.kcc <- na.omit(unique(unlist(df)))
  }
  
  selected.kcc <- factor(selected.kcc, levels = unique(selected.kcc))
  # a new empty df
  climate_df <- data.frame(
    kcc = names(kcc_colors),
    color = kcc_colors
  )
  # if(is.factor(selected.kcc)){
  #   climate_df <- climate_df[climate_df$kcc %in% selected.kcc, ]
  # }
  climate_df <- climate_df[climate_df$kcc %in% selected.kcc, ]
  if(long.legend){
    koppen.tsv <- read.csv2(kcc_df, sep = "\t")
    climate_df <- merge(climate_df, koppen.tsv, by.x = "kcc", by.y = "code", all.x = TRUE)
    climate_df$kcc <- paste(climate_df$kcc, climate_df$value)
    climate_df$color <- climate_df$color.x
    climate_df <- climate_df[ , c("kcc", "color")]
  }
  # # Plot
  # gout <- ggplot2::ggplot(climate_df, ggplot2::aes(x = kcc, y = 1, fill = factor(kcc))) + 
  #   ggplot2::geom_bar(stat = "identity") +
  #   ggplot2::scale_fill_manual(values = climate_df$color, name = 'Koppen Climates Classes') +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
  #   ggplot2::labs(x = NULL, y = NULL) + 
  #   ggplot2::theme_void()
  # # gout
  # tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(gout))
  # leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  # legend.grob <- tmp$grobs[[leg]]
  # plot <- ggplot2::ggplot() +
  #   ggplot2::geom_blank(ggplot2::aes(1, 1)) +
  #   ggplot2::theme_void()  # Use theme_void() to create an empty canvas
  # legend <- plot +
  #   ggplot2::annotation_custom(
  #     grob = legend.grob, 
  #     xmin = -Inf, xmax = Inf, 
  #     ymin = -Inf, ymax = Inf
  #   )
  # # print(final_plot)
  # return(legend)
  # # return(gout)
  # Plot
  gout <- ggplot2::ggplot(climate_df, ggplot2::aes(x = kcc, y = 1, fill = factor(kcc))) + 
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = climate_df$color, name = 'Koppen Climates Classes') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::labs(x = NULL, y = NULL) + 
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),  # Set all legend box margins to zero using margin()
      legend.margin = ggplot2::margin(0, 0, 0, 0)       # Set no additional margin around the legend
    )
  
  # Extract legend as before
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(gout))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend.grob <- tmp$grobs[[leg]]
  
  # Ensure the legend is placed without margins in the final annotation
  plot <- ggplot2::ggplot() +
    ggplot2::geom_blank(ggplot2::aes(1, 1)) +
    ggplot2::theme_void()  # Use theme_void() to create an empty canvas
  legend <- plot +
    ggplot2::annotation_custom(
      grob = legend.grob, 
      xmin = -Inf, xmax = Inf, 
      ymin = -Inf, ymax = Inf
    )
  
  return(legend)
  
}