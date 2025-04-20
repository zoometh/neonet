#' Creates a Sankey diagram
#' 
#' @name neo_kcc_sankey
#' 
#' @description Creates a Sankey diagram. To study sites Koppen Climate classes changes, run `neo_kcc_extract()` to extract the KCC values (ex: dataframe 'df_cc'). To study the whole ROI, provide different maps of the same area (before, after). By default it uses a smaller ROI 
#'
#' @param kcc_data A dataframe (sites) or a vector of two or more maps (before, after), ex: c("https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/clim/koppen_8k.tif", "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/clim/koppen_7k.tif")
#' @param col.req Columns of the KCC.
#' @param selected.per To subset on periods (ex: LM) or with the whole dataset (NA).
#'
#' @return A ggplot
#'
#' @examples
#' 
#' # For the whole area of two periods 'koppen_8k' and 'koppen_7k'
#' neo_kcc_sankey(kcc_data =  c("https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/clim/koppen_8k.tif", "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/clim/koppen_7k.tif"),
#' roi = "https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi-midi-france.geojson")
#' 
#' # For sites of two periods "EN" and "MM"
#' neo_kcc_sankey(df_cc, 
#'                col.req = col.req, 
#'                selected.per = c("EN", "MM"))
#'
#' @export
neo_kcc_sankey <- function(kcc_data = NA,
                           roi = NA,
                           col.req = NA,
                           kcc_colors = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/koppen.tsv",
                           selected.per = NA,
                           verbose = TRUE){
  # kcc_data_ <- as.data.frame(kcc_data)
  `%>%` <- dplyr::`%>%`
  koppen_colors <- read.table(kcc_colors, sep = "\t", header = TRUE)
  if(inherits(kcc_data, "sf")){
    if(verbose){
      print("Before-after changes of sites KCC")
    }
    df <- sf::st_set_geometry(kcc_data, NULL)
    if(!all(col.req %in% colnames(df))){
      stop("Field names in 'col.req' are not present in the site dataframe")
    }
    df <- df[ , which(names(df) %in% col.req)]
    if(!is.na(selected.per)){
      df <- df[df$Period %in% selected.per, ]
    }
    # for(per in selected.per){
    # per <- "LM"
    kcc_color_map <- setNames(koppen_colors$color, koppen_colors$code)
    tit <- paste0("KCC changes between ", paste0(selected.per, collapse = " - "))
    caption <- paste0("number of archaeological sites = ", nrow(df), ")")
  }
  if(inherits(kcc_data, "character")){
    if(verbose){
      print("Before-after changes of the area KCC")
    }
    source("R/neo_kcc_crop.R")
    if(verbose){
      print("Crop on ROI")
    }
    nb.stages <- length(kcc_data)
    l = list() ; stages.names <- c()
    df <- data.frame()
    for(i in seq(1, nb.stages)){
      # store maps as df
      map <- neo_kcc_crop(kcc_data[i], roi = roi)
      l[[length(l) + 1]] <- as.data.frame(map, xy = FALSE)
      # retrieve map names
      stage.name <- terra::varnames(map)
      stages.names <- c(stages.names, stage.name)
      # df[, stage.name] <- as.data.frame(map, xy = FALSE)
    }
    df <- data.frame(matrix(ncol = nb.stages, nrow = nrow(l[[1]])))
    colnames(df) <- stages.names
    # df <- data.frame(matrix(ncol = nb.stages, nrow = 0, dimnames = list(NULL, c(stages.names))))
    for(i in seq(1, nb.stages)){
      df[, i] <- l[[i]]
    }
    kcc_color_map <- setNames(koppen_colors$color, row.names(koppen_colors))
    # swap labels (ex: 15 -> Cwc)
    names(kcc_color_map) <- koppen_colors$code
    kcc_color_map2 <- setNames(row.names(koppen_colors), koppen_colors$code)
    rev_map <- setNames(names(kcc_color_map2), kcc_color_map2)
    df[stages.names] <- lapply(df[stages.names], function(x) rev_map[as.character(x)])
    tit <- paste0("KCC changes between ", paste0(stages.names, collapse = " - "))
    caption <- paste0("number of climate cells = ", nrow(df))
    # df <- sf::st_set_geometry(kcc_data, NULL)
  }
  # df <- na.omit(df)
  df.sank <- df %>%
    ggsankey::make_long(colnames(df))
  # Count node occurrences
  node_counts <- df.sank %>%
    dplyr::group_by(x, node) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")
  df.sank <- dplyr::left_join(df.sank, node_counts, by = c("x", "node"))
  gout <- ggplot2::ggplot(df.sank, ggplot2::aes(x = x,
                                                next_x = next_x,
                                                node = node,
                                                next_node = next_node,
                                                fill = factor(node),
                                                label = node)) + 
    ggsankey::geom_sankey(node.color = "black",
                          show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = kcc_color_map) +
    # ggsankey::geom_sankey_label(size = 3, color = "black", fill= "white", hjust = 0.5) +
    ggsankey::geom_sankey_label(aes(label = paste0(node, "\n(n = ", count, ")")),
                                size = 3, color = "black", fill= "white", hjust = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(title = tit,
                  caption = caption)
    # ggplot2::labs(title = tit) +
  
  if(verbose){
    print(paste0("*Sankey plot has been created"))
  }
  return(gout)
}