#' @name neo_datasum
#'
#' @description summarise dataset: spatial and chronological extend, n dates, etc.
#'
#' @param df.c14 the original XLSX with neonet columns (SiteName, Period, etc.) with with checked values (see: neo_subset)
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A plot or a file
#'
#' @examples
#'
#'
#'
#' @export
neo_datasum <- function(df.c14,
                        info = c("stats", "maps"),
                        roi = "C:/Rprojects/neonet/doc/data/wsh_atl.geojson", # TO change
                        # roi2 = "C:/Rprojects/neonet/doc/data/wsh_med.geojson", # TO change
                        col.used = c("SiteName", "Period", "PhaseCode",
                                     "LabCode", "C14Age", "C14SD",
                                     "Material", "MaterialSpecies",
                                     "tpq", "taq",
                                     "bib", "bib_url",
                                     "Longitude", "Latitude", "Country"),
                        shown.per = c("EM", "MM", "LM", "EN", "MN", "LN"),
                        missing.values = c("", "n/a", NA),
                        ref.period = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/periods.tsv",
                        verbose = TRUE){
  # df.c14 <- read.csv2("C:/Rprojects/neonet/inst/extdata/id00164_doc_elencoc14.tsv", sep = "\t") ; roi = "C:/Rprojects/neonet/doc/data/wsh_atl.geojson"
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  
  periods.colors <- read.csv(ref.period, sep = "\t")
  ordered.periods <- periods.colors$period
  # num_cells_with_values <- sum(df.c14 %in% missing.values, na.rm = TRUE)
  # df.c14[df.c14 %in% missing.values, ] <- NA
  df.c14[df.c14 %in% c("n/a", "n/d", "")] <- NA
  if("stats" %in% info){
    linfos <- list()
    n.dates <- nrow(df.c14) # number of dates
    n.sites <- length(unique(df.c14$SiteName)) # number of dates
    geo.extent <- list(N = max(as.numeric(df.c14$Latitude)), # NSEW extent
                       S = min(as.numeric(df.c14$Latitude)),
                       E = max(as.numeric(df.c14$Longitude)),
                       W = min(as.numeric(df.c14$Longitude))
    )
    time.extent <- list(tpq = min(df.c14$tpq),
                        taq = max(df.c14$taq))
    # missing
    nb.missing <- sum(is.na(df.c14[ , col.used]))
    nb.values <- nrow(df.c14) * length(col.used)
    perc.missing <- nb.missing/nb.values
    perc.missing <- paste0(as.character(
      as.integer((nb.missing/nb.values)*100)), "%")
    
    df.c14$context <- paste0(df.c14$SiteName,"-",df.c14$PhaseCode)
    n.context <- length(unique(df.c14$context))
    n.missing.context <- nrow(df.c14[df.c14$PhaseCode %in% missing.values, ])
    perc.missing.context <- paste0(as.character(
      as.integer((n.missing.context/nrow(df.c14))*100)), "%")
    
    n.missing.material <- nrow(df.c14[df.c14$Material %in% missing.values, ])
    perc.missing.material <- paste0(as.character(
      as.integer((n.missing.material/nrow(df.c14))*100)), "%")
    
    n.missing.materialspecies <- nrow(df.c14[df.c14$MaterialSpecies %in% missing.values, ])
    perc.missing.materialspecies <- paste0(as.character(
      as.integer((n.missing.materialspecies/nrow(df.c14))*100)), "%")
    
    df.per <- as.data.frame(table(df.c14$Period))
    names(df.per)[names(df.per) == 'Var1'] <- 'Period'
    # setdiff(df.per$Var1, ordered.periods)
    df.per <- df.per[match(ordered.periods, df.per$Period), ]
    
    n.bib <- length(unique(df.c14$bib_url))
    
    linfos <- c(linfos,
                n.dates = n.dates,
                n.sites = n.sites,
                n.context = n.context,
                n.bib = n.bib,
                geo.extent = geo.extent,
                time.extent = time.extent,
                perc.missing = perc.missing,
                perc.missing.context = perc.missing.context,
                perc.missing.material = perc.missing.material,
                perc.missing.materialspecies = perc.missing.materialspecies)
    print(str(linfos))
    print(df.per)
  }
  if("maps" %in% info){
    df.c14 <- read.csv("C:/Rprojects/neonet/inst/extdata/id00164_doc_elencoc14.tsv", sep = "\t")
    # /!\ this site belongs to Med
    df.c14 <-  df.c14[df.c14$SiteName != "Cueva de Murcielagos de Albunol", ]
    periods.colors <- read.csv(ref.period, sep = "\t")
    periods.colors.selected <- periods.colors[periods.colors$period %in% shown.per, c("period", "color")]
    periods.colors.selected <- rbind(periods.colors.selected, c("others", "#808080"))
    # fetch not listed periods (others)
    unshown.per <- as.character(unlist(unique(df.c14[!(df.c14$Period %in% shown.per), "Period"])))
    if(verbose & length(unshown.per) > 0){
      print("These periods will be gathered into 'other':")
      cat(unshown.per, sep = ", ")
    }
    df.c14 <- within(df.c14, Period[Period %in% unshown.per] <- 'others')
    df.c14 <- merge(df.c14, periods.colors.selected, by.x = "Period", by.y = "period", all.x = T)
    df.c14 <- df.c14 %>% 
      dplyr::arrange(factor(Period, levels = shown.per))
    # unique(df.c14$color)
    # world map
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    # roi map
    roi.sf <- sf::st_read(roi)
    Xs <- df.c14$Longitude <- as.numeric(df.c14$Longitude)
    Ys <- df.c14$Latitude <- as.numeric(df.c14$Latitude)
    buff <- .4
    bbox <- c(left = min(Xs) - buff, 
              bottom = min(Ys) - buff, 
              right = max(Xs) + buff, 
              top = max(Ys) + buff)
    bas.leg <- data.frame (xmin = 1, xmax = 2.5, ymin = 36, ymax = 37)
    mid.leg <- data.frame (xmin = 1, xmax = 2.5, ymin = 37.5, ymax = 38.5)
    top.leg <- data.frame (xmin = 1, xmax = 2.5, ymin = 39, ymax = 40)
    a.size <- 3
    a.alpha <- .5
    
    lg <- list()
    for(per in unique(df.c14$Period)){
      # per <- "others"
      print(per)
      df.c14.per <- df.c14[df.c14$Period == per, ]
      coloramp <- periods.colors.selected[periods.colors.selected$period == per, "color"]
      colfunc <- colorRampPalette(c("white", coloramp))(4)
      colfunc <- colfunc[2:4]
      tit <- paste0(periods.colors[periods.colors$period == per, "period_full_name"],
                   " (", per, ") - ", nrow(df.c14.per), " dates")
      df.c14.sf <- sf::st_as_sf(x = df.c14.per,                         
                            coords = c("Longitude", "Latitude"),
                            crs = 4326)
      skde1 <- eks::st_kde(df.c14.sf)

      
      map <- ggplot2::ggplot(world) +
        ggplot2::geom_sf(color = "darkgrey", fill = "white") +
        ggplot2::ggtitle(tit) +
        ggplot2::ggtitle(tit) +  
        ggplot2::geom_sf(data = roi.sf, fill = "white", 
                         inherit.aes = FALSE) +
        # TODO: ad dlegend
        ggplot2::geom_sf(data=eks::st_get_contour(skde1), 
                         colour=colfunc, 
                         fill=colfunc,
                         alpha = a.alpha) + 
        # ggplot2::geom_sf(data = eks::st_get_contour(skde1, cont = c(25, 50, 75)), colour = colfunc, 
        #                               fill = colfunc,
        #                               alpha = .5) + 
        # ggplot2::geom_sf(data = eks::st_get_contour(skde1, cont = c(25, 50, 75)), 
        #                  ggplot2::aes(colour = colfunc, 
        #                  fill = colfunc,
        #                  alpha = .5)) + 
        ggplot2::geom_point(data = df.c14.per, 
                            ggplot2::aes(x = Longitude, y = Latitude, color = color)) +
        ggplot2::coord_sf(xlim = c(bbox[["left"]], 
                                   bbox[["right"]]), 
                          ylim = c(bbox[["bottom"]], 
                                   bbox[["top"]])) +
        ggplot2::annotate("rect", 
                 fill = colfunc[1], 
                 color = "black", 
                 xmin = bas.leg$xmin,
                 xmax = bas.leg$xmax, 
                 ymin = bas.leg$ymin, 
                 ymax = bas.leg$ymax,
                 alpha = a.alpha) +
        ggplot2::annotate("text", 
                 x = bas.leg$xmax+1, 
                 y = bas.leg$ymin+.5,
                 size = a.size,
                 label = " 75%") +
        
        ggplot2::annotate("rect", 
                 fill = colfunc[2], 
                 color = "black", 
                 xmin = mid.leg$xmin,
                 xmax = mid.leg$xmax, 
                 ymin = mid.leg$ymin, 
                 ymax = mid.leg$ymax,
                 alpha = a.alpha) +
        ggplot2::annotate("text", 
                 x = mid.leg$xmax+1, 
                 y = mid.leg$ymin+.5,
                 size = a.size,
                 label = " 50%") +
        
        ggplot2::annotate("rect", 
                 fill = colfunc[3], 
                 color = "black", 
                 xmin = top.leg$xmin,
                 xmax = top.leg$xmax, 
                 ymin = top.leg$ymin, 
                 ymax = top.leg$ymax,
                 alpha = a.alpha) +
        ggplot2::annotate("text", 
                 x = top.leg$xmax+1, 
                 y = top.leg$ymin+.5,
                 size = a.size,
                 label = " 25%") +
        ggplot2::scale_colour_identity() +
        # ggplot2::guides(colour = ggplot2::guide_legend(title="Species")) +
        ggplot2::theme_bw() 
      lg[[length(lg)+1]] <- map
    }
    margin <- ggplot2::theme(plot.margin = ggplot2::unit(c(.2, -.1, .2, -.1), "cm"))
    ggplot2::ggsave(file = "C:/Rprojects/neonet/results/_by_periods.png", 
                    gridExtra::arrangeGrob(grobs = lapply(lg, "+", margin), ncol = 2),
                    height = 18, width = 8)
  }
}

