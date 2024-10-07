#'
#' @export
neo_kcc_ca <- function(df_cc = NA,
                       col.req = NA,
                       selected.per = c("MM", "LM", "EN", "MN"),
                       colname.period = "Period",
                       kcc.col = c("koppen_11k.tif", "koppen_10k.tif", "koppen_9k.tif",
                                   "koppen_8k.tif", "koppen_7k.tif", "koppen_6k.tif"),
                       present = 2000,
                       ref.period = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/periods.tsv",
                       verbose = TRUE){
  # df_cc_ <- as.data.frame(df_cc)
  source("R/config.R")
  `%>%` <- dplyr::`%>%`
  kcc.col <- gsub(".tif", "", kcc.col)
  source("R/config.R")
  if(inherits(df_cc, "sf")){
    if(verbose){
      print(paste0("Reads a 'sf' dataframe"))
    }
    df <- sf::st_set_geometry(df_cc, NULL)
  }
  if(is.data.frame(df_cc) & !inherits(df_cc, "sf")){
    if(verbose){
      print(paste0("Reads a simple dataframe"))
    }
    df <- df_cc
  }
  df <- df[df[[colname.period]] %in% selected.per, ]
  
  # for(per in selected.per){}
  # df <- df[ , which(names(df) %in% col.req)]
  df_list <- list()
  for (kcc in kcc.col){
    # kcc <- "koppen_9k"
    # df.temp <- df[, c("LabCode", kcc)]
    df.temp <- df[, c(colname.period, kcc)]
    data.disj <- ade4::acm.disjonctif(df.temp[2])
    names(data.disj) <- gsub(paste0(kcc, "."), "", names(data.disj))
    df.tot <- cbind(df.temp, data.disj)
    df.tot[[kcc]] <- NULL
    # df_grouped <- aggregate(cbind(value1, value2) ~ group, data = df, FUN = sum)
    df_grouped <- df.tot %>%
      group_by_at(colname.period) %>%
      summarise(across(everything(), sum))
    df_grouped$kcc <- kcc
    # print(df_grouped)
    df_list[[length(df_list)+1]] <- df_grouped
  }
  g_list <- list()
  for(i in seq(1, length(df_list))){
    print(i)
    test <- df_list[[i]]
    test <- as.data.frame(test)
    rownames(test) <- test$Period
    test[, c(colname.period, "kcc")] <- NULL
    test <- test[ , colSums(test) > 0]
    if(sum(test) == 0){
      break
    }
    # CA
    ca <- FactoMineR::CA(test, graph = FALSE)            # AFC
    inertCA1 <- round(as.numeric(ca$eig[,2][1]), 1)
    inertCA2 <- round(as.numeric(ca$eig[,2][2]), 1)
    coords_ind_ca <- as.data.frame(ca$row$coord)
    coords_var_ca <- as.data.frame(ca$col$coord)
    coords_ca <- rbind(coords_ind_ca, coords_var_ca)
    colnames(coords_ca)[1] <- 'CA1'
    colnames(coords_ca)[2] <- 'CA2'
    coords_ca$id <- row.names(coords_ca)
    # merge colors of kcc and periods
    periods.colors <- read.csv(ref.period, sep = "\t")
    periods.colors <- periods.colors[periods.colors$period %in% selected.per, ]
    periods.colors <- periods.colors[ , c("period", "color")]
    names(periods.colors)[names(periods.colors) == 'period'] <- 'id'
    kcc.colors <- as.data.frame(kcc_colors)
    names(kcc.colors)[names(kcc.colors) == 'kcc_colors'] <- 'color'
    kcc.colors$id <- row.names(kcc.colors)
    all.colors <- rbind(periods.colors, kcc.colors)
    # merge
    ca.kcc.per <- merge(coords_ca, all.colors, by="id", all.x = T)
    ca.kcc.per$shape <- NA
    # assign shapes
    # kcc.per$color[is.na(depots.m$color)] <- "black"
    ca.kcc.per$shape[ca.kcc.per$id %in% selected.per] <- 19
    ca.kcc.per$shape[is.na(ca.kcc.per$shape)] <- 17
    # titre
    the.periods <- paste(selected.per, collapse = ", ")
    tit <- paste0("CA on occupied climates")
    subtit <- paste0("KCC map: ", kcc.col[i])
    capt <- paste0("Periods: ", the.periods)
    # graphique
    gca <- ggplot2::ggplot(ca.kcc.per, aes(CA1, CA2, color = color, shape = shape)) +
      ggplot2::labs(title = tit,
                    subtitle = subtit,
                    caption = capt) +
      ggplot2::geom_point(# fill = color, # pour les shape > 20
        # stroke = .5, # pour les shape > 20
        size = 3) + # 1.5
      ggrepel::geom_text_repel(aes(label = id),
                               cex=3,
                               segment.size = 0.1,
                               segment.alpha = 0.5,
                               max.overlaps = Inf) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.2, alpha = 0.3) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed",size = 0.2, alpha = 0.3) +
      ggplot2::geom_text(x = 0,
                         y = -Inf,
                         label = paste0(inertCA1,"%"),
                         vjust = -1,
                         size = 2,
                         alpha = 0.5) +
      ggplot2::geom_text(x = -Inf,
                         y = 0,
                         label = paste0(inertCA2,"%"),
                         vjust = 1,
                         angle = 90,
                         size = 2,
                         alpha = 0.5) +
      ggplot2::scale_color_identity() +
      ggplot2::scale_shape_identity() +
      ggplot2::theme(plot.title = element_text(size = 10, face = "bold"),
                     plot.subtitle = element_text(size = 8)) +
      ggplot2::theme(axis.text=element_text(size = 5),
                     axis.title.x=element_text(size = 8),
                     axis.title.y=element_text(size = 8))+
      ggplot2::theme(axis.ticks = element_line(size = 0.2))+
      ggplot2::theme(legend.position = "none")+
      ggplot2::theme(strip.text.x = element_text(size = 8),
                     strip.text.y = element_blank()) +
      ggplot2::theme(panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.2)) +
      ggplot2::theme(panel.background = element_rect(fill = 'transparent')) +
      ggplot2::theme(panel.spacing.y = unit(0, "lines"))
    g_list[[length(g_list)+1]] <- gca
  }
  return(g_list)
}
