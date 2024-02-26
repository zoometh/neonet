
neo_kcc_sankey(df_cc = NA,
               col.req = c("sourcedb", "site", "labnr", "c14age", "c14std", "period", "culture", "lon", "lat")){
  # df_cc_ <- as.data.frame(df_cc)
  df <- sf::st_set_geometry(df_cc, NULL)
  df <- df[ , -which(names(df) %in% col.req)]
  # df <- na.omit(df)
  df.sank <- df %>%
    ggsankey::make_long(colnames(df))
  df.sank
  
  koppen_codes <- c("Af", "Am", "Aw", "BWh", "BWk", "BSh", "BSk", 
                    "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc", "Cfa", 
                    "Cfb", "Cfc", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", 
                    "Dwb", "Dwc", "Dwd", "Dfa", "Dfb", "Dfc", "Dfd", "ET", "EF")
  hex_colors <- c("#0000FF", "#0078FF", "#46AAFF", "#FF0000", "#FF9696", "#F5A500", "#FFDC64", 
                  "#FFFF00", "#C8C800", "#969600", "#96FF96", "#64C864", "#329632", "#C8FF50", 
                  "#64FF50", "#32C800", "#FF00FF", "#C800C8", "#963296", "#966496", "#AAAFFF", 
                  "#5A78DC", "#4B50B4", "#320087", "#00FFFF", "#37C8FF", "#007D7D", "#00465F", 
                  "#B2B2B2", "#666666")
  tempxx <- data.frame(code = koppen_codes,
                       color = hex_colors)
  write.table(x = tempxx, file = "C:/Rprojects/neonet/inst/extdata/koppen.tsv", sep =  "\t",
             row.names = F)
  # koppen_df <- data.frame(code = koppen_codes, hexColor = hex_colors)
  color_vector <- setNames(hex_colors, koppen_codes)
  tit <- paste0("KCC changes in Kyears (n = ", nrow(df), ")")
  ggplot(df.sank, aes(x = x,
                      next_x = next_x,
                      node = node,
                      next_node = next_node,
                      fill = factor(node),
                      label = node)) + 
    ggsankey::geom_sankey(flow.alpha = 0.5,
                          node.color = "black",
                          show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = color_vector) +
    ggsankey::geom_sankey_label(size = 3, color = "black", fill= "white", hjust = -0.5) +
    labs(caption = "@mycaption") +
    labs(title = tit) +
    # theme(axis.title = element_blank(),
    #       axis.text.y = element_blank(),
    #       axis.ticks = element_blank(),
    #       panel.grid = element_blank()) +
    theme_bw()
  # pl <- pl + theme(legend.position = "none")
  
  # pl <- pl + scale_fill_viridis_d(option = "inferno")
  # pl <- pl + 
  # pl <- pl + labs(subtitle = "using  David Sjoberg's ggsankey package")
  # pl <- pl + 
  # pl <- pl + labs(fill = 'Nodes')
  # pl
}