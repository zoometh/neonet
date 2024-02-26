
neo_kcc_sankey(df_cc = NA,
               col.req = NA,
               kcc_colors = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/koppen.tsv",
               selected.per = c("EN", "LM")){
  # df_cc_ <- as.data.frame(df_cc)
  df <- sf::st_set_geometry(df_cc, NULL)
  for(per in selected.per){
    # per <- "LM"
    df <- df[df$Period == per, ]
    df <- df[ , which(names(df) %in% col.req)]
    # df <- na.omit(df)
    df.sank <- df %>%
      ggsankey::make_long(colnames(df))
    df.sank
    print(paste0("*read KCC colors"))
    koppen_colors <- read.csv2(kcc_colors, sep = "\t")
    kcc_colors <- setNames(koppen_colors$color, koppen_colors$code)  
    tit <- paste0(per, " - KCC changes in Kyears (n = ", nrow(df), ")")
    ggplot(df.sank, aes(x = x,
                        next_x = next_x,
                        node = node,
                        next_node = next_node,
                        fill = factor(node),
                        label = node)) + 
      ggsankey::geom_sankey(flow.alpha = 0.5,
                            node.color = "black",
                            show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = kcc_colors) +
      ggsankey::geom_sankey_label(size = 3, color = "black", fill= "white", hjust = -0.5) +
      labs(caption = "@neonet") +
      labs(title = tit) +
      # theme(axis.title = element_blank(),
      #       axis.text.y = element_blank(),
      #       axis.ticks = element_blank(),
      #       panel.grid = element_blank()) +
      theme_bw()
  }
  # pl <- pl + theme(legend.position = "none")
  
  # pl <- pl + scale_fill_viridis_d(option = "inferno")
  # pl <- pl + 
  # pl <- pl + labs(subtitle = "using  David Sjoberg's ggsankey package")
  # pl <- pl + 
  # pl <- pl + labs(fill = 'Nodes')
  # pl
}