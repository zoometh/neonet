## works with collect_c14.R

report_db <- function(){
  # .. and Grob
  top_title <- grid::textGrob(paste("db:", selected.db))
  listing.head <- 60
  listing.sz <- .7
  # layouts
  mytheme.listing <- gridExtra::ttheme_default(
    core = list(fg_params = list(cex = listing.sz),
                padding = unit(c(1, 1), "mm")),
    colhead = list(fg_params = list(cex = listing.sz)),
    rowhead = list(fg_params = list(cex = listing.sz)))
  lay <- rbind(c(1, 1, 1, 1, 1, 1, 4, 4, 4, 4), 
               c(1, 1, 1, 1, 1, 1, 3, 3, 3, 3), 
               c(1, 1, 1, 1, 1, 1, 3, 3, 3, 3), 
               c(1, 1, 1, 1, 1, 1, 3, 3, 3, 3),
               c(2, 2, 2, 2, 2, 2, 3, 3, 3, 3), 
               c(2, 2, 2, 2, 2, 2, 3, 3, 3, 3))
  # .. Grob text
  context_title <- grid::textGrob(paste0("Dates subsetted btw ",
                                         paste0(chr.interval, collapse = "/"), " uncal BC (",
                                         nrow(df_selected), " dates) \n",
                                         "Everything here is uncal BC"), 
                                  gp = grid::gpar(fontsize = 12))
  
  
  ## dataframes
  # t1: nb of dates by culture
  df_ndates_by_group <- as.data.frame(table(df_selected[[chr.column]]))
  df_ndates_by_group <- df_ndates_by_group[order(-df_ndates_by_group$Freq), ]
  names(df_ndates_by_group)[names(df_ndates_by_group) == "Var1"] <- chr.column
  names(df_ndates_by_group)[names(df_ndates_by_group) == "Freq"] <- "date_nb"
  # t2: mean dates of cultures
  df_mean_by_group <- df_selected %>%
    group_by(.data[[chr.column]]) %>%
    summarise(date_avg = mean(c14age_uncalBC)) 
  df_mean_by_group$date_avg <- as.integer(df_mean_by_group$date_avg)
  # merge t1 and t2
  df_groups = merge(df_ndates_by_group, df_mean_by_group, by = chr.column)
  df_groups <- df_groups %>%
    arrange(date_avg)
  # .. and Grob
  df_groups_grob <- tableGrob(head(df_groups, listing.head),
                              rows = NULL, 
                              theme = mytheme.listing)
  
  
  ## distribution chr = histogram
  distr_chr <- ggplot(df_selected, aes(x = c14age_uncalBC)) + 
    # TODO: color scale ramp on column 'c14age_uncalBC'
    geom_histogram(binwidth = 100, # You can adjust the binwidth as needed
                   color = "blue", fill = "blue") +
    labs(x = "uncal BC",
         y = "Frequency") +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(), 
      axis.title.y = element_blank(),
      axis.text = ggplot2::element_text(size = 6)
    )
  ## distribution spat = map
  distr_spat <- fspat(df_selected, roi)
  
  ## plot
  g <- grid.arrange(distr_spat, distr_chr, df_groups_grob, context_title,
                    layout_matrix = lay,
                    top = top_title)
  g.out <- paste0(root.path, "/img/", "_db_", selected.db, ".png")
  ggsave(file = g.out, g, width = 14, height = 10)
}
