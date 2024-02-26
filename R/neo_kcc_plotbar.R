neo_kcc_plotbar <- function(df_cc = NA,
                           col.req = NA,
                           outDir = NA,
                           kcc.file = c("koppen_6k.tif", "koppen_7k.tif", "koppen_8k.tif",
                                        "koppen_9k.tif", "koppen_10k.tif", "koppen_11k.tif"),
                           kcc_colors = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/koppen.tsv",
                           selected.per = NA){
  # df_cc_ <- as.data.frame(df_cc)
  df <- sf::st_set_geometry(df_cc, NULL)
  # for(per in selected.per){
  # per <- "LM"
  df <- df[df$Period %in% selected.per, ]
  df <- df[ , which(names(df) %in% col.req)]
  # library(tidyverse)
  df_long <- df %>%
    tidyr::pivot_longer(cols = starts_with("koppen_"), names_to = "koppen_type", values_to = "value") %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(koppen_type, value) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(koppen_type) %>%
    dplyr::mutate(percentage = count / sum(count) * 100) %>%
    dplyr::ungroup()
  # # reorder df_long
  df_long$koppen_type <- factor(df_long$koppen_type)
  kcc <- DescTools::SplitPath(kcc.file)$filename
  df_long$koppen_type <- factor(df_long$koppen_type, levels = kcc)
  df_long <- df_long[order(df_long$koppen_type), ]
  # Plotting
  
  # Assuming 'df' is your dataframe, 'order_vector' is your vector of values, 
  # and 'column_name' is the name of the column you're reordering by
  # order_vector <- c("value3", "value1", "value2") # Example values corresponding to 'column_name'
  # df_reordered <- df[match(order_vector, df$column_name), ]
  
  period.names <- paste0(selected.per, collapse = "-")
  # TODO: by site?
  tit <- paste0(period.names, " KCC changes in Kyears (n = ", nrow(df), " dates)")
  gout <- ggplot(df_long, aes(x = koppen_type, y = percentage, fill = factor(value))) +
    geom_bar(stat = "identity", position = "fill") +
    # scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_manual(values = kcc_color_map) +
    labs(x = "Koppen Classification", y = "Percentage", 
         title = tit) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(filename = paste0(outDir, period.names, "_kcc_stacked.png"),
         gout,
         width = 16, height = 12)
}
  