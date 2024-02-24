# install.packages("c14bazAAR", repos = c(ropensci = "https://ropensci.r-universe.dev"))
library(c14bazAAR)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
# library(DT)
# library(patchwork)
library(sf)
library(rnaturalearth)


present <- 1950
chr.interval <- c(-12000, -3000)
roi <- sf::st_read("https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi.geojson",
                   quiet = TRUE)

df <- get_c14data("kiteeastafrica") # NO culture (cultural periods)
df <- get_c14data("medafricarbon") # YES culture

selected.db <- c("calpal")
df <- get_c14data("calpal") # YES period, culture


head(df)
colnames(df)
head(unique(df$culture))

# df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)


## filters
# columns
col.req <- c("labnr", "c14age", "c14std", "period", "lon", "lat")
df_selected <- df[, c(col.req, c("site"))]
df_selected$c14age_uncalBC <- df_selected$c14age - present
df_selected$c14age_uncalBC <- - df_selected$c14age_uncalBC# data
df_selected <- df_selected[complete.cases(df_selected[col.req]), ]
# chrono
df_selected <- df_selected[df_selected$c14age_uncalBC > chr.interval[1] & df_selected$c14age_uncalBC < chr.interval[2], ]
nrow(df_selected)
# spatial
df_sf <- st_as_sf(df_selected, coords = c("lon", "lat"), crs = 4326)
inside <- st_within(df_sf, roi, sparse = FALSE)
df_selected <- df_selected[inside, ]
# nrow(df_selected)



# nb of dates by culture
df_ndates_by_group <- as.data.frame(table(df_selected$period))
df_ndates_by_group <- df_ndates_by_group[order(-df_ndates_by_group$Freq), ]
names(df_ndates_by_group)[names(df_ndates_by_group) == "Var1"] <- "period"
names(df_ndates_by_group)[names(df_ndates_by_group) == "Freq"] <- "date_nb"

# mean dates of cultures
df_mean_by_group <- df_selected %>%
  group_by(period) %>%
  summarise(date_avg = mean(c14age_uncalBC)) %>%
  arrange(desc(date_avg))
df_mean_by_group$date_avg <- as.integer(df_mean_by_group$date_avg)
# names(df_mean_by_group)[names(df_mean_by_group) == "mean_value"] <- "nb"

df_groups = merge(df_ndates_by_group, df_mean_by_group, by = "period")
# mean dates of cultures
df_groups <- df_groups %>%
  arrange(date_avg)

distr_chr <- ggplot(df_selected, aes(x = c14age_uncalBC)) + 
  # TODO: color scale ramp on column 'c14age_uncalBC'
  geom_histogram(binwidth = 100, # You can adjust the binwidth as needed
                 color = "blue", fill = "blue") +
  labs(x = "uncal BC",
       y = "Frequency") +
  theme_minimal()


df_spat <- st_as_sf(df_selected, coords = c("lon", "lat"), crs = 4326)
# df_spat <- st_zm(df_spat)
buff <- 1
world <- ne_countries(scale = "medium", returnclass = "sf")
distr_spat <- ggplot2::ggplot(world) +
  # TODO: color scale ramp on column 'c14age_uncalBC'
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = df_spat, inherit.aes = FALSE) +
  ggplot2::coord_sf(xlim = c(sf::st_bbox(df_spat)[1] - buff, sf::st_bbox(df_spat)[3] + buff),
                    ylim = c(sf::st_bbox(df_spat)[2] - buff, sf::st_bbox(df_spat)[4] + buff)) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text = ggplot2::element_text(size = 7)) 
distr_spat

# ggplot() + 
#   geom_sf(data = bck_admin.shp) +
#   geom_sf(data = tiles needed, fill = 'red') +
#   theme_bw()

# # Arrange the plots side by side
# combined_plots <- distr_spat + distr_chr + plot_layout(ncol = 1)
# print(combined_plots)

listing.sz = .4
mytheme.listing <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = listing.sz),
              padding=unit(c(.5, .5), "mm")),
  colhead = list(fg_params=list(cex = listing.sz)),
  rowhead = list(fg_params=list(cex = listing.sz)))
# df_ndates_by_group_grob <- tableGrob(head(df_ndates_by_group, 20), rows=NULL, theme = mytheme.listing)
# df_mean_by_group_grob <- tableGrob(head(df_mean_by_group, 20), rows=NULL, theme = mytheme.listing)
df_groups_grob <- tableGrob(head(df_groups, 35), rows=NULL, theme = mytheme.listing)


# grid.arrange(distr_spat, distr_chr, df_ndates_by_group_grob, df_mean_by_group_grob, ncol = 2)

grid.arrange(arrangeGrob(distr_spat, distr_chr, ncol = 1, nrow = 2),
             arrangeGrob(df_groups_grob, ncol = 1, nrow = 1), widths = c(2, 1))

# #################
# # Combine the plots and table with custom layout
# layout_matrix <- rbind(c(1,2),
#                        c(3,2))
# combined_plot <- arrangeGrob(distr_spat, distr_chr, hist_plot, empty_plot, layout_matrix = layout_matrix)
# 
# # Draw the combined layout
# grid.draw(combined_plot)


lay <- rbind(c(1, 1, 1, 1, 1, 1, 3, 3, 3), 
             c(1, 1, 1, 1, 1, 1, 3, 3, 3), 
             c(1, 1, 1, 1, 1, 1, 3, 3, 3), 
             c(1, 1, 1, 1, 1, 1, 3, 3, 3),
             c(2, 2, 2, 2, 2, 2, 3, 3, 3), 
             c(2, 2, 2, 2, 2, 2, 3, 3, 3))
top_title <- grid::textGrob(selected.db, gp = grid::gpar(fontface = "bold"))


grid.arrange(distr_spat, distr_chr, df_groups_grob, 
             layout_matrix = lay,
             top = top_title
             # bottom = bottom_title,
             # left = "Example Layout", right = right_title
             )

