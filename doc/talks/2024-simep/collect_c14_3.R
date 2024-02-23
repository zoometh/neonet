# Browse different DB to collect LM / EN dates

# install.packages("c14bazAAR", repos = c(ropensci = "https://ropensci.r-universe.dev"))
library(c14bazAAR)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(sf)
library(rnaturalearth)


present <- 1950
chr.interval.uncalBC <- c(-9000, -4000)
roi <- sf::st_read("https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi.geojson",
                   quiet = TRUE)
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

# dbs
# DB not done: kiteeastafrica, nerd, aida,  (no culture)
# DB done: calpal, medafricarbon, agrichange, neonet, bda, calpal, radon, katsianis

selected.db <- "katsianis"
df <- get_c14data(selected.db) # YES period, culture
# .. and Grob
top_title <- grid::textGrob(paste("db:", selected.db))

## filters
# columns (by default we try 'period')
col.req <- c("labnr", "c14age", "c14std", "period", "lon", "lat")
# test if the "period" column exist or not
df_selected <- tryCatch({
  df[, c(col.req, c("site"))]
}, error = function(e) {
  col.req <- c("labnr", "c14age", "c14std", "culture", "lon", "lat")
  df[, c(col.req, c("site"))]
})
is.period <- "period" %in% colnames(df_selected)
if(is.period){
  chr.column <- "period"
  } else {
  chr.column <- "culture"
  col.req <- c("labnr", "c14age", "c14std", "culture", "lon", "lat")
  }

df_selected$c14age_uncalBC <- df_selected$c14age - present
df_selected$c14age_uncalBC <- - df_selected$c14age_uncalBC# data
df_selected <- df_selected[complete.cases(df_selected[col.req]), ]
# chrono
df_selected <- df_selected[df_selected$c14age_uncalBC > chr.interval.uncalBC[1] & df_selected$c14age_uncalBC < chr.interval.uncalBC[2], ]
nrow(df_selected)
# spatial
df_sf <- st_as_sf(df_selected, coords = c("lon", "lat"), crs = 4326)
inside <- st_within(df_sf, roi, sparse = FALSE)
df_selected <- df_selected[inside, ]
# .. and Grob
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
df_spat <- st_as_sf(df_selected, coords = c("lon", "lat"), crs = 4326)
buff <- .5
world <- ne_countries(scale = "medium", returnclass = "sf")
distr_spat <- ggplot2::ggplot(world) +
  # TODO: color scale ramp on column 'c14age_uncalBC'
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = df_spat, inherit.aes = FALSE, size = 1) +
  ggplot2::coord_sf(xlim = c(sf::st_bbox(roi)[1] - buff, sf::st_bbox(roi)[3] + buff),
                    ylim = c(sf::st_bbox(roi)[2] - buff, sf::st_bbox(roi)[4] + buff)) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text = ggplot2::element_text(size = 6)) 

## plot
g <- grid.arrange(distr_spat, distr_chr, df_groups_grob, context_title,
                      layout_matrix = lay,
                      top = top_title)
g.out <- paste0("C:/Rprojects/neonet/doc/talks/2024-simep/img/", "_db_", selected.db, ".png")
ggsave(file = g.out, g, width = 14, height = 10)
