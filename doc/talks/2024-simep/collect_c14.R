# install.packages("c14bazAAR", repos = c(ropensci = "https://ropensci.r-universe.dev"))
library(c14bazAAR)
library(dplyr)
library(ggplot2)

present <- 1950
roi <- 
ws.roi <- sf::st_read(ref.spat, quiet = TRUE)

df <- get_c14data("kiteeastafrica") # NO culture (cultural periods)
df <- get_c14data("medafricarbon") # YES culture
df <- get_c14data("calpal") # YES culture
df$c14age_uncalBC <- df$c14age - present
df$c14age_uncalBC <- - df$c14age_uncalBC
head(df)
colnames(df)
head(unique(df$culture))

# nb of dates by culture
df_ndates_by_group <- as.data.frame(table(df$culture))
df_ndates_by_group <- df_ndates_by_group[order(-df_ndates_by_group$Freq), ]

# mean dates of cultures
df_mean_by_group <- df %>%
  group_by(culture) %>%
  summarise(mean_value = mean(c14age_uncalBC)) %>%
  arrange(desc(mean_value))

distr_chr <- ggplot(df, aes(x = c14age_uncalBC)) + 
  # TODO: color scale ramp
  geom_histogram(binwidth = 100, # You can adjust the binwidth as needed
                 color = "blue", fill = "blue") +
  labs(x = "uncal BC",
       y = "Frequency") +
  theme_minimal()

library(sf)
library(rnaturalearth)

df_spat <- df[ , c("lon", "lat")]
df_spat <- na.omit(df_spat)
df_spat <- st_as_sf(df_spat, coords = c("lon", "lat"), crs = 4326)
# df_spat <- st_zm(df_spat)
buff <- 1
world <- ne_countries(scale = "medium", returnclass = "sf")
distr_spat <- ggplot2::ggplot(world) +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = df_spat, inherit.aes = FALSE) +
  # ggplot2::coord_sf(xlim = c(sf::st_bbox(df_spat)[1] - buff, sf::st_bbox(df_spat)[3] + buff),
  #                   ylim = c(sf::st_bbox(df_spat)[2] - buff, sf::st_bbox(df_spat)[4] + buff)) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text = ggplot2::element_text(size = 7)) 
distr_spat

# ggplot() + 
#   geom_sf(data = bck_admin.shp) +
#   geom_sf(data = tiles needed, fill = 'red') +
#   theme_bw()


