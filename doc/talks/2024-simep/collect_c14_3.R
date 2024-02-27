# Browse different DB to collect LM / EN dates

# install.packages("c14bazAAR", repos = c(ropensci = "https://ropensci.r-universe.dev"))
library(c14bazAAR)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(sf)
library(rnaturalearth)

fspat <- function(df_selected, roi, outfile){
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
  g.out <- paste0("C:/Rprojects/neonet/doc/talks/2024-simep/img/", outfile)
  ggsave(file = g.out, distr_spat, width = 14, height = 10)
}

parse_db <- function(l.dbs, 
                     df.all, 
                     col.req, 
                     chr.interval.uncalBC, 
                     present = 1950, 
                     roi = NA){
  # collect dates form a list of dbs, creates missing columns (period or culture), filter on chronology (time interval) and spatial location (roi)
  `%>%` <- dplyr::`%>%` # used to not load dplyr
  for(selected.db in l.dbs){
    # selected.db <- l.dbs[i]
    # selected.db <- "neonet"
    print(paste0("*read: ", selected.db))
    df <- c14bazAAR::get_c14data(selected.db) # YES period, culture
    print(paste0("  n = ", nrow(df)))
    # colnames(df)
    ## filters
    is.not.both <- !("culture" %in% colnames(df) & "period" %in% colnames(df))
    is.not.period <- !("period" %in% colnames(df))
    is.not.culture <- !("culture" %in% colnames(df))
    df_selected <- df
    if(is.not.both & is.not.culture){
      df_selected$culture <- NA 
    }
    if(is.not.both & is.not.period){
      df_selected$period <- NA 
    }
    df_selected <- df_selected[ , col.req]
    df_selected <- df_selected %>%
      dplyr::filter(!(is.na("period") & is.na("culture")))
    # head(df_selected)
    df_selected$c14age_uncalBC <- df_selected$c14age - present
    df_selected$c14age_uncalBC <- - df_selected$c14age_uncalBC# data
    # chrono
    chr.sup <- df_selected$c14age_uncalBC > chr.interval.uncalBC[1]
    chr.inf <- df_selected$c14age_uncalBC < chr.interval.uncalBC[2]
    df_selected <- df_selected[chr.sup & chr.inf, ]
    # spatial
    df_selected <- df_selected[!(df_selected$lon == "" & df_selected$lat == ""), ]
    df_selected <- df_selected[!is.na(df_selected$lon) & !is.na(df_selected$lat), ]
    if(!is.na(roi)){
      df_sf <- sf::st_as_sf(df_selected, coords = c("lon", "lat"), crs = 4326)
      inside <- sf::st_within(df_sf, roi, sparse = FALSE)
      df_selected <- df_selected[inside, ]
    }
    df_selected <- df_selected[, col.req]
    df.all <- rbind(df.all, df_selected)
  }
  return(df.all)
}

fref <- function(df.all.res, outfile = "df_ref_per.xlsx"){
  # write a reference file used to map external db to periods (class) equal to: ..., LM, EN, ...
  print(unique(df.all.res$sourcedb))
  df.ref.per <- df.all.res[, c("period", "culture")]
  df.ref.per <- df.ref.per[!duplicated(df.ref.per), ]
  openxlsx::write.xlsx(df.ref.per, paste0(root.path, "/", outfile))
}

fread_ref <- function(infile = "C:/Rprojects/neonet/doc/talks/2024-simep/df_ref_per.xlsx"){
  # read the reference file and remove rows with empty class
  df_ref_per <- openxlsx::read.xlsx(infile)
  df_ref_per$class <- toupper(df_ref_per$class)
  df_ref_per <- df_ref_per[!is.na(df_ref_per$class), ]
  return(df_ref_per)
}


frm_duplicates <- function(df.classes){
  # remove duplicates
  # TODO: prioritise duplicates according to the db they are coming from (ex: neonet)
  df.classes <- df.classes[!duplicated(df.classes$labnr), ]
  return(df.classes)
}


root.path <-"C:/Rprojects/neonet/doc/talks/2024-simep"
present <- 1950
chr.interval.uncalBC <- c(-9000, -4000)
roi <- sf::st_read("https://raw.githubusercontent.com/zoometh/neonet/main/doc/talks/2024-simep/roi.geojson",
                   quiet = TRUE)
# dbs
# DB not done: kiteeastafrica, nerd, aida,  (no culture)
# DB done: calpal, medafricarbon, agrichange, neonet, bda, calpal, radon, katsianis
l.dbs <- c("calpal", "medafricarbon", "agrichange", "neonet", "bda", "calpal", "radon", "katsianis")
# l.dbs <- c("neonet")
renaming_vector <- c(
  sourcedb = "sourcedb",
  SiteName = "site",
  LabCode = "labnr",
  C14Age = "c14age",
  C14SD = "c14std",
  db_period = "period.x",
  db_culture = "culture.x",
  Period = "class",
  lon = "lon",
  lat = "lat"
)
col.req <- c("sourcedb", "site", "labnr", "c14age", "c14std", "period", "culture", "lon", "lat")
df.all <- setNames(data.frame(matrix(ncol = length(col.req), nrow = 0)), col.req)
df.all.res <- parse_db(l.dbs, df.all, col.req, chr.interval.uncalBC, present, roi)
# fspat(df.all.res, roi, outfile = "_db__all_class.png")
# fspat(df.all.res, roi, outfile = "_db__all.png")
df_ref_per <- fread_ref("C:/Rprojects/neonet/doc/talks/2024-simep/df_ref_per.xlsx")
df_ref_per$period_culture <- paste0(df_ref_per$period, "/", df_ref_per$culture)
df.all.res$period_culture <- paste0(df.all.res$period, "/", df.all.res$culture)
df.classes <- merge(df.all.res, df_ref_per, by = "period_culture")
df.classes <- frm_duplicates(df.classes)
# map colnames to neonet format
df.c14 <- df.classes %>%
  dplyr::rename(!!!setNames(renaming_vector, names(renaming_vector))) %>%
  dplyr::select(names(renaming_vector))
colnames(df.c14)
# df.classes <- df.classes %>%
#   dplyr::rename(db_sourcedb = sourcedb,
#                 SiteName = site,
#                 LabCode = labnr,
#                 C14Age = c14age,
#                 C14SD = c14std,
#                 db_period = period.x,
#                 db_culture = culture.x,
#                 Period = class,
#                 lon = lon,
#                 lat = lat) %>%
#   dplyr::select(db_sourcedb, SiteName, LabCode, C14Age, C14SD, db_period, db_culture, Period, lon, lat)
df.c14 <- neo_calib(df.c14)
df.c14 <- sf::st_as_sf(df.c14, coords = c("lon", "lat"), crs = 4326)
# ...
xxx <- df.c14 %>% 
  sample_n(250)
# unique(xxx$Period)
kcc.file <- c("koppen_6k.tif", "koppen_7k.tif", "koppen_8k.tif",
              "koppen_9k.tif", "koppen_10k.tif", "koppen_11k.tif")
df_cc <- neo_kcc_get_cc(df.c14 = df.c14, kcc.file = kcc.file)
col.req <- gsub(pattern = ".tif", "", kcc.file)
neo_kcc_plotbar(df_cc, 
                col.req = col.req,
                selected.per = c("LM", "MM"),
                outDir = "C:/Rprojects/neonet/doc/talks/2024-simep/img/")
# neo_kcc_sankey(df_cc, col.req = col.req, 
#                selected.per = c("EN", "MM"), 
#                outDir = "C:/Rprojects/neonet/doc/talks/2024-simep/img/")
# df <- read.csv(paste0(root.path, "/medians.csv"))
