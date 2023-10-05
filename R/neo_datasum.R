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
                        col.used = c("SiteName", "Period", "PhaseCode",
                                     "LabCode", "C14Age", "C14SD",
                                     "Material", "MaterialSpecies",
                                     "tpq", "taq",
                                     "bib", "bib_url",
                                     "Longitude", "Latitude", "Country"),
                        missing.values = c("", "n/a", NA),
                        ref.period = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/periods.tsv",
                        verbose = TRUE){
  periods.colors <- read.csv(ref.period, sep = "\t")
  ordered.periods <- periods.colors$period
  # num_cells_with_values <- sum(df.c14 %in% missing.values, na.rm = TRUE)
  # df.c14[df.c14 %in% missing.values, ] <- NA
  df.c14[df.c14 %in% c("n/a", "n/d", "")] <- NA
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
