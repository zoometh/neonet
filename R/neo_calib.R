#' @name neo_calib
#'
#' @description Calibrate the radiocarbon dates, calculate tpq / taq and the median
#'
#' @param df.c14 A dataframe. The original XLSX with neonet columns (SiteName, Period, etc.) with with checked values (see: neo_subset)
#' @param intCal calibration curve
#' @param ci Confidence interval. Default 0.95 (95%)
#' @param present to calibrate from BP (1950).
#' @param ref.period period referenced in NeoNet (and colors). A TSV file.
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return
#'
#' @examples
#'
#' df <- neo_calib(df.c14 = df.c14,
#'                 stat.mean = TRUE,
#'                 verbose.freq = 100)
#'
#' @export
neo_calib <- function(df.c14 = NA,
                      intCal = 'intcal20',
                      ci = 0.95,
                      present = 1950,
                      stat.median = TRUE,
                      stat.mean = FALSE,
                      ref.period = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/periods.tsv",
                      verbose = TRUE,
                      verbose.freq = 50){
  `%>%` <- dplyr::`%>%`
  if(inherits(df.c14, "sf")){
    if(verbose){
      print(paste0("Reads a 'sf' dataframe"))
    }
    df.c14 <- sf::st_set_geometry(df.c14, NULL)
  }
  if(stat.mean){
    if(verbose){
      print(paste0("Create a column 'mean' to be filled with dates weighted means"))
      df.c14$mean <- NA
    }
  }
  # calculate tpq/taq
  # TODO: is column colors useful?
  df.c14$taq <- df.c14$tpq <- df.c14$median <- df.c14$colors <- NA
  df.c14 <- df.c14 %>%
    dplyr::filter(!is.na("C14Age") & !is.na("C14SD")) %>%
    dplyr::filter(C14Age != '' & C14SD != '')
  if(verbose){print("Run date calibration")}
  for (i in 1:nrow(df.c14)){
    if(verbose){
      if(i %% verbose.freq == 0) {
        print(paste0(as.character(i), "/", as.character(nrow(df.c14))))
      }
    }
    ages1 <- Bchron::BchronCalibrate(ages = df.c14[i, "C14Age"],
                                     ageSds = df.c14[i, "C14SD"],
                                     calCurves = intCal,
                                     ids = 'Date1',
                                     allowOutside = TRUE)
    # median
    # age_samples <- Bchron::sampleAges(ages1)
    # med <- as.numeric(apply(age_samples, 2, quantile, probs = c(ci)))
    # weighted median
    weighted.median <- matrixStats::weightedMedian(x = ages1$Date1$ageGrid, w = ages1$Date1$densities)
    df.c14[i, "median"] <- -(weighted.median - present)
    if(stat.mean){
      weighted.mean <- matrixStats::weightedMean(x = ages1$Date1$ageGrid, w = ages1$Date1$densities)
      df.c14[i, "mean"] <- -(weighted.mean - present)
    }
    df.c14[i, "tpq"] <- -(min(ages1$Date1$ageGrid) - present)
    df.c14[i, "taq"] <- -(max(ages1$Date1$ageGrid) - present)
  }
  if(stat.mean){
    if(verbose){
      print("Reorder the columns")
    }
    # df.c14 <- df.c14 %>%
    #   dplyr::select(everything(), tpq, taq)
    df.c14 <- df.c14 %>% 
      dplyr::relocate(mean, .after = median)
  }
  return(df.c14)
}