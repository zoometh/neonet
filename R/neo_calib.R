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
#'
#'
#' @export
neo_calib <- function(df.c14 = NA,
                      intCal = 'intcal20',
                      ci = 0.95,
                      present = 1950,
                      ref.period = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/periods.tsv",
                      verbose = TRUE,
                      verbose.freq = 50){
  `%>%` <- dplyr::`%>%`
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
    age_samples <- Bchron::sampleAges(ages1)
    med <- as.numeric(apply(age_samples, 2, quantile, probs = c(ci)))
    df.c14[i, "median"] <- -(med - present)
    df.c14[i, "tpq"] <- -(min(ages1$Date1$ageGrid) - present)
    df.c14[i, "taq"] <- -(max(ages1$Date1$ageGrid) - present)
  }
  return(df.c14)
}