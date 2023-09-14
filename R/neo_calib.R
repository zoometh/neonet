#' @name neo_calib
#'
#' @description calculate tpq / taq
#'
#' @param df.c14 the original XLSX with neonet columns (SiteName, Period, etc.) with with checked values (see: neo_subset)
#' @param intCal calibration curve
#' @param Present to calibrate from BP (1950).
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
                      Present = 1950,
                      ref.period = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/periods.tsv",
                      verbose = TRUE,
                      verbose.freq = 50){
  # calculate tpq/taq
  df.c14$taq <- df.c14$tpq <- df.c14$median <- df.c14$colors <- NA
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
                                     ids = 'Date1')
    # median
    age_samples <- Bchron::sampleAges(ages1)
    med <- as.numeric(apply(age_samples, 2, quantile, probs = c(0.95)))
    df.c14[i, "median"] <- -(med - Present)
    df.c14[i, "tpq"] <- -(min(ages1$Date1$ageGrid) - Present)
    df.c14[i, "taq"] <- -(max(ages1$Date1$ageGrid) - Present)
  }
  return(df.c14)
}