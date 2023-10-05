#' @name neo_datamiss
#'
#' @description show a synthetic view of missing data
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
neo_datamiss <- function(df.c14 = NA,
                         main = "",
                         ylabels = c(1, 50, 100, 200, 500, 1000, 2000, 3000, 5000),
                         col.used = c("SiteName", "Period", "PhaseCode",
                                      "LabCode", "C14Age", "C14SD",
                                      "Material", "MaterialSpecies", 
                                      # "AnatomicalParts",
                                      "tpq", "taq",
                                      "bib", "bib_url",
                                      "Longitude", "Latitude", "Country"),
                         export = TRUE,
                         outDir = "C:/Rprojects/neonet/results/",
                         width = 11,
                         height = 9,
                         outFile = "missing_info.jpg"
){
  # dataframe of missing data

  # # df.tot <- read.csv(paste0(c14.path, "shinyapp/c14_dataset.tsv"), sep = "\t")
  # df.c14[df.c14 == "n/a"] <- NA
  df.c14[df.c14 %in% c("n/a", "n/d", "")] <- NA
  df.c14 <- df.c14[ , col.used]
  if(export){
    jpeg(paste0(outDir, outFile),
         width = width, height = height, units = "cm", res = 300)
  }
  ylabels <- sort(c(ylabels, nrow(df.c14)))
  Amelia::missmap(df.c14,
                  x.cex = .6,
                  y.cex = .4,
                  # y.labels=row.names(df.tot),
                  y.labels = ylabels,
                  y.at = ylabels,
                  cex = 0.2,
                  main = main,
                  legend = F,
                  col = c("white", "blue"),
                  rank.order = FALSE,
                  margins = c(5, 2))
  if(export){
    dev.off()
    if(verbose){
      print(paste0("The plot missing data'", outFile, "' has been exported to: '", outDir, "'"))
    }
  }
  # ggsave(filename = "docs/publication/missing_info.png", g.miss, width = 17, units = "cm")
}
