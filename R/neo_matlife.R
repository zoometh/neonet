#' @name neo_matlife
#'
#' @description join the datatframe of material life duration to the dataset of c14
#'
#' @param df.c14 the original XLSX with neonet columns (SiteName, Period, etc.)
#' @param ref.mat.life the path to the thesaurus of correspondences between dated material and material life duration
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return Add a column to the c14 dataset
#'
#' @examples
#'
#'
#' df.c14.mat <- neo_matlife(df.c14)
#'
#'
#' @export
neo_matlife <- function(df.c14 = NA,
                        ref.mat.life = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/140_id00140_doc_thesaurus.tsv"){
  ref.mat.life <- read.csv(ref.mat.life, sep = "\t")
  # 3 types
  short.life <- subset(ref.mat.life, life.duration == 'short.life')
  long.life <- subset(ref.mat.life, life.duration == 'long.life')
  other.life <- ref.mat.life[is.na(ref.mat.life$life.duration),]
  # dataframe
  family.life <- c(rep("short.life", nrow(short.life)),
                   rep("long.life", nrow(long.life)),
                   rep("other.life", nrow(other.life)))
  type.life <- c(short.life$material.type,
                 long.life$material.type,
                 other.life$material.type)
  material.life <- data.frame(family.life = family.life,
                              type.life = type.life)
  #
  short.life <- as.character(material.life[material.life$family.life == "short.life", "type.life"])
  long.life <- as.character(material.life[material.life$family.life == "long.life", "type.life"])
  other.life <- as.character(material.life[material.life$family.life == "other.life", "type.life"])
  df.c14$mat.life <- ifelse(df.c14$Material %in%  short.life, "short life",
                            ifelse(df.c14$Material %in%  long.life,"long life","others"))
  return(df.c14)
}
