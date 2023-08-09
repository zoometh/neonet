#' @name neo_doi
#'
#' @description conform a dataset to NeoNet dataset
#'
#' @param df.c14 the new original dataset
#' @param df.c14.pub the already published dataset (ex: NeoNet med)
#' @param out.df.c14.topub the name of the conformed dataset to output
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return Export a file
#'
#' @examples
#'
#'
#' @export
neo_doi <- function(df.c14 = NA,
                    df.c14.pub = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/140_140_id00140_doc_elencoc14.tsv",
                    out.df.c14.topub = "c14_dataset_atl.tsv",
                    outDir = "C:/Rprojects/neonet/inst/extdata/",
                    verbose = TRUE){
  if(verbose){
    print("Missing columns")
  }
  # old dataset
  df.c14.pub <- read.csv(df.c14.pub, sep = "\t")
  if(verbose){
    in.original.only <- setdiff(colnames(df.c14), colnames(df.c14.pub)) # OK
    print(paste0("Columns in the original dataset but not in the publicated one:"))
    cat(in.original.only, sep = ", ")
    print("\n")
    in.publicated.only <- setdiff(colnames(df.c14.pub), colnames(df.c14)) # OK
    print(paste0("Columns in the publicated dataset but not in the original one:"))
    cat(in.publicated.only, sep = ", ")
    print("\n")
  }
  # rm non usefull col
  df.c14[ , in.original.only] <- NULL
  if(verbose){
    print("Order columns")
  }
  df.c14 <- df.c14[ , colnames(df.c14.pub)]
  write.table(df.c14, paste0(outDir, out.df.c14.topub),
              sep = "\t", 
              row.names = FALSE)
  if(verbose){
    print(paste0(out.df.c14.topub, " has been exported in: ", outDir))
  }
}