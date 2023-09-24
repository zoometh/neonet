#' @name neo_std
#'
#' @description conform a dataset to NeoNet dataset: standardisation
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
neo_std <- function(df.c14 = NA,
                    df.c14.pub = 'http://mappaproject.arch.unipi.it/mod/files/140_140_id00140_doc_elencoc14.tsv',
                    export = TRUE,
                    out.df.c14.topub = NA,
                    outDir = "C:/Rprojects/neonet/inst/extdata/",
                    verbose = TRUE){
  if(verbose){
    print("Missing columns")
  }
  # old dataset
  df.c14.pub <- read.csv(df.c14.pub, sep = "\t")
  if(verbose){
    in.original.only <- setdiff(colnames(df.c14), colnames(df.c14.pub)) # OK
    print(paste0("Columns in the new dataset but not in the publicated one:"))
    cat(in.original.only, sep = ", ")
    # print("\n")
    in.publicated.only <- setdiff(colnames(df.c14.pub), colnames(df.c14)) # OK
    print(paste0("Columns in the publicated dataset but not in the original one:"))
    cat(in.publicated.only, sep = ", ")
    # print("\n")
  }
  # rm non usefull col
  df.c14[ , in.original.only] <- NULL
  if(verbose){
    print("Order columns")
  }
  df.c14 <- df.c14[ , colnames(df.c14.pub)]
  if(export){
    if(is.na(out.df.c14.topub)){
      out.df.c14.topub <- paste0("new-c14-dataset-", Sys.Date(), ".tsv")
    }
    # write.table(df.c14, paste0(outDir, out.df.c14.topub),
    #             sep = "\t", 
    #             row.names = FALSE)
    write.table(df.c14, paste0(outDir, out.df.c14.topub),
               sep = "\t",
               row.names = FALSE,
               fileEncoding = "UTF-8")
    if(verbose){
      print(paste0(out.df.c14.topub, " has been exported in: ", outDir))
    }
  } else {
    return(df.c14)
  }
}



