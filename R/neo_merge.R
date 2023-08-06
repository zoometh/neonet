#' @name neo_merge
#'
#' @description merge published and unpublished datasets for the NeoNet app
#'
#' @param df.c14 the new original dataset
#' @param df.c14.pub the already published dataset (ex: NeoNet med)
#' @param data.bib the original .bib file path with BibTex references
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return 
#'
#' @examples
#'
#'
#'
#' @export
neo_subset <- function(df.c14 = NA,
                       df.c14.pub = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/140_140_id00140_doc_elencoc14.tsv",
                       data.bib = NA,
                       data.bib.pub = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/id00140_doc_reference.bib",
                       merge.c14 = TRUE,
                       merge.bib = TRUE,
                       verbose = TRUE){
  if(merge.c14){
    if(verbose){
      print("Merge new and publised c14 datasets")
    }
    # old dataset
    df.c14.pub <- read.csv(df.c14.pub, sep = "\t")
    setdiff(colnames(df.c14.pub), colnames(df.c14)) # OK
    # rm non usefull col
    supp.col <- setdiff(colnames(df.c14), colnames(df.c14.pub))
    df.c14[ , supp.col] <- NULL
    df.c14.2 <- rbind(df.c14, df.c14.pub)
  }
  if(merge.bib){
    if(verbose){
      print("Merge new and publised bibtex datasets")
    }
    data.bib <- paste0(getwd(), "/inst/extdata/", "NeoNet_atl_ELR.bib")
    data.bib.pub <- "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/id00140_doc_reference.bib"
    
    combined_bib <- ""
    for (path_to_bib_file in c(data.bib, data.bib.pub)) {
      fileCon <- file(path_to_bib_file)
      content <- readLines(fileCon)
      close(fileCon)
      combined_bib <- paste0(combined_bib, "\n", "\n", trimws(paste0(content, collapse="\n")))
    } 
    
    # old dataset
    df.c14.pub <- read.csv(df.c14.pub, sep = "\t")
    setdiff(colnames(df.c14.pub), colnames(df.c14)) # OK
    # rm non usefull col
    supp.col <- setdiff(colnames(df.c14), colnames(df.c14.pub))
    df.c14[ , supp.col] <- NULL
    df.c14.2 <- rbind(df.c14, df.c14.pub)
  }
}

# # read dataset
# data.c14 <- paste0(path.data, "NeoNet_atl_ELR (1).xlsx")
# df.c14 <- openxlsx::read.xlsx(data.c14)
# df.c14 <- df.c14[df.c14$Country == "France", ]
# df.c14 <- as.data.frame(apply(df.c14, 2, trimws))
#
# # call function
# df.c14 <- neo_subset(df.c14)
