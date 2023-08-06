#' @name neo_bib
#'
#' @description convert the bibtex file '.bib' into long references. Export bibtex references "bibentry" in Markdown output. ex: bib[1] = Delibrias G, Guillier M, Labeyrie J (1982). “Gif natural radiocarbon measurements IX.” _Radiocarbon_, *24*(3), 291-343. recalcultate the "long.ref" value from DOIs for unique references if exists
# or BibTex entries (file 'references_xx.bib')
#'
#' @param df.c14 the original XLSX with neonet columns (SiteName, Period, etc.)
#' @param data.bib the original .bib file path with BibTex references
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return df.c14 with bibliographic references
#'
#' @examples
#'
#'
#' uniq.refs <- neo_bib(df.c14, data.bib)
#'
#'
#' @export
neo_bib <- function(df.c14 = NA,
                    data.bib = NA,
                    sleep.sec = .3,
                    verbose = TRUE){
  # df <- df.c14
  # recalcultate the "long.ref" value from DOIs for unique references if exists
  # or BibTex entries (file 'references_xx.bib')
  # a shorter df for uniques references
  df.bib <- bibtex::read.bib(data.bib)
  uniq.refs <- unique(df.c14[c("bib", "bib_url")])
  names(uniq.refs) <- c("short.ref", "key.or.doi")
  uniq.refs$long.ref <- NA
  # uniq.refs$long.ref <- uniq.refs$key.or.doi
  uniq.refs <- uniq.refs[with(uniq.refs, order(key.or.doi)), ] # sort
  if(verbose){print(paste0("nb of unique references: ", nrow(uniq.refs)))}
  #
  for(i in 1:nrow(uniq.refs)){
    # i <- 1
    flag <- 0
    a.ref <- uniq.refs[i, "key.or.doi"]
    # a.ref <- "10.4312/dp.46.22"
    if(verbose){
      print(paste0("[", as.character(i), "] ", a.ref))
    }
    # BibTex - - - - - - - - - - - - - - - - - -
    if(grepl("^[[:upper:]]", a.ref)){
      # reuse the bibentry
      a.bibref <- capture.output(print(df.bib[c(a.ref)]))
      a.citation <- paste0(a.bibref, collapse = " ")
      uniq.refs[i, "long.ref"] <- a.citation
      flag <- 1
    }
    # DOIs (all start with '10.' or 'https://doi.org/10') - - - - - - - -
    # a.ref <- "https://doi.org/10.1002/ajpa.23468"
    if(grepl("^10\\.", a.ref) | grepl("^https://doi.org", a.ref) | grepl("^https://dx.doi.org", a.ref)){
      a.ref <- gsub("https://doi.org/", "", a.ref)
      a.ref <- gsub("http://dx.doi.org/", "", a.ref)
      a.ref <- paste0("https://doi.org/", a.ref)
      Sys.sleep(sleep.sec)
      tryCatch(
        expr = {
          if(verbose){
            print("  - DOI OK")
          }
          uniq.refs[i, "long.ref"] <- RefManageR::GetBibEntryWithDOI(a.ref)
          # uniq.refs[i, "long.ref"] <- as.character(RefManageR::GetBibEntryWithDOI(a.ref))
        },
        error = function(e){
          if(verbose){
            print("  - DOI error -> 'MISSING REF'")
          }
          uniq.refs[i, "long.ref"] <- "MISSING REF"
        }
        # warning=function(cond) {
        #   # uniq.refs[i, "reference"] <- "a.ref" # not working
        #   print("  - DOI not recover")
        # }
      )
      flag <- 1
    }
    # others
    if(flag == 0){
      uniq.refs[i, "long.ref"] <- "MISSING REF"
      if(verbose){
        print("  - 'MISSING REF'")
      }
    }
  }
  # merge C14 data and references
  df.c14 <- merge(df.c14, uniq.refs, by.x = "bib_url", by.y = "key.or.doi", all.x = T)
  df.c14$bib <- NULL
  colnames(df.c14)[which(names(df.c14) == "long.ref")] <- "bib"
  # colnames(df.c14)[which(names(df.c14) == "C14BP")] <- "C14Age"
  return(df.c14)
}
