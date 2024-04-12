#' Read, format and merge Leapfrog dataset and cultures with NeoNet
#' # v. "C:\Rprojects\leapfrog"
#'
#' @name neo_leapfrog
#'
#' @description Read, format and merge Leapfrog dataset and cultures with NeoNet. v. entites: "https://api.nakala.fr/data/10.34847/nkl.3d8bv011/e201bf47cdabfa9dfba7590845d32d3062cd82a1"
#'
#' @param data.neonet the URL of the NeoNet dataset (Default: Fedora / Uni Pisa)
#' @param data.leapfrog the URL of the Leapfrog dataset (Default: local)
#' @param cultures.leapfrog the URL of the file where the Leapfrog culture are listed (Default: local)
#' @param ref.period period referenced in NeoNet (and colors). A TSV file.
#' @param all.x preserve all records of NN (TRUE) or not (Default: FALSE)
#' @param DT if TRUE export as interactive datatable (HTML)
#'
#' @details columns NN and LF are hexa colors mapping respectively 'Period' (Neonet dataset) and 'Cultural_Complex' (LeapFrog dataset) columns
#'
#' @return
#'
#' @examples
#'
#' # Export as DT interactive dataframe
#' neo_leapfrog(DT = T)
#'
#' @export
neo_leapfrog <- function(data.neonet = "https://digitallib.unipi.it/fedora/objects/mag:1062/datastreams/MMf3519905c5344b41cf3ae62e0df4d70b/content", # https://raw.githubusercontent.com/historical-time/caa23/main/neonet/data/140_id00140_doc_elencoc14.csv",
                         data.leapfrog = "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/leapfrog/Binder.csv",
                         cultures.leapfrog = "https://raw.githubusercontent.com/zoometh/neonet/main/doc/data/leapfrog/cultures.tsv",
                         # ref.period = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/periods.tsv",
                         all.x = F,
                         DT = F,
                         verbose = T){
  `%>%` <- dplyr::`%>%`
  # TODO: red the GH periods.tsv NeoNet colors
  lcul_col <- list(# colors
    EM = "#0000CF", # BLUE
    MM = "#1D1DFF", #
    LM = "#3737FF", #
    LMEN = "#6A6AFF", #
    UM = "#8484FF", #
    EN = "#FF1B1B", # RED
    EMN = "#FF541B", #
    MN = "#FF8D1B", #
    LN = "#FFC04D", #
    UN = "#E7E700" # NEO UNDEF.
  )
  # periods.colors <- read.csv(ref.period, sep = "\t")
  # lcul_col <- setNames(periods.colors$color, periods.colors$period)
  # lcul_col1 <- periods.colors$color
  # names(lcul_col1) <- periods.colors$period
  nn.peri <- as.data.frame(t(t(lcul_col)))
  nn.peri$V1 <- as.character(nn.peri$V1)
  nn.peri$Period <- rownames(nn.peri)
  
  nn.data <- read.csv(paste0(file = data.neonet),
                      header = T, sep = "\t")
  lf.data <- read.csv(paste0(file = data.leapfrog),
                      header = T,
                      sep = ";")
  lf.cult <- read.csv(paste0(file = cultures.leapfrog), fileEncoding = "ISO-8859-1",
                      header = T,
                      sep = "\t")
  # lf.cult[] <- lapply(lf.cult, function(x) {
  #   if (is.character(x)) {
  #     iconv(x, from = "ISO-8859-1", to = "UTF-8")
  #   } else {
  #     x
  #   }
  # })
  # lf.cult <- read.table(paste0(file = cultures.leapfrog), encoding = "UTF-8",
  #                       header = T,
  #                       sep = "\t")
  # data <- read.csv(cultures.leapfrog, sep="\t", fileEncoding="ISO-8859-1")
  
  # merge data
  df.data <- merge(nn.data, lf.data, by.x = "LabCode", by.y = "Lab_count_id.", all.x = all.x)
  df.data <- df.data[ , c(1:7, 21, 22, 25)]
  # clean cultures
  df.cult <- lf.cult[rowSums(lf.cult == "") != ncol(lf.cult),] # empty row
  df.cult <- df.cult[ , c(1:3, 5)]
  # merge df with LF cultures
  df <- merge(df.data, df.cult, by.x = "Cultural_complex", by.y = "code_aspect", all.x = T)
  # merge df with NN periods
  df <- merge(df, nn.peri, by = "Period", all.x = T)
  
  # reorder
  df <- df[order(df$SiteName), ]
  rownames(df) <- seq(1, nrow(df))
  # selected columns + colors
  col.col <- c("V1", "SiteName", "Country", "Period", "PhaseCode", "C14SD", "C14Age", "X14C_age_BP", "Cultural_complex", "Complexe.culturel", "Cultural_aspect", "hexa")
  df <- df[ , col.col]
  names(df)[names(df) == 'V1'] <- 'NN'
  names(df)[names(df) == 'hexa'] <- 'LF'
  
  # Replace invalid UTF-8 characters
  df <- df %>%
    dplyr::mutate(across(where(is.character), ~iconv(., from = "UTF-8", to = "UTF-8", sub = "")))
  
  
  # df[, argColRng] <- df[, dataColRng] < Argu
  # output
  if(DT){
    library(dplyr)
    df <- DT::datatable(df,
                        width = "100%",
                        height = "100%",
                        extensions = 'Buttons',
                        options = list(
                          dom = 'Blfrtip',
                          buttons = c('copy', 'csv', 'excel', 'pdf'),
                          lengthMenu = list(c('50', '100', '200', -1),
                                            c('50', '100', '200', 'All')),
                          paging = T
                        )) %>%
      DT::formatStyle(
        "NN",
        # "Cultural_complex",
        backgroundColor = DT::styleEqual(df$NN,
                                         df$NN)) %>%
      DT::formatStyle(
        "LF",
        # "Period",
        backgroundColor = DT::styleEqual(df$LF,
                                         df$LF))
  }
  return(df)
}

df <- neo_leapfrog(DT = T)

# df <- df %>%
#   mutate(across(where(is.character), ~iconv(., from = "unknown", to = "UTF-8", sub = "byte")))
# 
# # Now try to render your datatable
# DT::datatable(df)
