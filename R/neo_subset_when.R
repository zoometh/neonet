#' @name neo_subset_when
#'
#' @description Subset an sf object (dataframe of dates) on w-medians and periods
#'
#' @param df.c14 a dataset of dates in a GeoJSON file (coming from the export of the NeoNet app)
#' @param when A tuple in BC.
#' @param period description
#' @param top.date If TRUE, will only keep one datye by site: the oldest for EN, the younger for LM
#'
#' @return A dataset
#'
#' @examples
#' 
#'
#'
#' @export
neo_subset_when <- function(df.c14 = NA, 
                           when = NA,
                           period = NA,
                           top.date = FALSE,
                           col.median = "median",
                           col.period = "Period",
                           verbose = TRUE){
  `%>%` <- dplyr::`%>%`
  df.dates <- df.c14
  if(inherits(when, "numeric")){
    if(verbose){
      print(paste0("Temporal subset on time bounds"))
    }
    df.dates <- df.dates[df.dates$median > -when[1] & df.dates$median < -when[2], ]
    if(verbose){
      print(paste0("    Dataset before filtering on time bounds: ", nrow(df.c14)))
    }
    if(verbose){
      print(paste0("    Dataset after filtering on time bounds: ", nrow(df.dates), " (dates inside the time bounds)"))
    }
  }
  if(inherits(period, "character")){
    df.dates2 <- df.dates
    if(verbose){
      print(paste0("Temporal subset on Period"))
    }
    df.dates2 <- df.dates2[df.dates2$Period == period, ]
    if(verbose){
      print(paste0("    Dataset before filtering on Period: ", nrow(df.dates)))
    }
    if(verbose){
      print(paste0("    Dataset after filtering on Period: ", nrow(df.dates2), " (dates inside the Period)"))
    }
    df.dates <- df.dates2
  }
  if(top.date){
    if(verbose){
      print(paste0("Top dates (Temporal subset)"))
    }
    df.dates3 <- df.dates
    neolithic <- period %in% c("EN", "EMN", "MN", "LN", "UN")
    paleolithic <- !neolithic
    if(neolithic){
      df.dates3 <- df.dates3 %>% 
        dplyr::group_by(SiteName) %>% 
        dplyr::slice_min(median)
    } else {
      df.dates3 <- df.dates3 %>% 
        dplyr::group_by(SiteName) %>% 
        dplyr::slice_max(median)
    }
    if(verbose){
      print(paste0("    Dataset before filtering on the oldest/youngest date by site: ", nrow(df.dates)))
    }
    if(verbose){
      print(paste0("    Dataset after filtering on the oldest/youngest date by site: ", nrow(df.dates3)))
    }
    df.dates <- df.dates3
  }
  return(df.dates)
}