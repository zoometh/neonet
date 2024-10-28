#' @name neo_dbs_sitename_dates
#'
#' @description Avoid site names mispelling (ex: Franchthi from one database, and Franchthi Cave from another database) by performing a left join to replace SiteName in df.c14 with SiteName from sitenames when there's a match on AlternativeNames
#'
#' @param df.c14 a dataset of dates in a GeoJSON file (coming from the export of the NeoNet app)
#' @param verbose if TRUE (default) then display different messages.
#'
#' @return A list with ggplot ($map) and a dataframe ($data)
#'
#' @examples
#' 
#' myc14data <- "C:/Rprojects/neonet/results/neonet_2023-09-23.geojson"
#' neo_isochr(df.c14 = myc14data)
#' shell.exec(myc14data)
#'
#'
#' @export
neo_dbs_sitename_dates <- function(sitenames.equiv = "https://raw.githubusercontent.com/zoometh/neonet/main/inst/extdata/c14_corrected_sitenames.tsv"){
  sitenames <- read.csv2(sitenames.equiv, sep = "\t")
  `%>%` <- dplyr::`%>%`
  # Perform a left join to replace SiteName in df.c14 with SiteName from sitenames when there's a match on AlternativeNames
  df <- df.c14 %>%
    dplyr::left_join(sitenames, by = c("SiteName" = "AlternativeNames")) %>%
    dplyr::mutate(SiteName = dplyr::coalesce(SiteName.y, SiteName)) %>%
    dplyr::select(-SiteName.y)
  return(df)
}

# library(dplyr)
# library(purrr)
# 
# # Join and replace SiteName where there's a match, and capture the changes in a nested list
# changes <- df.c14 %>%
#   left_join(sitenames, by = c("SiteName" = "AlternativeNames")) %>%
#   mutate(new_SiteName = coalesce(SiteName.y, SiteName)) %>%
#   filter(SiteName != new_SiteName) %>%
#   transmute(SiteName = new_SiteName, AlternativeName = SiteName)
# 
# # Create the nested list of changes
# change_list <- pmap(changes, function(SiteName, AlternativeName) list(SiteName = SiteName, AlternativeName = AlternativeName))
# 
# # Update df.c14 with the new SiteName values
# df.c14 <- df.c14 %>%
#   left_join(sitenames, by = c("SiteName" = "AlternativeNames")) %>%
#   mutate(SiteName = coalesce(SiteName.y, SiteName)) %>%
#   select(-SiteName.y)
# 
# # Display the result and changes
# head(df.c14)
# change_list


length(unique(df.c14$SiteName))

# Load necessary libraries
