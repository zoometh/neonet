# print info of a given date, from its idf. Usefull to remove wrong dates before neo_isochr()
neo_find_date <- function(df = NA,
                          idf.dates = NA,
                          id.field = "idf",
                          fields = c("idf", "sourcedb", "labcode", "site", "median", "period"),
                          print.it = TRUE){
  dates <- df[df[[id.field]] %in% idf.dates, fields]
  if(print.it){print(dates)}
  return(dates)
}
