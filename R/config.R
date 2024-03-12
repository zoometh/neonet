koppen_codes <- c("Af", "Am", "Aw", "BWh", "BWk", "BSh", "BSk",
                  "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc", "Cfa",
                  "Cfb", "Cfc", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa",
                  "Dwb", "Dwc", "Dwd", "Dfa", "Dfb", "Dfc", "Dfd", "ET", "EF")
koppen_colors <- c("#0000FF", "#0078FF", "#46AAFF", "#FF0000", "#FF9696", "#F5A500", "#FFDC64",
                   "#FFFF00", "#C8C800", "#969600", "#96FF96", "#64C864", "#329632", "#C8FF50",
                   "#64FF50", "#32C800", "#FF00FF", "#C800C8", "#963296", "#966496", "#AAAFFF",
                   "#5A78DC", "#4B50B4", "#320087", "#00FFFF", "#37C8FF", "#007D7D", "#00465F",
                   "#B2B2B2", "#666666")
kcc_colors <- setNames(koppen_colors, koppen_codes)
# ## colors
# koppen_codes <- c("Af", "Am", "Aw", "BWh", "BWk", "BSh", "BSk", 
#                   "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc", "Cfa", 
#                   "Cfb", "Cfc", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", 
#                   "Dwb", "Dwc", "Dwd", "Dfa", "Dfb", "Dfc", "Dfd", "ET", "EF")
# hex_colors <- c("#0000FF", "#0078FF", "#46AAFF", "#FF0000", "#FF9696", "#F5A500", "#FFDC64", 
#                 "#FFFF00", "#C8C800", "#969600", "#96FF96", "#64C864", "#329632", "#C8FF50", 
#                 "#64FF50", "#32C800", "#FF00FF", "#C800C8", "#963296", "#966496", "#AAAFFF", 
#                 "#5A78DC", "#4B50B4", "#320087", "#00FFFF", "#37C8FF", "#007D7D", "#00465F", 
#                 "#B2B2B2", "#666666")
# # koppen_df <- data.frame(code = koppen_codes, hexColor = hex_colors)
# color_vector <- setNames(hex_colors, koppen_codes)

rename_c14bazAAR <- c(
  sourcedb = "sourcedb",
  SiteName = "site",
  LabCode = "labnr",
  C14Age = "c14age",
  C14SD = "c14std",
  db_period = "period.x",
  db_culture = "culture.x",
  Period = "class",
  lon = "lon",
  lat = "lat"
)