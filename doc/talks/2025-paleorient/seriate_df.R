library(dplyr)

root <- "C:/Users/TH282424/Rprojects/neonet/"
root.path <- paste0(root, "doc/talks/2025-paleorient/")
obj.case.out <- paste0(root.path, "img/")

df <- openxlsx::read.xlsx(paste0(root.path, "carpo.xlsx"))
df <- df[order(df$MEDIAN), ]
df <- df %>%
  tidyr::separate_rows(Archaeological.site, sep = "\\+")  # Expand list into separate rows
df <- as.data.frame(df)
df$idf <- paste0(df$Archaeological.site, "/", df$PHASE)
rownames(df) <- df$idf
df$idf <- df$Archaeological.site <- df$PHASE <- df$MEDIAN <- NULL
for(meth in c("Identity", "PCA")){
  ordre <- seriation::seriate(df, method = meth)
  # PNG
  png(filename = paste0(obj.case.out, "presence_cerealia_taxa_", meth, ".png"), 
      width = 13, height = 11, units = "cm", res = 300)
  seriation::bertinplot(
    as.matrix(df), 
    ordre,
    gp_labels = grid::gpar(cex = 0.6)  # Adjust font size here
  )
  dev.off()
  # SVG
  svg(filename = paste0(obj.case.out, "presence_cerealia_taxa_", meth, ".svg"), 
      width = 13, height = 11)
  seriation::bertinplot(
    as.matrix(df), 
    ordre,
    gp_labels = grid::gpar(cex = 1)  # Adjust font size here
  )
  dev.off()
}

