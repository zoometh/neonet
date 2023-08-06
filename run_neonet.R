setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("R/neo_subset.R")
source("R/neo_bib.R")
source("R/neo_matlife.R")
source("R/neo_calib.R")
source("R/neo_html.R")
source("R/neo_datamiss.R")
source("R/neo_datasum.R")

# new dataset, atl
# c14
data.c14 <- paste0(getwd(), "/inst/extdata/", "NeoNet_atl_ELR (1).xlsx")
df.bib <- paste0(getwd(), "/inst/extdata/", "NeoNet_atl_ELR.bib")
df.c14 <- openxlsx::read.xlsx(data.c14)
# df.c14 <- df.c14[df.c14$Country == "France", ]
df.c14 <- df.c14[seq(1, 50), ]
df.c14 <- neo_subset(df.c14,
                     rm.C14Age = TRUE,
                     rm.Spatial = FALSE,
                     rm.Period = FALSE)
df.c14 <- neo_calib(df.c14)
df.c14 <- neo_bib(df.c14, df.bib)
df.c14 <- neo_matlife(df.c14)
df.c14 <- neo_calc(df.c14)
neo_datamiss(df.c14)
neo_datasum(df.c14)



# # print
# col.not.used <- c("Anatomical.part.(type)", "OBS", "Reliability", "bib_url", "MaterialSpecies")
# df.c14[ , col.not.used] <- NULL
# View(df.c14)
#
# # export
# # write.table(df.tot, paste0(output.path, "c14_dataset.tsv"),
# #             sep = "\t",
# #             row.names = FALSE)
# # write.table(c14.bibrefs, paste0(output.path, "references.bib"),
# #             sep="\t",
# #             row.names=FALSE)
# # write.table(material.life.duration, paste0(output.path, "c14_material_life.tsv"),
# #             sep="\t",
# #             row.names=FALSE)



