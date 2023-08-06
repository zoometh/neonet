setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("R/neo_subset.R")
source("R/neo_bib.R")
source("R/neo_matlife.R")
source("R/neo_calib.R")
source("R/neo_merge.R")
source("R/neo_html.R")
source("R/neo_datamiss.R")
source("R/neo_datasum.R")

# new dataset, atl
# c14
data.c14 <- paste0(getwd(), "/inst/extdata/", "NeoNet_atl_ELR (1).xlsx")
df.bib <- paste0(getwd(), "/inst/extdata/", "NeoNet_atl_ELR.bib")
df.c14 <- openxlsx::read.xlsx(data.c14)
# df.c14 <- df.c14[df.c14$Country == "France", ]
# df.c14 <- df.c14[seq(1, 50), ]
df.c14 <- neo_subset(df.c14,
                     rm.C14Age = TRUE,
                     rm.Spatial = FALSE,
                     rm.Period = FALSE)
df.c14 <- neo_calib(df.c14)
# df.c14 <- neo_bib(df.c14, df.bib)
neo_datamiss(df.c14)
neo_datasum(df.c14)
# for the app, a merge
df.c14 <- neo_merge(df.c14 = df.c14, 
                    data.bib = data.bib, 
                    merge.bib = F)
df.c14 <- neo_matlife(df.c14)
df.c14 <- neo_html(df.c14)
# df.c14$bib <- as.character(df.c14$bib)
write.table(df.c14, "C:/Rprojects/neonet/R/app-dev/c14_dataset_med_x_atl.tsv",
            sep = "\t",
            row.names = FALSE)