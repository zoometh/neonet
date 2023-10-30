library(rcarbon)

source("R/neo_isochr.R")
source("R/neo_spd.R")
source("R/neo_calib.R")

mydir <-  paste0(getwd(), "/doc/presentation/bhdc/img/")
df.c14 <- paste0(mydir, "neonet-data-2023-10-22.geojson")
neo_isochr(df.c14 = df.c14, lbl.dates = TRUE, lbl.time.interv = TRUE, 
           lbl.time.interv.size = 4, lbl.dates.size = 3, 
           outDir = mydir, mapname = "donzere_EN", export = TRUE)

mydir <-  paste0(getwd(), "/doc/presentation/bhdc/img/")
df.c14 <- paste0(mydir, "neonet-data-2023-10-22.geojson")
neo_isochr(df.c14 = df.c14, selected.per = "LM", lbl.dates = TRUE, lbl.time.interv = TRUE, 
           lbl.time.interv.size = 4, lbl.dates.size = 3,
           outDir = mydir, mapname = "donzere_LM",
           export = TRUE)

###



setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("R/neo_subset.R")
source("R/neo_bib.R")
source("R/neo_matlife.R")
source("R/neo_calib.R")
source("R/neo_merge.R")
source("R/neo_html.R")
source("R/neo_datamiss.R")
source("R/neo_datasum.R")
# source("R/neo_doi.R")
source("R/neo_spd.R")
source("R/neo_spdplot.R")


df.c14 <- read.csv2("R/app-dev-neonet2/c14_dataset_med_x_atl_2.tsv", sep = "\t")
neo_datasum(df.c14 = df.c14, info = "stats", export = F)

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
                     rm.Period = TRUE)
df.c14 <- neo_calib(df.c14)
df.c14a <- neo_std(df.c14,
                  out.df.c14.topub = "c14_dataset_atl_4.tsv")
# df.c14 <- neo_std(df.c14,
#                   df.c14.pub = "C:/Rprojects/neonet/inst/extdata/140_140_id00140_doc_elencoc14.tsv",
#                   export = F)
# df.c14.bib <- neo_bib(df.c14, df.bib)
neo_datamiss(df.c14a)
neo_datasum(df.c14a)
neo_spd(df.c14a, mapname = "SPD")

# for the app, a merge
df.c14 <- neo_merge(df.c14 = "C:/Rprojects/neonet/R/app-dev-neonet/c14_dataset_atl.tsv", 
                    df.c14.pub = "C:/Rprojects/neonet/R/app-dev-neonet/NeoNet_Med_v2.tsv",
                    merge.bib = F,
                    write.c14 = T,
                    out.c14.merged.nme = "c14_dataset_med_x_atl_2.tsv")
df.c14 <- neo_matlife(df.c14)
df.c14 <- neo_html(df.c14)
# df.c14$bib <- as.character(df.c14$bib)
write.table(df.c14, 
            file = "C:/Rprojects/neonet/R/app-dev-neonet/c14_dataset_med_x_atl.tsv",
            sep = "\t",
            row.names = FALSE)

###
