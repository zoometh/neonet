

######
# BDA: not working: zip file is corrupt
# bda_url <- "https://api.nakala.fr/data/10.34847/nkl.dde9fnm8/189e04d917ffd68352a389006f357b58efda855e"
# bda_dir <- tempdir()
# bda_dir <- getwd()
# bda_files <- file.path(bda_dir, "bda.zip")
# download.file(bda_url, destfile = bda_files, method = "auto")
# unzip(bda_files, unzip = "internal")

######
# leapfrog: not working: zip file is corrupt
bda_url <- "https://api.nakala.fr/data/10.34847/nkl.dde9fnm8/189e04d917ffd68352a389006f357b58efda855e"
bda_dir <- tempdir()
bda_dir <- getwd()
bda_files <- file.path(bda_dir, "bda.zip")
download.file(bda_url, destfile = bda_files, method = "auto")
unzip(bda_files, unzip = "internal")