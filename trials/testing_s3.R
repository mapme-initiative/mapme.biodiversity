devtools::install_github("cboettig/minioclient")

library(minioclient)
test <- mc_ls("s3/fbedecarrats/mapme_biodiversity", recursive = TRUE)
