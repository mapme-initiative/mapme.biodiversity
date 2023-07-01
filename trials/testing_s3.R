devtools::install_github("cboettig/minioclient")

library(minioclient)
install_mc()
test <- mc_ls("s3/fbedecarrats/mapme_biodiversity", recursive = TRUE)

# Launch locally on Windows
system(paste0(getwd(),
              "/trials/minio.exe server C:/minio --console-address :9090"))
