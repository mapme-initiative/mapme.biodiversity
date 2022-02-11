.resources_list <- function(){
  list(
    "treecover" = list("type" = "raster",
                       "source" = "GFW",
                       "downloader" = ".get_treecover",
                       "arguments" = list("vers" = "GFC-2018-v1.6")),
    "lossyear" = list("type" = "raster",
                      "source" = "GFW",
                      "downloader" = ".get_lossyear")
  )
}
