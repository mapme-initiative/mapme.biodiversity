.calc_funs <- function(){
  list(
    "cover" = list(
      name = ".calc_cover",
      inputs = list("treecover" = "raster", "lossyear" = "raster"),
      args = list("minSize" = 0:9999,
                  "minCover" = c(15,25,50,75,100),
                  "years" = 2000:2020)
    )
  )
}
