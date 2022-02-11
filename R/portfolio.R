#' Initiate a biodiversity portfolio
#'
#' The function is to be used either on an \code{sf} object representing assets
#' for the initiation of a new portfolio or a character string pointing to a
#' Geopackage written to disk previously by \code{mapme.biodiversity}. Portfolio
#' wide parameters such as the timeframe of the analysis are to be added and initialization.
#'
#' @param x Either an \code{sf} object or a charchter string to a Geopackage written
#'   by \code{mapme.biodiversity}. Only Polygons and MultiPolygons are allowed.
#' @param years A numeric vector indicating the years of analysis.
#' @param sources DEfaults to NA in case of a new initialization. Users can point
#'   to a directory where resource have been previously downloaded.
#' @name mapme_portfolio
#' @return A \code{mapme_portfolio} object.
#' @export
#'
init_portfolio = function(x, years, sources){
  if (!class(x)[1] %in% c("sf", "character")){
    print("Class of x must be either an sf object or a character string pointing to a Geopackage.")
  }
  #if(class(x)[1] == "character") x = st_read(x, quiet = TRUE)
  atts = list(years = years, sources = sources)
  x = as_portfolio(x, atts)
  x
}

#' Convert an object to a portfolio
#'
#' This function is an internal function used to frequently to convert
#' intermediate objects to a portfolio
#'
#' @param x object that will be converted to a portfolio
#' @param ... additional arguments
#' @name mapme_portfolio
#' @export
#'
as_portfolio = function(x, atts) UseMethod("as_portfolio")

#' Conversion method for sf objects
#'
#' Checks to be performed:
#' - has at least one asset
#' - all assets are either Polygon or MultiPolygon (TODO)
#' - is Lat/Lon, otherwise transform to EPSG:4326
#'
#' @param x The sf object to be transformed to a portfolio
#' @param atts All attributes of the original object + portfolio specific attributes
#' @name mapme_portfolio
#' @export
as_portfolio.sf = function(x, atts){
  if(nrow(x)<1) stop("x must contain at least one asset.")
  if(st_crs(x) != st_crs(4326)){
    message("CRS of x is not EPSG:4326. Attempting to transform.")
    x = st_transform(x, 4326)
  }

  #setting portfolio level attributes
  attr(x, "nitems") =  nrow(x)
  attr(x, "bbox") = st_bbox(x)
  # todo: think about ways how users can specify a sources directory
  attr(x, "sources") = atts$sources
  attr(x, "years") = atts$years

  # setting new class
  class(x) = c("mapme_portfolio", class(x))
  x

}


#' Conversion methods for chrachter vector
#'
#' This function is used to re-read an exported Geopackage to the R Session
#' to allow users to continue their analysis in other sessions. Becuase the sizes
#' (e.g. number of rows/columns) very much differ from one indicator to another
#' in order to reduce file size geometries are stored in one table of the Geopackage
#' and metadata and indicator values are stored in related tables. This function
#' will re-rearange the input to the mapme_portfolio object on read time.
#'
#' @param x A chracter vector pointing to a Geopackage
#' @param atts All attributes of the original object + portfolio specific attributes
#' @name mapme_portfolio
#' @export
#'
as_portfolio.character = function(x, atts){
  if(!file.exists(x)){
    stop(sprintf("File %s does not exists. Make sure it points to a Geopackage created by mapme.biodiversity.", x))
  }
  if(tools::file_ext(x) != "gpkg"){
    stop(sprintf("File %s must be a Geopackage created by mapme.biodiversity.", x))
  }
  # Function body of how to read the gpkg back to a portfolio object
  # needs to be developed later once we have completed the write function
  x = st_read(x, quiet = TRUE)
  x = as_portfolio(x, atts)
  x
}

#' Conversion method for a tbl object
#'
#' In order to support a selection of tidyverse verbs which return tibbles,
#' this function will create a mapme_portfolio object from these objects.
#'
#' @param x A tibble returned by a tidyverse verb to be converted to a mapme_portfolio
#' @param atts All attributes of the original object + portfolio specific attributes
#' @name mapme_portfolio
#' @export
#'
as_portfolio.tbl = function(x, atts){
  atts = attributes(x)
  x = st_as_sf(x)
  as_portfolio(x, atts)
}


#' Print method for mapme_portfolio objects
#'
#' @param x mapme_portfolio object
#' @param ... additional arguments
#' @name mapme_portfolio
#' @export
print.mapme_portfolio <- function(x, ...){
  atts = attributes(x)
  cat(sprintf("Bio-Diversity portfolio with %s assets\n", nrow(x)))
  cat(sprintf(
    "Analysis Years: %s\nBounding Box: %s\n",
    paste0(atts$years[1], " to ", atts$years[length(atts$years)]), paste(names(atts$bbox), round(atts$bbox,4), sep = ": ", collapse = " ")))
  cat("Data:\n")
  class(x) =  setdiff(class(x), c("mapme_portfolio", "sf"))
  print(x)
  if(!all(is.na(atts$sources))){
    cat("Available Sources:\n")
    print.data.frame(atts$sources, ...)
  }
  invisible(as_portfolio(x, atts))
}



#' Single bracket selection method for mapme_portfolios
#'
#' @param x a mapme_portfolio object
#' @param i row indicator
#' @param j colomn indicator
#' @param drop logical from sf to make geometry column sticky
#' @name mapme_portfolio
#' @export
#'
"[.mapme_portfolio" <- function(x, i, j, drop = FALSE){
  atts = attributes(x)
  class(x) =  setdiff(class(x), c("mapme_portfolio"))
  x = x[i,j,drop]
  as_portfolio(x, atts)
}

#' Dobuble bracket assigmnent method for mapme_portfolios
#'
#' @param x a mapme_portfolio object
#' @param i element index
#' @param value the value(s) to assign to the element
#' @export
"[[<-.mapme_portfolio" <- function(x, i, value){
  atts = attributes(x)
  class(x) =  setdiff(class(x), c("mapme_portfolio"))
  x[[i]] = value
  as_portfolio(x, atts)
}

#' Dollar sign assignment method for mapme_portfolios
#'
#' @param x a mapme_portfolio object
#' @param i element index
#' @param value the value(s) to assign to the element
#' @name mapme_portfolio
#' @export
"$<-.mapme_portfolio" <- function(x, i, value){
  atts = attributes(x)
  class(x) =  setdiff(class(x), c("mapme_portfolio"))
  x[[i]] = value
  as_portfolio(x, atts)
}


#' Support for geometries retrivals
#'
#' @param x a mapme_portfolio
#' @name mapme_portfolio
#' @export
st_geometry.mapme_portfolio = function(x, ...){
  class(x) =  setdiff(class(x), c("mapme_portfolio"))
  st_geometry(x, ...)
}

