#' Armed Conflict Location & Event Data (ACLED)
#'
#' From ACLED's homepage: *The Armed Conflict Location & Event Data Project
#' (ACLED) is a disaggregated data collection, analysis, and crisis mapping
#' project. ACLED collects information on the dates, actors, locations,
#' fatalities, and types of all reported political violence and protest events
#' around the world. The ACLED team conducts analysis to describe, explore, and
#' test conflict scenarios, and makes both data and analysis open for free use
#' by the public.*
#'
#' In order to access data from the ACLED API, you first must register
#' an account. Make sure you register with your institutional domain
#' (e.g., organization, university, or company) email address rather than
#' your personal email address, in order to be able to use the API,
#' as explained [here](https://acleddata.com/myacled-faqs).
#' Note that the ACLED API provides a *living database*
#' with single events being altered or removed altogether over time.
#'
#' @name acled
#' @param years A numeric vector specifying the years for which to make
#'   ACLED data available (between 1997 and today).
#' @param email Email addressed used to register with ACLED (see Details).
#' @param password Password used to register with ACLED (see Details).
#' @param accept_terms A logical indicating if you agree to abide by ACLED's terms
#'   of use. Defaults to FALSE, thus must be manually set to TRUE.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references Raleigh, C., Kishi, R. & Linke, A. Political instability patterns
#'   are obscured by conflict dataset scope conditions, sources, and coding
#'   choices. Humanit Soc Sci Commun 10, 74 (2023).
#'   \doi{doi:10.1057/s41599-023-01559-4}
#' @source Armed Conflict Location & Event Data Project (ACLED).
#' @include register.R
#' @importFrom httr2 request req_perform resp_body_json
#' @export
get_acled <- function(
    years = 1997,
    email = Sys.getenv("ACLED_EMAIL"),
    password = Sys.getenv("ACLED_PASSWORD"),
    accept_terms = FALSE) {

  if (is.null(mapme_options()[["outdir"]])) {
    warning(paste(
      "ACLED layers must be downloaded from the source location",
      "irrespective if `outdir` was specified or not."
    ))
  }

  # check input arguments
  if (!accept_terms) {
    msg <- "Please read and agree to ACLED's Terms of Use here:\nhttps://acleddata.com/terms-and-conditions"
    stop(msg)
  } else {
    if (isTRUE(mapme_options()[["verbose"]])) {
      msg <- "You agreed to abide to ACLED's Terms of Use (https://acleddata.com/terms-and-conditions)."
      message(msg)
    }
  }
  if (is.null(email) | email == "") {
    msg <- "Please specify your email registered with ACLED."
    stop(msg)
  }
  if (is.null(password) | password == "") {
    msg <- "Please specify your password registered with ACLED."
    stop(msg)
  }
  years <- check_available_years(years, 1997:as.integer(format(Sys.Date(), "%Y")))

  function(x,
           name = "acled",
           type = "vector",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    # token authorization url
    .token_url <- "https://acleddata.com/oauth/token"
    acled_yearly <- purrr::map_chr(years, function(year) {
      filename <- sprintf("acled_events_%s.gpkg", year)
      # make full path to output file
      if (is.null(outdir)) {
        filename <- file.path(tempdir(), filename)
      } else {
        filename <- file.path(outdir, filename)
      }
      # if already available just return the file name
      if (spds_exists(filename)) {
        return(filename)
      }
      # start building the request
      base_url <- "https://acleddata.com/api/acled/read"
      req <- httr2::request(base_url)
      # add year as query parameter
      req <- httr2::req_url_query(req, year = year)
      # authenticate the request
      req <- httr2::req_oauth_password(
        req,
        client = httr2::oauth_client("acled", .token_url),
        username = email,
        password = password
      )
      # prepare for page iteration
      next_page <- TRUE
      page <- 1
      data_lst <- NULL
      running_count <- 0L
      # iterate pages
      while (next_page) {
        req <- httr2::req_url_query(req, page = page)
        resp <- httr2::req_perform(req)
        cnt <- httr2::resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)
        # check if we still have some data
        count <- cnt$count
        if (count == 0L) {
          next_page <- FALSE
          next
        }
        # increment running count
        running_count <- running_count + count
        # get data as data.frame
        events <- cnt$data
        # store events data.frame in list
        if (page == 1) {
          data_lst <- list(events)
          total_count <- cnt$total_count
        } else {
          data_lst[[page]] <- events
        }
        # increment page number
        page <- page + 1
      }
      # check everything OK
      if (running_count != total_count) {
        msg <- sprintf("ACLED API returned only %d events out of %d.", running_count, total_count)
        stop(msg)
      }
      if (is.null(data_lst)) {
        stop("ACLED API returned 0 events.")
      }
      # rbind list into global data.frame
      data <- purrr::list_rbind(data_lst)
      # convert to sf object
      data <- sf::st_as_sf(data,
                           coords = c("longitude", "latitude"),
                           crs = sf::st_crs("EPSG:4326"))
      # convert all non-geometry columns to character to be compatible with previous version
      # temporarily extract geometry
      geom <- sf::st_geometry(data)
      # convert non-geometry columns
      data_df <- sf::st_drop_geometry(data)
      data_df[] <- lapply(data_df, as.character)
      # reattach geometry
      data <- sf::st_sf(data_df, geom)
      # write the file and return file name
      sf::write_sf(data, filename)
      return(filename)
    })
    # make footprint
    bbox <- c(xmin = -180.0, ymin = -90.0, xmax = 180.0, ymax = 90.0)
    fps <- sf::st_as_sfc(sf::st_bbox(bbox, crs = "EPSG:4326"))
    fps <- sf::st_as_sf(rep(fps, length(acled_yearly)))
    fps[["source"]] <- acled_yearly
    fps <- make_footprints(fps, what = "vector")
  }
}

register_resource(
  name = "acled",
  description = "Armed Conflict Location & Event Data (ACLED)",
  licence = "Visit acleddata.com",
  source = "Visit acleddata.com",
  type = "vector"
)
