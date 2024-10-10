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
#' In order to access data from the ACLED API, you first must register an
#' an account. Note, that the ACLED API used here provides a *living database*
#' with single events being altered or removed altogether over time.
#'
#' @name acled
#' @param years A numeric vector specifying the years for which to make
#'   ACLED data available (between 1997 and today). Defaults to 2000.
#' @param key ACLED API key obtained by registering with ACLED (see Details).
#' @param email Email addressed used to register with ACLED (see Details).
#' @param accept_terms A logical indicating if you agree to abid by ACLED's terms
#'   of use. Defaults to FALSE, thus must be manually set to TRUE.
#' @keywords resource
#' @returns A function that returns an `sf` footprint object.
#' @references Raleigh, C., Kishi, R. & Linke, A. Political instability patterns
#'   are obscured by conflict dataset scope conditions, sources, and coding
#'   choices. Humanit Soc Sci Commun 10, 74 (2023).
#'   \doi{https://doi.org/10.1057/s41599-023-01559-4}
#' @source Armed Conflict Location & Event Data Project (ACLED).
#' @include register.R
#' @importFrom httr2 request req_perform resp_check_status resp_body_json
#' @export
get_acled <- function(
    years = 2000,
    key = Sys.getenv("ACLED_ACCESS_KEY"),
    email = Sys.getenv("ACLED_ACCESS_EMAIL"),
    accept_terms = FALSE) {
  if (!accept_terms) {
    msg <- "Please read and agree to ACLED's Terms of Use here:\nhttps://acleddata.com/terms-of-use/"
    stop(msg)
  } else {
    msg <- "You agreed to abide to ACLED's Terms of Use (https://acleddata.com/terms-of-use/)."
    message(msg)
  }

  if (is.null(email) | email == "") {
    msg <- "Please specify your email registered with ACLED."
    stop(msg)
  }

  if (is.null(key) | key == "") {
    msg <- "Please specify your API key registered with ACLED."
    stop(msg)
  }

  years <- check_available_years(years, 1997:2024)

  function(x,
           name = "acled",
           type = "vector",
           outdir = mapme_options()[["outdir"]],
           verbose = mapme_options()[["verbose"]]) {
    acled_yearly <- purrr::map_chr(years, function(year) {
      filename <- sprintf("acled_events_%s.gpkg", year)

      if (is.null(outdir)) {
        filename <- file.path(tempdir(), filename)
      } else {
        filename <- file.path(outdir, filename)
      }

      if (spds_exists(filename)) {
        return(filename)
      }

      base_url <- .prep_acled_url(key, email, year)
      next_page <- TRUE
      page <- 1
      data <- NULL

      while (next_page) {
        page_url <- paste0(base_url, sprintf("&page=%s", page))

        req <- request(page_url)
        rsp <- req_perform(req)
        resp_check_status(rsp)
        cnt <- resp_body_json(rsp)

        if (cnt$status != 200) {
          stop("ACLED API query failed with message:\n", cnt$error$message)
        }

        if (cnt$count == 0) {
          next_page <- FALSE
          next
        }

        events <- purrr::map(cnt$data, function(y) as.data.frame(y))
        events <- purrr::list_rbind(events)
        events <- st_as_sf(events,
          coords = c("longitude", "latitude"),
          crs = st_crs("EPSG:4326")
        )

        if (page == 1) {
          data <- list(events)
        } else {
          data[[page]] <- events
        }

        page <- page + 1
      }

      if (is.null(data)) {
        stop("ACLED API returned 0 events.")
      }

      data <- st_sf(tibble::as_tibble(purrr::list_rbind(data)))
      write_sf(data, filename)
      return(filename)
    })

    bbox <- c(xmin = -180.0, ymin = -90.0, xmax = 180.0, ymax = 90.0)
    fps <- st_as_sfc(st_bbox(bbox, crs = "EPSG:4326"))
    fps <- st_as_sf(rep(fps, length(acled_yearly)))
    fps[["source"]] <- acled_yearly
    fps <- make_footprints(fps, what = "vector")
  }
}

.prep_acled_url <- function(key, email, year) {
  url <- "https://api.acleddata.com/acled/read?"
  url <- paste0(url, "terms=accept")
  url <- paste0(url, "&key=", key)
  url <- paste0(url, "&email=", email)
  url <- paste0(url, "&year=", year)
  url
}

register_resource(
  name = "acled",
  description = "Armed Conflict Location & Event Data (ACLED)",
  licence = "Visit acleddata.com",
  source = "Visit acleddata.com",
  type = "vector"
)
