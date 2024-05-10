#' @include register.R
.match_opts <- function(user_opts, defaults) {
  if (length(user_opts) != 0) {
    override_i <- which(names(defaults) %in% names(user_opts))
    if (length(override_i) > 0) defaults[override_i] <- user_opts[names(defaults)[override_i]]
    add_i <- which(!names(user_opts) %in% names(defaults))
    if (length(add_i) > 0) defaults <- c(defaults, user_opts[add_i])
  }
  unlist(defaults[order(names(defaults))])
}

set_mapme_gdal_config <- function(opts) {
  stopifnot(is.character(opts))
  .pkgenv$gdal_config <- opts[order(names(opts))]
}

get_mapme_gdal_config <- function() {
  .pkgenv$gdal_config
}

#' @include register.R
gdal_cloud_opts <- function(...) {
  user_opts <- unlist(list(...))
  defaults <- c(
    "VSI_CACHE" = "TRUE",
    "GDAL_CACHEMAX" = "30%",
    "VSI_CACHE_SIZE" = "10000000",
    "GDAL_INGESTED_BYTES_AT_OPEN" = "32000",
    "GDAL_DISABLE_READDIR_ON_OPEN" = "FALSE",
    "GDAL_READDIR_LIMIT_ON_OPEN" = "1000",
    "GDAL_NUM_THREADS" = "ALL_CPUS",
    "CPL_VSIL_USE_TEMP_FILE_FOR_RANDOM_WRITE" = "YES",
    "GDAL_HTTP_MULTIPLEX" = "YES",
    "GDAL_HTTP_MERGE_CONSECUTIVE_RANGES" = "YES",
    "GDAL_HTTP_USERAGENT" = "mapme-r"
  )
  .match_opts(user_opts, defaults)
}

#' @include register.R
gdal_http_opts <- function(...) {
  user_opts <- list(...)
  defaults <- list(
    "GDAL_HTTP_HEADER_FILE" = NULL,
    "GDAL_HTTP_CONNECTTIMEOUT" = "20",
    "GDAL_HTTP_COOKIE" = NULL,
    "GDAL_HTTP_COOKIEFILE" = NULL,
    "GDAL_HTTP_COOKIEJAR" = NULL,
    "GDAL_HTTP_NETRC" = NULL,
    "GDAL_HTTP_NETRC_FILE" = NULL,
    "GDAL_HTTP_HEADERS" = NULL,
    "GDAL_HTTP_SSLCERT" = NULL,
    "GDAL_HTTP_SSLCERTTYPE" = NULL,
    "GDAL_HTTP_SSLKEY" = NULL,
    "GDAL_HTTP_KEYPASSWD" = NULL,
    "GDAL_HTTP_VERSION" = "2",
    "GDAL_HTTP_MULTIPLEX" = "YES",
    "GDAL_HTTP_MULTIRANGE" = "YES",
    "GDAL_HTTP_MERGE_CONSECUTIVE_RANGES" = "YES",
    "GDAL_HTTP_USERPWD" = NULL,
    "GDAL_HTTP_BEARER" = NULL,
    "GDAL_HTTP_PROXY" = NULL,
    "GDAL_HTTP_PROXYUSERPWD" = NULL,
    "GDAL_PROXY_AUTH" = NULL,
    "GDAL_HTTP_TIMEOUT" = "60",
    "GDAL_HTTP_USERAGENT" = "mapme-r"
  )
  .match_opts(user_opts, defaults)
}
#' @include register.R
gdal_s3_opts <- function(...) {
  user_opts <- unlist(list(...))
  defaults <- list(
    "AWS_NO_SIGN_REQUEST" = NULL,
    "AWS_ACCESS_KEY_ID" = NULL,
    "AWS_SECRET_ACCESS_KEY" = NULL,
    "AWS_SESSION_TOKEN" = NULL,
    "CPL_AWS_CREDENTIALS_FILE" = NULL,
    "AWS_DEFAULT_PROFILE" = "default",
    "AWS_PROFILE" = "default",
    "AWS_CONFIG_FILE" = "~/.aws/credentials",
    "AWS_ROLE_ARN" = NULL,
    "AWS_WEB_IDENTITY_TOKEN_FILE" = NULL,
    "AWS_REGION" = "us-east-1",
    "AWS_DEFAULT_REGION" = NULL,
    "AWS_REQUEST_PAYER" = NULL,
    "AWS_S3_ENDPOINT" = NULL,
    "AWS_HTTPS" = NULL,
    "AWS_VIRTUAL_HOSTING" = NULL,
    "VSIS3_CHUNK_SIZE" = NULL,
    "CPL_VSIL_CURL_IGNORE_GLACIER_STORAGE" = NULL,
    "CPL_VSIL_CURL_IGNORE_STORAGE_CLASSES" = NULL,
    "CPL_VSIS3_USE_BASE_RMDIR_RECURSIVE" = NULL,
    "CPL_VSIS3_CREATE_DIR_OBJECT" = NULL
  )
  .match_opts(user_opts, defaults)
}

#' @include register.R
gdal_gcs_opts <- function(...) {
  user_opts <- list(...)
  defaults <- list(
    "GS_NO_SIGN_REQUEST" = NULL,
    "GS_SECRET_ACCESS_KEY" = NULL,
    "GS_ACCESS_KEY_ID" = NULL,
    "GS_OAUTH2_REFRESH_TOKEN" = NULL,
    "GS_OAUTH2_CLIENT_ID" = NULL,
    "GS_OAUTH2_CLIENT_SECRET" = NULL,
    "GS_OAUTH2_PRIVATE_KEY" = NULL,
    "GS_OAUTH2_PRIVATE_KEY_FILE" = NULL,
    "GS_OAUTH2_CLIENT_EMAIL" = NULL,
    "GS_OAUTH2_SCOPE" = NULL,
    "CPL_GS_CREDENTIALS_FILE" = "~/.boto",
    "GS_USER_PROJECT" = NULL
  )
  .match_opts(user_opts, defaults)
}

#' @include register.R
gdal_az_opts <- function(...) {
  user_opts <- list(...)
  defaults <- list(
    "AZURE_NO_SIGN_REQUEST" = NULL,
    "AZURE_STORAGE_CONNECTION_STRING" = NULL,
    "AZURE_STORAGE_ACCESS_TOKEN" = NULL,
    "AZURE_STORAGE_ACCOUNT" = NULL,
    "AZURE_STORAGE_ACCESS_KEY" = NULL,
    "AZURE_STORAGE_SAS_TOKEN" = NULL,
    "AZURE_IMDS_OBJECT_ID" = NULL,
    "AZURE_IMDS_CLIENT_ID" = NULL,
    "AZURE_IMDS_MSI_RES_ID" = NULL
  )
  .match_opts(user_opts, defaults)
}

#' @include register.R
gdal_swift_opts <- function(...) {
  user_opts <- list(...)
  defaults <- list(
    "SWIFT_STORAGE_URL" = NULL,
    "SWIFT_AUTH_TOKEN" = NULL,
    "SWIFT_AUTH_V1_URL" = NULL,
    "SWIFT_USER" = NULL,
    "SWIFT_KEY" = NULL
  )
  .match_opts(user_opts, defaults)
}
