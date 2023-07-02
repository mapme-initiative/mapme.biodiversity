devtools::install_github("cboettig/minioclient")

library(minioclient)
install_mc()
test <- mc_ls("s3/fbedecarrats/mapme_biodiversity", recursive = TRUE)

# Launch locally on Windows
system(paste0(getwd(),
              "/trials/minio.exe server C:/minio --console-address :9090"))
# Environment variables
Sys.setenv("AWS_ACCESS_KEY_ID" = "minioadmin",
           "AWS_SECRET_ACCESS_KEY" = "minioadmin",
           "AWS_S3_ENDPOINT"= "localhost:9000",
           "AWS_SESSION_TOKEN" = "")
# With minioclient
mc_alias_set(alias = "minio", scheme = "http")
# with aws.s3




library(aws.s3)
bucketlist(region = "", use_https = FALSE)

put_bucket("myuser", region = "", use_https = FALSE)

my_files <- list.files("trials/mapme_senegal", recursive = TRUE, full.name = TRUE) %>%
  str_remove("trials/")
map2(my_files, my_files, put_object, bucket = "myuser", region = "", use_https = FALSE)

my_s3_bucket <- get_bucket("myuser", region = "", use_https = FALSE)

put_bucket("myuser/mapme_senegal", region = "", use_https = FALSE)

my_s3_porfolio <- get_bucket("myuser/mapme_senegal", region = "",
                             use_https = FALSE)

bucket_name <- stringr::str_split_1(my_porfolio_name, "/")[1]




list.test <- function(x, ...) {
  if(class(x) == "s3_bucket") {
    endpoint_as_str <- deparse(substitute(x))
    bucket_name_str <- stringr::str_split_1(endpoint_as_str, "/")[1]
    print(paste(endpoint_as_str, bucket_name_str))
  }
}

list.test(my_s3_bucket)


list.test(
)
eval(parse(text = "my_s3_bucket"))

# Emulate list.files
list.files.s3_bucket <- function(x, ...) {
  unname(purrr::map_chr(x, "Key"))
}


list.files <- function(x, pattern = ".*", ...) {
  if (class(x) == "s3_bucket") {
    unname(purrr::map_chr(x, "Key")) |>
      stringr::str_subset(pattern)
  } else {
    base::list.files(x, ...)
  }
}


list.dirs <- function(x, ...) {
  if

}



list.files(my_s3_bucket, pattern = ".gpkg$")
list.files("trials/mapme_senegal/gmw", pattern = ".gpkg$", full.names = TRUE)

