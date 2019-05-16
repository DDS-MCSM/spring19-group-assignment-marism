#' Title
#'
#' @details Download file from 'download.url' and save it as 'destination.file' in dir.data
#' Path is relative to working directory
#'
#' @examples
create.dir.data <- function() {
  dir.data <- file.path(getwd(), "data")
  if (!dir.exists(dir.data)) {
    dir.create(dir.data)
  }
}

#' Download file
#'
#' @details Download file from 'download.url' and save it as 'destination.file' in dir.data
#' Path is relative to working directory
#'
#' @param download.url
#' @param destination.file
#'
#' @export
#'
#' @examples
download.data <- function(url, destfile) {
  create.dir.data()
  destination.file.path <- file.path(getwd(), "data", destfile)
  if (!file.exists(destination.file.path)) {
    download.file(url = url, destfile = destination.file.path)
  }
}

#' Title
#'
#' @param url
#'
#' @return df.tcp21
#' @export
#'
#' @examples
get.opendata <- function(opendata.url = NULL) {
  # Obtain raw data of opendata.rapid7.com website
  if (is.null(opendata.url)) { # Default file
    opendata.url <- "https://opendata.rapid7.com/sonar.tcp/2019-04-04-1554350684-ftp_21.csv.gz"
  }
  # split URL by '/' and get last value (name on file)
  opendata.file.gz <- tail(strsplit(opendata.url, '/')[[1]], 1)
  # remove '.gz' extension name
  opendata.file <- substr(opendata.file.gz, 1, nchar(opendata.file.gz) - 3)
  if (!file.exists(file.path(getwd(), "data", opendata.file))) {
    download.data(url = opendata.url, destfile = opendata.file)
    R.utils::gunzip(opendata.file.gz)
  }
  df.tcp21 <- read.csv(file.path(getwd(), "data", opendata.file), stringsAsFactors = FALSE)
  return(df.tcp21)
}

#' Title
#'
#' @return df.maxmind
#' @export
#'
#' @examples
get.mixmind <- function() {
  # Maxmind - Obtain raw data (city)
  maxmind.url <- "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City-CSV.zip"
  maxmind.file <- "maxmind.zip"
  download.data(url = maxmind.url, destfile = maxmind.file)
  zipfiles <- unzip(zipfile = file.path(getwd(), "data", maxmind.file), list = T)
  maxmind.source <- zipfiles$Name[grep(pattern = ".*GeoLite2-City-Blocks-IPv4.csv", x = zipfiles$Name)]
  unzip(zipfile = file.path(getwd(), "data", maxmind.file),
        exdir = file.path(getwd(), "data"),
        files = maxmind.source)
  maxmind.source <- file.path(getwd(), "data", maxmind.source)
  df.maxmind <- read.csv(maxmind.source, stringsAsFactors = FALSE)
  return(df.maxmind)
}

generate.df <- function(nrows) {
  #muestra <- sample(1:nrow(df.tcp21), scope)
  #df.scans <- df.tcp21[muestra,]
  #rm(muestra)
}
