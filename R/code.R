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
    download.data(url = opendata.url, destfile = opendata.file.gz)
    R.utils::gunzip(file.path(getwd(), "data", opendata.file.gz), remove = FALSE)
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

#' Title
#'
#' @param df
#' @param ip.col
#'
#' @return
#' @export
#'
#' @examples
add.numeric.ip <- function(df, ip.col) {
  # you can't use $ to select a column via d$a.
  # R will look for a column whose name is a.
  # Instead, you can do either d[[a]] or d[,a].
  df[[paste(ip.col, ".num", sep = "")]] <- iptools::ip_to_numeric(df[[ip.col]])
  return(df)
}

#' Title
#'
#' @param df
#' @param nrows
#'
#' @return
#' @export
#'
#' @examples
generate.df <- function(df, nrows) {
  # Returns data frame with n first rows
  partial.df <- df[1:nrows,]
  return(partial.df)
}

#' Title
#'
#' @param df.maxmind
#' @param df.scans
#'
#' @return
#'
#' @examples
geolocate.ip.range <- function(df.maxmind, df.scans) {
  # Foreach IP (source and destination) identify network range using parallel computing"
  no_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(no_cores)
  parallel::clusterExport(cl, "df.maxmind", envir = environment())
  df.scans$sloc <- sapply(df.scans$saddr.num,
                          function(ip)
                            which((ip >= df.maxmind$min_numeric) &
                                    (ip <= df.maxmind$max_numeric)))
  df.scans$dloc <- sapply(df.scans$daddr.num,
                          function(ip)
                            which((ip >= df.maxmind$min_numeric) &
                                    (ip <= df.maxmind$max_numeric)))
  parallel::stopCluster(cl)
  rm(cl, no_cores)
  return(df.scans)
}

#' Title
#'
#' @param df.maxmind
#' @param df.scans
#'
#' @return
#'
#' @examples
join.tidy.data <- function(df.maxmind, df.scans) {
  # Join and tidy data frame
  suppressMessages(library(dplyr))
  df <- dplyr::left_join(df.scans, df.maxmind, by = c("sloc" = "rowname"))
  df <- dplyr::select(df, timestamp_ts, saddr, latitude, longitude, accuracy_radius,
                      is_anonymous_proxy, is_satellite_provider)
  names(df) <- c("timestamp_ts", "saddr", "slatitude", "slongitude",
                 "accuracy_radius", "is_anonymous_proxy", "is_satellite_provider")
  df.dst <- df.scans %>%
    left_join(df.maxmind, by = c("dloc" = "rowname")) %>%
    select(daddr, latitude, longitude)
  names(df.dst) <- c("daddr", "dlatitude", "dlongitude")
  df <- dplyr::bind_cols(df, df.dst)
  rm(df.dst, df.scans)
  return(df)
}

#' CodeBook session2.
#'
#' @return Generates "geoftps.rds" RDS file
#' @export
#'
#' @examples
codebook.ftp <- function() {
  url <- NULL
  sample <- 500
  output.name <- "geoftps.rds"
  codebook(url, sample, output.name)
}

#' Title
#'
#' @param url
#' @param sample
#' @param output.name
#'
#' @return Generates output.name RDS file
#' @export
#'
#' @examples
codebook <- function(url, sample, output.name) {
  df.tcp <- get.opendata(url) # Default value: tcp21
  df.maxmind <- get.mixmind()
  df.tcp <- add.numeric.ip(df.tcp, "saddr")
  df.tcp <- add.numeric.ip(df.tcp, "daddr")

  # Get a sample of scans
  df.tcp.sample <- generate.df(df.tcp, sample)

  # Expanding MaxMind network ranges
  df.maxmind <- cbind(df.maxmind, iptools::range_boundaries(df.maxmind$network))
  df.maxmind$rowname <- as.integer(row.names(df.maxmind))

  df.tcp.sample <- geolocate.ip.range(df.maxmind, df.tcp.sample)

  # Join and tidy data frame
  df <- join.tidy.data(df.maxmind, df.tcp.sample)

  # Set categoric variables as factors
  df$is_anonymous_proxy <- as.factor(df$is_anonymous_proxy)
  df$is_satellite_provider <- as.factor(df$is_satellite_provider)
  saveRDS(object = df, file = file.path(getwd(), "data", output.name))
}

