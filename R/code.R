#' @title Imprirmir por consola
#' @description Funcion que imprime por pantalla, simpre y cuando este habilitada a true la variable verbose
#' @details No se le pasa ningun valor a la función ya que dejamos como predefinido "data" por defecto, sin capacidad de ser alterado.
#' @param x Texto que queremos imprimir
#' @param verbose Mostrar comentarios
#' @export
#' @examples
#' syso(TRUE, "Hello World!!")
syso <- function(verbose, x) {
  if (verbose) print(x)
}


#' @title Valida la existencia del directorio de trabajo "data"
#' @description Función que establece el directorio de trabajo en data y en caso de no existir lo crea.
#' @details No se le pasa ningun valor a la función ya que dejamos como predefinido "data" por defecto, sin capacidad de ser alterado.
#'
#' @export
#' @examples
#' create.dir.data()
create.dir.data <- function() {
  dir.data <- file.path(getwd(), "data")
  if (!dir.exists(dir.data)) {
    dir.create(dir.data)
  }
}

#' @title Descarga de fichero de internet
#' @description Función que descarga un fichero de una url y lo guarda con el nombre del fichero que indiquemos.
#' @details Descarga de la '$url' y lo guarda con el nombre de '$destfile', internamente hace una llamada a 'create.dir.data()', para asegurarnos que el directorio de trabajo data existe, por otro lado valida que el nombre del fichero ya no este en el repositorio de trabajo y solo en ese caso se lo descarga.
#' @param url Lugar donde iremos a buscar los datos
#' @param destfile Nombre del archivo de destino con el que guardaremos los datos
#' @param verbose Mostrar comentarios
#' @export
#' @examples
#' download.data(url, destfile)
download.data <- function(url, destfile, verbose) {
  create.dir.data()
  destination.file.path <- file.path(getwd(), "data", destfile)
  if (!file.exists(destination.file.path)) {
    download.file(url = url, destfile = destination.file.path)
  } else {
    syso(verbose, "El fichero ya existe")
  }
}

#' @title Get opendata csv or json file from rapid7.com and convert it into dataframe object.
#' @description This function downloads a compressed '.gz' file from opendata.rapid7.com website, and imports file from csv or json into dataframe object.
#' @param opendata.url Any url of https://opendata.rapid7.com
#' @param verbose Print logs
#' @return dataframe
#' @export
#' @examples
#' get.opendata(url, verbose)
get.opendata <- function(opendata.url = NULL, verbose) {
  # Obtain raw data from opendata.rapid7.com website
  if (is.null(opendata.url)) { # Default file
    opendata.url <- "https://opendata.rapid7.com/sonar.tcp/2019-04-04-1554350684-ftp_21.csv.gz"
  }
  if (strsplit(opendata.url, '/')[[1]][3] != "opendata.rapid7.com") {
    stop('URL is not valid!')
  }
  # split URL by '/' and get last value (name on file)
  opendata.file.gz <- tail(strsplit(opendata.url, '/')[[1]], 1)
  # remove '.gz' extension name
  opendata.file <- substr(opendata.file.gz, 1, nchar(opendata.file.gz) - 3)
  # Get file format
  file.format <- substr(opendata.file, nchar(opendata.file) - 3, nchar(opendata.file))
  # Download and extract
  if (!file.exists(file.path(getwd(), "data", opendata.file))) {
    download.data(url = opendata.url, destfile = opendata.file.gz, verbose)
    R.utils::gunzip(file.path(getwd(), "data", opendata.file.gz))
  } else {
    syso(verbose, paste("File", opendata.file, "already exists. No need to download."))
  }
  if (file.format == ".csv") {
    df <- read.csv(file.path(getwd(), "data", opendata.file), stringsAsFactors = FALSE)
    syso(verbose, paste(opendata.file, "imported."))
  } else if (file.format == "json") {
    lines <- readLines(file.path(getwd(), "data", opendata.file))
    df <- data.frame(do.call(rbind, lapply(lines, jsonlite::fromJSON)))
    syso(verbose, paste(opendata.file, "imported."))
  } else {
    syso(verbose, paste("Format error (", file.format, ")"))
  }
  return(df)
}

#' @title Obtención del fichero maxmid
#' @description Función que se conecta a la web de geolite.maxmind que lleva implicita, y se descarga un fichero .zip, de la lista de ficheros unicamente se queda con el que cumple la expresion regular *GeoLite2-City-Blocks-IPv4.csv lo descomprime y nos lo retona como csv
#' @details Funcion que nos retorna un fichero con la geolicalizacion de IPV4 por ciudad
#' @param verbose Mostrar comentarios
#' @return df.maxmind
#' @export
#' @examples
#' get.maxmind()
get.maxmind <- function(verbose) {
  # Maxmind - Obtain raw data (city)
  maxmind.url <- "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City-CSV.zip"
  maxmind.file <- "maxmind.zip"
  download.data(url = maxmind.url, destfile = maxmind.file, verbose)
  zipfiles <- unzip(zipfile = file.path(getwd(), "data", maxmind.file), list = T)
  maxmind.source <- zipfiles$Name[grep(pattern = ".*GeoLite2-City-Blocks-IPv4.csv", x = zipfiles$Name)]
  unzip(zipfile = file.path(getwd(), "data", maxmind.file),
        exdir = file.path(getwd(), "data"),
        files = maxmind.source)
  maxmind.source <- file.path(getwd(), "data", maxmind.source)
  df.maxmind <- read.csv(maxmind.source, stringsAsFactors = FALSE)
  return(df.maxmind)
}

#' @title Función para convertir las IP en numeros
#' @description Dada una IP te la convierte a numero quitando los espacios.
#' @details Le pasas el datframe y la columna donde se encuentran las IP y el te retorna el mismo data frame convertiendo las IP en nuemeros
#' @param df Fichero que contiene los data frame
#' @param ip.col Columna donde estan las IP
#' @return df
#' @export
#' @examples
#' \dontrun{
#' add.numeric.ip(df, ip.col)
#' }
add.numeric.ip <- function(df, ip.col) {
  # you can't use $ to select a column via d$a.
  # R will look for a column whose name is a.
  # Instead, you can do either d[[a]] or d[,a].
  df[[paste(ip.col, ".num", sep = "")]] <- iptools::ip_to_numeric(df[[ip.col]])
  return(df)
}

#' @title Funció para obtener las n primeras muestras de un data frame
#' @description Función a la que le pasas un data frame con muchos datos y te retorna las n linias que le pases
#' como parametro
#' @details Necesitas pasarle un df y un valor numerico
#' @param df Fichero que contiene el data frame
#' @param nrows Numero de muestras que queremos tratar
#' @return partial.df
#' @export
#' @examples
#' generate.df(df, 50)
generate.df <- function(df, nrows) {
  # Returns data frame with n first rows
  partial.df <- df[1:nrows,]
  return(partial.df)
}

#' @title Identificacion del rango de la IP utlizando threads paralelos
#' @description Función que permite optimizar al maximo la capacidad de procesado del ordenador, enviando procesos por cada uno de los cores que tiene disponible el ordenador exceptuando de uno que lo deja para procesos propios de la maquina.
#' @details Por cada IP, miramos a que rango de IP pertenece, ya en el df.maxmind, fichero que nos indica la geolocalizacion, nos lo especifican los rangos de IP con mnascaras de red equivalente. Nos retorna el df
#' @param df.maxmind Data frame de maxmind
#' @param df.scans Data frame de scans
#' @return df.scans
#' @examples
#' geolocate.ip.range(df.maxmind, df.scans)
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

#' @title Unión de resultados
#' @description Función que nos une los dos data frame, para optener los resultados que queremos obtener la geolocalización a nivel de ciudad de las IP identifcadas con vulnerabilidad.
#' @details Dados dos df de entrada, retorna uno con los resultados que queremos mostrar
#' @param df.maxmind Data frame de maxmind
#' @param df.scans Data frame de scans
#' @return df
#' @examples
#' join.tidy.data(df.maxmind, df.scans)
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


#' @title Geolocate ip address using maxmind
#' @description Used in UntrustedServers.Rmd
#' @param df.maxmind Dataframe Maxmind con la geolocalización
#' @param df.raw Dataframe con las muestras
#'
#' @return df.raw.geo
#' @export
geolocate.http.responses <- function(df.maxmind, df.raw) {
  no_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(no_cores)
  parallel::clusterExport(cl, "df.maxmind", envir = environment())
  df.raw$loc <- parallel::parSapply(cl, df.raw$ip.num,
                                    function(ip)
                                      which((ip >= df.maxmind$min_numeric) &
                                              (ip <= df.maxmind$max_numeric)))
  parallel::stopCluster(cl)
  if (is.list(df.raw$loc)) {
    df.raw$loc <- as.numeric(df.raw$loc)
  }
  rm(cl, no_cores)
  # De ambos ficheros después de procesarlos, los juntamos en uno único, conservando únicamente aquellos campos que de verdad nos aportan informacion.
  suppressMessages(library(dplyr))
  df <- dplyr::left_join(df.raw, df.maxmind, by = c("loc" = "rowname"))
  df.raw.geo <- dplyr::select(df, data, ip, port, latitude, longitude, accuracy_radius)
  names(df.raw.geo) <- c("data", "ip", "port", "latitude", "longitude", "accuracy_radius")
  rm(df)
  return(df.raw.geo)
}


#' @title Return cpe22 or cpe23 from vendor and version
#' @description Requiered net.security package updated.
#'
#' @param vendor Fabricante
#' @param version Version
#' @param cpe.version Version del cpe
#' @param cpes dataframe con los cpe
#'
#' @return cpe
#' @export
get.cpe <- function(vendor, version, cpe.version = 22, cpes = NULL) {
  if (is.null(cpes)) { # Default file
    cpes <- net.security::GetDataFrame("cpes")
  }
  if (is.na(vendor) | is.na(version)) return(NA)
  if (tolower(vendor) == "apache") {
    # if vendor is "apache", search only product "http_server"
    i <- which(cpes$vendor == tolower(vendor) & cpes$version == version
               & cpes$product == "http_server")
  } else if (tolower(vendor) == "microsoft-iis") {
    # if vendor is "Microsoft-IIS", search only product "internet_information_server	"
    i <- which(cpes$vendor == "microsoft" & cpes$version == version
               & cpes$product == "internet_information_server")
  } else  {
    i <- which(cpes$vendor == tolower(vendor) & cpes$version == version)
  }
  row <- cpes[i,]
  if (nrow(row) == 0) { # If no results, try using our vendor as product
    i <- which(cpes$product == tolower(vendor) & cpes$version == version)
    row <- cpes[i,]
  }
  if (cpe.version == 22 & nrow(row) != 0) {
    return(head(row$cpe.22, 1)) # only return the first match
  } else if (cpe.version == 23 & nrow(row) != 0) {
    return(head(row$cpe.23, 1)) # only return the first match
  } else {
    return(NA)
  }
}


#' @title Return data frame of CVEs for a specific CPE
#'
#' @param cpe CPE
#' @param cves dataframe with cves
#'
#' @return score
#' @export
get.cve <- function(cpe, cves = NULL) {
  if (is.null(cves)) { # Default file
    cves <- net.security::GetDataFrame("cves")
  }
  cves.found <- cves[grep(cpe, cves$vulnerable.configuration), ]
  return(cves.found)
}


#' @title Return an averaged score of cvss2 or cvss3
#'
#' @param df.cve data frame with list of CVEs
#' @param cvss CVSS version. 2 or 3
#'
#' @return score
#' @export
get.score <- function(df.cve, cvss = 2) {
  if (nrow(df.cve) == 0) {
    return(0)
  }
  if (cvss == 2) {
    score <- (sum(df.cve$cvss2.score) + sum(df.cve$cvss2.score.exploit) + sum(df.cve$cvss2.score.impact)) / nrow(df.cve)
  } else if (cvss == 3) {
    score <- (sum(df.cve$cvss3.score) + sum(df.cve$cvss3.score.exploit) + sum(df.cve$cvss3.score.impact)) / nrow(df.cve)
  }
  return(score)
}

#' @title Return maxim score of cvss2 or cvss3
#'
#' @param df.cve data frame with list of CVEs
#' @param cvss CVSS version. 2 or 3
#'
#' @return max.score
#' @export
get.score.max <- function(df.cve, cvss = 2) {
  if (nrow(df.cve) == 0) {
    return(0)
  }
  if (cvss == 2) {
    max.score <- max(df.cve$cvss2.score)
  } else if (cvss == 3) {
    max.score <- max(df.cve$cvss3.score)
  }
  return(max.score)
}

#' @title Parsing headers of dataframe with data column encoded in base64
#' @description Decode data in base64, extract headers, status, http version and server
#' @param df.raw Data frame con las muestras
#' @return parsed df
#' @export
parse.headers <- function(df.raw) {
  # Convert base64 to raw data
  df.raw$data <- sapply(df.raw$data, function(d) jsonlite::base64_dec(d))
  # Delete NULL chars
  df.raw$data <- sapply(df.raw$data, function(d) d[!d == '00'])
  # raw to char
  df.raw$data <- sapply(df.raw$data, function(d) rawToChar(d))

  # Get Response Headers
  df.raw$headers <- sapply(df.raw$data, function(d) head(unlist(strsplit(d, '\r\n\r\n', useBytes = TRUE)), 1))
  # Get Response Content
  # df$body <- sapply(df$data, function(d) tail(strsplit(d, '\r\n\r\n', useBytes = TRUE)[1], -1))
  # Convert headers into vector of lines
  df.raw$headers <- sapply(df.raw$headers, function(d) unlist(strsplit(d, '\r\n', useBytes = TRUE)))

  # First line: version and status
  df.raw$http.status <- sapply(df.raw$headers, function(d) d[1])
  df.raw$http.status <- enc2utf8(df.raw$http.status)
  df.raw$http.version <- sapply(df.raw$http.status, function(d) substr(d, 1, 8))
  # Delete first line (http version and status) of headers
  df.raw$headers <- sapply(df.raw$headers, function(d) tail(d, length(d) - 1))

  # Extract "Server" value
  df.raw$server <- sapply(df.raw$headers,
                          function(h) sapply(strsplit(h, ": ", useBytes = TRUE),
                                             function(v) {
                                               if (grepl("Server", v[1], useBytes = TRUE)) {v[2]}
                                             }))

  # Remove NULL values
  df.raw$server <- sapply(df.raw$server, function(x) paste(plyr::compact(x), ""))
  # Delete last space added
  df.raw$server <- sapply(df.raw$server, function(x) gsub(" $","", x))

  df.raw$vendor <- sapply(df.raw$server, function(x) unlist(strsplit(x, "/", useBytes = TRUE))[1])
  df.raw$version <- sapply(df.raw$server, function(x) unlist(strsplit(x, "/", useBytes = TRUE))[2])
  df.raw$version <- sapply(df.raw$version, function(x) unlist(strsplit(x, " ", useBytes = TRUE))[1])

  return(df.raw)
}


#' @title Generación del fichero de muestras
#' @description Función que utiliza el fichero usado en el informe Rmd
#' @param port Nos indica el puerto del que estamos realizando el analisis
#' @param rows Filas que queremos extraer
#' @param verbose Si queremos mostrar por consola más información de las operaciones que va realizando
#' @return df Dataframe con las 50 muestras
#' @export
http.responses <- function(port=80, rows=50, verbose=TRUE) {

  # Example of rds file generation:
  # user@linux$ head -n 50  2019-04-22-1555944117-http_get_80.json > sample80_50.json
  # sample.file <- file.path(getwd(), "data", "sample80_50.json")
  # lines <- readLines(sample.file)
  # df.raw <- plyr::ldply(lines, function(x) as.data.frame(jsonlite::fromJSON(x), stringsAsFactors = FALSE))
  # saveRDS(object = df.raw, file = file.path(getwd(), "data", "df_http_get_80_raw_50.rds"))

  rds.file <- file.path(getwd(), "data", paste("df_http_get_", port, "_raw_", rows, ".rds", sep = ""))
  if (file.exists(rds.file)) {
    df.raw <- readRDS(rds.file)
  } else {
    stop(verbose, "File does not exist")
  }

  df <- parse.headers(df.raw)

  return(df)
}

#' @title Generación de mapa por pais
#' @description Funcion que te calcula el pais dada la long/lat
#' @param pointsDF Dataframe donde donde tenemos la geolocalización
#' @param mapping Tipo de mapa que queremos pintar "world"
#' @export
latlong2map <- function(pointsDF, mapping) {
  # load up the map data
  local.map <- map(mapping, fill=TRUE, col="transparent", plot=FALSE)
  # pull out the IDs from the name
  IDs <- sapply(strsplit(local.map$names, ":"), function(x) x[1])
  # Prepare SpatialPolygons object
  maps_sp <- map2SpatialPolygons(local.map, IDs=IDs,
                                 proj4string=CRS("+proj=longlat +datum=wgs84"))
  # Convert pointsDF to a SpatialPoints object
  pointsSP <- SpatialPoints(pointsDF,
                            proj4string=CRS("+proj=longlat +datum=wgs84"))
  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- over(pointsSP, maps_sp)
  # Return the names of the Polygons object containing each point
  mapNames <- sapply(maps_sp@polygons, function(x) x@ID)
  # now return a vector of names that match the points
  mapNames[indices]
}


#' @title Funcion general
#' @description Función que ejecuta ordenadamente todas las funciones decladaras, y simula el comportamiento del codigo facilitado en la práctica. Utiliza los datos de maxmind para geolocalizar IPs, pero a parte del archivo con datos sobre conexiones tcp21, también acepta cualquier archivo de https://opendata.rapid7.com/sonar.tcp
#' @param url Url de opendata.rapid7.com con el fichero a evaluar
#' @param sample Numero de muestras.
#' @param output.name Nombre del fichero de salida donde guardaremos los datos.
#' @param verbose Mostrar comentarios
#' @return Generates output.name RDS file
#' @export
#' @examples
#' url <- "https://opendata.rapid7.com/sonar.tcp/2019-04-21-1555815947-http_get_5555.csv.gz"
#' sample <- 500
#' output.name <- "output.rds"
#' verbose <- TRUE
#' codebook(url, sample, output.name, verbose)
codebook <- function(url, sample, output.name, verbose = FALSE) {

  syso(verbose, "Inicio de la ejecución....")
  syso(verbose, "...")
  syso(verbose, "Inicio de la obtención fichero opendata...")
  df.tcp <- get.opendata(url, verbose) # Default value: tcp21
  syso(verbose, "Fin de la optencion fichero opendata")

  syso(verbose, "Inicio de la obtención del fichero maxmind...")
  df.maxmind <- get.maxmind(verbose)
  syso(verbose, "Fin obtención del fichero maxmind.")

  df.tcp <- add.numeric.ip(df.tcp, "saddr")
  df.tcp <- add.numeric.ip(df.tcp, "daddr")

  # Get a sample of scans
  syso(verbose, "Recogiendo muestras de opendata")
  df.tcp.sample <- generate.df(df.tcp, sample)

  # Expanding MaxMind network ranges
  syso(verbose, "Tratamiento de los resultados...")
  df.maxmind <- cbind(df.maxmind, iptools::range_boundaries(df.maxmind$network))
  df.maxmind$rowname <- as.integer(row.names(df.maxmind))

  df.tcp.sample <- geolocate.ip.range(df.maxmind, df.tcp.sample)

  # Join and tidy data frame
  syso(verbose, "Procesando los resultados...")
  df <- join.tidy.data(df.maxmind, df.tcp.sample)

  # Set categoric variables as factors
  syso(verbose, "Guardando resultados...")
  df$is_anonymous_proxy <- as.factor(df$is_anonymous_proxy)
  df$is_satellite_provider <- as.factor(df$is_satellite_provider)
  saveRDS(object = df, file = file.path(getwd(), "data", output.name))
  syso(verbose, paste("Los resuldos se pueden ver en", file.path(getwd(), "data", output.name)))
}

#' @title Ejecución de CodeBook
#' @description Función que inicializa una serie de variables para simular el comportamiento definido en el codigo facilitado en la práctica.
#' @details Dato unas variables llamamos a lo que sería el equivalente a nuestro main, el corzando del progrma
#' @return Generates "geoftps.rds" RDS file
#' @export
#' @examples
#' codebook.ftp()
codebook.ftp <- function() {
  tini <- Sys.time()
  verbose <- TRUE
  url <- NULL
  sample <- 500
  output.name <- "geoftps.rds"
  codebook(url, sample, output.name, verbose)
  fini <- Sys.time()
  fini - tini
}
