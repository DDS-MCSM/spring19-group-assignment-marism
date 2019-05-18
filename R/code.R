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
#' @param
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

#' @title Obtención fichero opendata
#' @description Función que se conecta a la web de rapid7 que lleva implicita, y se descarga un fichero en formato gz, el cual posteriormente lo descomprime y nos lo retorna en formato csv, borrando el fichero comprimido. En esta funcion se utlizan otras funciones ya descritas con anterioridad, por ejemplo download.data
#' @details Funcion que nos retorna un fichero con las IP que tienen abierto el puerto TCP
#' @param opendata.url Lugar donde iremos a buscar los datos (opcional)
#' @param verbose Mostrar comentarios
#' @return df.tcp21
#' @export
#' @examples
#' get.opendata()
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
  if (!file.exists(file.path(getwd(), "data", opendata.file))) {
    download.data(url = opendata.url, destfile = opendata.file.gz, verbose)
    R.utils::gunzip(file.path(getwd(), "data", opendata.file.gz))
  }
  df.tcp <- read.csv(file.path(getwd(), "data", opendata.file), stringsAsFactors = FALSE)
  return(df.tcp)
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
#' @examples
#' add.numeric.ip(df, ip.col)
add.numeric.ip <- function(df, ip.col) {
  # you can't use $ to select a column via d$a.
  # R will look for a column whose name is a.
  # Instead, you can do either d[[a]] or d[,a].
  df[[paste(ip.col, ".num", sep = "")]] <- iptools::ip_to_numeric(df[[ip.col]])
  return(df)
}

#' @title Funció para obtener las n primeras muestras de un data frame
#' @description Función a la que le pasas un data frame con muchos datos y te retorna las n linias que le pases
#'como parametro
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

#' @title Unió de resultados
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

#' @title Funcion general
#' @description Función que ejecuta ordenadamente todas las funciones decladaras, y simula el comportamiento
#' del codigo facilitado en la práctica.
#' @param url Url de opendata.rapid7.com con el fichero a evaluar
#' @param sample Numero de muestras.
#' @param output.name Nombre del fichero de salida donde guardaremos los datos.
#' @param verbose Mostrar comentarios
#' @return Generates output.name RDS file
#' @export
#' @examples
#' codebook("https://opendata.rapid7.com/sonar.tcp/2019-04-21-1555815947-http_get_5555.csv.gz", 500, "output.rds", TRUE)
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
  syso(verbose, "Tramiento de los resultados....")
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
  #syso(c("Los resuldos los podra ver en la carpeta data ",file))
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
