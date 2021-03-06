% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code.R
\name{download.data}
\alias{download.data}
\title{Descarga de fichero de internet}
\usage{
download.data(url, destfile, verbose)
}
\arguments{
\item{url}{Lugar donde iremos a buscar los datos}

\item{destfile}{Nombre del archivo de destino con el que guardaremos los datos}

\item{verbose}{Mostrar comentarios}
}
\description{
Función que descarga un fichero de una url y lo guarda con el nombre del fichero que indiquemos.
}
\details{
Descarga de la '$url' y lo guarda con el nombre de '$destfile', internamente hace una llamada a 'create.dir.data()', para asegurarnos que el directorio de trabajo data existe, por otro lado valida que el nombre del fichero ya no este en el repositorio de trabajo y solo en ese caso se lo descarga.
}
\examples{
download.data(url, destfile)
}
