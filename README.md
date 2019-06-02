
# Group Assignment - Data Driven Security

Group Assignment base repository for the Data Driven Security subject of the [CyberSecurity Management Msc](https://www.talent.upc.edu/ing/professionals/presentacio/codi/221101/cybersecurity-management/).

## Análisis de los servidores que ofrecen servicio por protocolos no seguros

Este informe se centra en realizar un estudio de los tipos de servidores que están ofreciendo respuesta por protocolos no seguro, obtener su geolocalición, versión, tipo de protocolo que están utilizando y respuesta que se obtiene. 
Una vez con estos datos los correlacionaremos con los cve, cpe para obtener las vulnerabilidades de cada uno de ellos y poder estimar un rango de vulnerabilidad. Finalmente correlacionamos los datos del PIB con la geolocalización y ya podemos realizar el estudio.

### Requerimientos

  - Obtener la geolocalización de estos servidores alrededor del mundo
  - Obtener el tipo de servidor que está ofreciendo servició
  - Protocolos de HTTP
  - ¿Los países desarrollados son más seguros que los países subdesarrollados? 
  
  
### Instalación

Si quieres instalar spring19-group-assignment-marism desde github utilizamos los siguiente comandos:

Primero nos descargamos el paquete de devtolls:

```r
install.packages("devtools")
```
Instalamos el pacakge, en de la rama master:

```r
devtools::install_github("DDS-MCSM/spring19-group-assignment-marism") 
```
Instalamos el pacakge, en de la rama devel:

```r
devtools::install_github("DDS-MCSM/spring19-group-assignment-marism", ref = "devel") 
```


### Data acquisition

Todo los datos han sido obtenidos de fuentes publicas y el an?lisis corresponde a la situaci?n en que nos encontrabamos a día 22-04-2019. 

Adjuntamos el detalle de los enlaces:

- [HTTP GET Responses](https://opendata.rapid7.com/sonar.http/)
- [MaxMind IP Geolocation](https://dev.maxmind.com/geoip/geoip2/geolite2/)
- [CPE's](http://static.nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.gz)
- [CVE's MITRE](http://cve.mitre.org/data/downloads/index.html#download)
- [CVE's NIST](https://nvd.nist.gov/download.cfm)
- [PIB](http://api.worldbank.org/v2/es/indicator/NY.GDP.MKTP.CD?downloadformat=csv)

### Funciones desarolladas

- add.numeric.ip -> Función para convertir las IP en números.
- codebook -> Función general, la cual llama a una serie de las funciones desarolladas.
- codebook.ftp -> Inicialización de la funcion codebook.
- create.dir.data -> Valida la existencia del directorio de trabajo "data".
- download.data -> Descargar los dataframe.
- generate.df -> Generar los dataframe.
- geolocate.http.responses -> Geolocalización dada una IP.
- get.cpe -> Tratamiento cpe22 or cpe23 from vendor and version
- get.cve -> Tratamiento de los cve.
- get.maxmind -> Tratamiendo del maxmind.
- get.opendata -> Tratamiento de las muestras de los servidores puerto 80
- get.score -> Obtencion de los cvss2 o cvss3 dependiento del valor de entrada
- http.responses -> Filtrado de las respuestas http
- latlong2map -> Obtencion de los paises dadas las cordenadas  (lon/lat)
- parse.headers -> Limpieza de los headers
- syso -> Imprimir por patalla

### Results / Conclusions.

Detallamos los análisis que hemos realizado:

- La geolocalización de los servidores.
- Los protocolos http que estaban utilizando para comunicarse.
- El codigo de respuesta de estos servidores.
- El tipo de servidor y la versión.
- Las vulnerabilidades de cada uno de los servidores a raiz de la correlación con cpe y cve.
- Los paises en que se encontraban los servidores con vulnerabilidades.

Con estos resultados podemos llegar a decir:

bla bla bla....


Dicho análisis únicamente se ha realizado con 5000 muestras y nos da una estimación de los resultados que podríamos llegar a obtener con el fichero entero. El cual no hemos procesado por la cantidad de información que contenía y la falta de tiempo.
Para futuras versiones de este mismo análisis sería interesante, aumentar el número de muestras con el que hemos realizado el análisis y por otro lado, ejecutar el análisis con ficheros de diferentes días. De esta manera podriamos garantizar que los resultados obtenidos se aproximarían a la realidad.
