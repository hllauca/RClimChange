#' Download and reshape netCDF files from NASA's NEX-GDDP dataset.
#'
#' @param Path Directory where URLs files are located.
#' @param FileURLs Filename of .csv with URLs to download GCM data.
#' @param Var Variable to download data (Pp, Tmax or Tmin).
#' @param RangeLat Limits of latitudes to subset data
#' @param RangeLon Limits of longitudes to subset data
#' @return NEX’s netCDF files for a region and variable of interest.
#' @export
GCM_download_data <- function(Path, FileURLs, Var, RangeLat, RangeLon){

    if (Var=='Pp'){
      Variable  <- 'pr'
      NameVar   <- "Precipitation"
      Units     <- "kg m-2 s-1"
    }

    if (Var=='Tmax'){
      Variable  <- 'tasmax'
      NameVar   <- "Daily Maximum Near-Surface Air Temperature"
      Units     <- "K"
    }

    if (Var=='Tmin'){
      Variable  <- 'tasmin'
      NameVar   <- "Daily Minimum Near-Surface Air Temperature"
      Units     <- "K"
    }


    # Cargar librerias
    if("ncdf4" %in% rownames(installed.packages()) == FALSE){
      install.packages("ncdf4")
    }
    library(ncdf4)

    if("RCurl" %in% rownames(installed.packages()) == FALSE){
      install.packages("RCurl")
    }
    library(RCurl)


    # Directorio de trabajo
    setwd(Path)


    # Leer URL de descarga
    Links  <- read.table(FileURLs, sep=',', header=F)
    nLinks <- nrow(Links)


    for (i in 1:nLinks){

        URL    <- as.vector(Links[i,])

        # Extraer nombre
        ncfname <- strsplit(URL, 'v1.0/')[[1]][2]

            if (file.exists(ncfname)==FALSE){

              # Descargar archivo .nc
              message(paste0('Downloading: ', ncfname))
              download.file(URL, destfile='netCDF_File.nc', method="auto")


              # Leer netCDF original
              ncFile <- nc_open('netCDF_File.nc')
              var    <- ncvar_get(ncFile, Variable)
              lat    <- ncvar_get(ncFile, 'lat')
              lon    <- ncvar_get(ncFile, 'lon')
              time   <- ncvar_get(ncFile, 'time')


              # Cortar datos para el Peru
              lon.reg <- subset(lon, (lon> RangeLon[1]) & (lon< RangeLon[2]))
              lat.reg <- subset(lat, (lat> RangeLat[1]) & (lat< RangeLat[2]))
              var.reg <- var[match(lon.reg, lon), match(lat.reg, lat),] # [longitud, latitud, tiempo]


              # Definir dimensiones
              londim   <- ncdim_def("lon","degrees_east", as.double(lon.reg))
              latdim   <- ncdim_def("lat","degrees_north", as.double(lat.reg))
              timedim  <- ncdim_def("time",'days since 2005-01-01 00:00:00',as.double(time))


              # Definir variables
              fillvalue <- 1.00000002004088e+20
              VarDef   <- ncvar_def(Variable, Units, list(londim,latdim,timedim), fillvalue, NameVar, prec="single")


              # Crear archivo netCDF
              ncout <- nc_create(ncfname,list(VarDef),force_v4=T)


              # Asignar variables
              ncvar_put(ncout,VarDef,var.reg)


              # Adicionar atributos
              ncatt_put(ncout,"lon","axis","X")
              ncatt_put(ncout,"lat","axis","Y")
              ncatt_put(ncout,"time","axis","T")


              # Cerrar y guardar archivo netCDF
              nc_close(ncout)
              unlink('netCDF_File.nc')
              message('Done!')
            }
        }
  }

