#' Download and reshape netCDF files from NASA's NEX-GDDP dataset.
#'
#' @param Path Directory where URLs files are located.
#' @param FileURLs Filename of .csv with URLs to download GCM data.
#' @param Var Variable to download data (Pp, Tmax or Tmin).
#' @param RangeLat Limits of latitudes to subset data
#' @param RangeLon Limits of longitudes to subset data
#' @return NEX’s netCDF files for a region and variable of interest.
#' @export
GCM_download_data <- function(FileURLs, Var, RangeLat, RangeLon){

    # Conditional to identify a variable process
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

    # Load require packages
    if("ncdf4" %in% rownames(installed.packages()) == FALSE){
    install.packages("ncdf4")
    }
    if("RCurl" %in% rownames(installed.packages()) == FALSE){
    istall.packages("RCurl")
    }
    library(ncdf4)
    library(RCurl)

    # Create a directory to store donloaded data
    dir.create('Downloaded')
    
    # Load and read URLs from a .csv file
    Links  <- read.table(FileURLs, sep=',', header=F)
    nLinks <- nrow(Links)

    # Start a loop
    for (i in 1:nLinks){
        
        # Read the NEX-GDDP URL to download
        URL    <- as.vector(Links[i,])

        # Extract netCDF filename
        ncfname <- strsplit(URL, 'v1.0/')[[1]][2]
    
            # Download just in case filename doesn't exist yet
            if (file.exists(ncfname)==FALSE){

              # Download netCDF file
              message(paste0('Downloading: ', ncfname))
              download.file(URL, destfile=file.path('Downloaded','netCDF_File.nc'), method="libcurl")

              # Read netCDF file
              ncFile <- nc_open(file.path('Downloaded','netCDF_File.nc'))
              var    <- ncvar_get(ncFile, Variable)
              lat    <- ncvar_get(ncFile, 'lat')
              lon    <- ncvar_get(ncFile, 'lon')
              time   <- ncvar_get(ncFile, 'time')

              # Sbset data for a study area
              lon.reg <- subset(lon, (lon> RangeLon[1]) & (lon< RangeLon[2]))
              lat.reg <- subset(lat, (lat> RangeLat[1]) & (lat< RangeLat[2]))
              var.reg <- var[match(lon.reg, lon), match(lat.reg, lat),] # [longitud, latitud, tiempo]

              # Define netCDF dimensions
              londim   <- ncdim_def("lon","degrees_east", as.double(lon.reg))
              latdim   <- ncdim_def("lat","degrees_north", as.double(lat.reg))
              timedim  <- ncdim_def("time",'days since 2005-01-01 00:00:00',as.double(time))

              # Define netCDF variables
              fillvalue <- 1.00000002004088e+20
              VarDef   <- ncvar_def(Variable, Units, list(londim,latdim,timedim), fillvalue, NameVar, prec="single")

              # Create a netCDF
              ncout <- nc_create(file.path('Downloaded',ncfname),list(VarDef),force_v4=T)

              # Assign variables
              ncvar_put(ncout,VarDef,var.reg)

              # Assign attributes
              ncatt_put(ncout,"lon","axis","X")
              ncatt_put(ncout,"lat","axis","Y")
              ncatt_put(ncout,"time","axis","T")

              # Close open netCDFs
              nc_close(ncout)
              nc_close(file.path('Downloaded',ncFile))
              unlink(file.path('Downloaded','netCDF_File.nc'))
              message('Done!')
            }
        }
  }
# End (not run)

