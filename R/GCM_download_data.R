#' Download and reshape netCDF files from NASA's NEX-GDDP dataset.
#'
#' @param FileURLs Text file with NEX-GDDP URLs to be downloaded.
#' @param Variable Variable of interest to be downloaded (pr, tasmax or tasmin). 'pr' as default.
#' @param RangeLat Range (min,max) of latitudes to subset data.
#' @param RangeLon Range (min,max) of longitudes to subset data.
#' @return NetCDF files extracted for an study region and variable of interest.
#' @examples
#' require(RClimChange)
#'
#' GCM_download_data(FileURLs='pr_rcp4.5.txt', #Download precipitation for RCP 4.5
#'                   Variable='pr',
#'                   RangeLat=c(-20.5,3.2),
#'                   RangeLon=c(277.5,295.6)) #Extent for Peru
#'
#' @export
#' @import  ncdf4
#' @import  RCurl
#' @import  tictoc
GCM_download_data <- function(FileURLs, Variable='pr', RangeLat, RangeLon){

    # Load packages
    library(ncdf4)
    library(RCurl)
    library(tictoc)

    # Conditional to identify the selected variable
    if (Variable=='pr'){
      NameVar   <- "Precipitation"
      Units     <- "kg m-2 s-1"
    }
    if (Variable=='tasmax'){
      NameVar   <- "Daily Maximum Near-Surface Air Temperature"
      Units     <- "K"
    }
    if (Variable=='tasmin'){
      NameVar   <- "Daily Minimum Near-Surface Air Temperature"
      Units     <- "K"
    }

    # Create a directory to store donloaded data
    dir.create(Variable)

    # Load and read URLs from a .csv file
    Links  <- read.table(FileURLs, header=F)
    nLinks <- nrow(Links)

    # Start a loop
    for (i in 1:nLinks){

        # Read the NEX-GDDP URL to download
        URL    <- as.vector(Links[i,])

        # Extract netCDF filename
        ncfname <- strsplit(URL, 'v1.0/')[[1]][2]

            # Download just in case filename doesn't exist yet
            if (file.exists(file.path(Variable, ncfname)) == FALSE){

              # Download netCDF file
              cat('\f')
              message(paste0('Downloading: ', ncfname))
              message(paste0('File ',i,' of ',nLinks))
              message(paste0('Please wait...'))
              download.file(URL, destfile=file.path(Variable,'Temporal_file.nc'), method="libcurl")

              # Read netCDF file
              message(paste0('Subseting data and saving a new netcdf'))
              message(paste0('Please wait...'))
              ncFile <- nc_open(file.path(Variable,'Temporal_file.nc'))
              var    <- ncvar_get(ncFile, Variable)
              lat    <- ncvar_get(ncFile, 'lat')
              lon    <- ncvar_get(ncFile, 'lon')
              time   <- ncvar_get(ncFile, 'time')

              # Subset data for a study area
              lon.reg <- subset(lon, (lon> RangeLon[1]) & (lon< RangeLon[2]))
              lat.reg <- subset(lat, (lat> RangeLat[1]) & (lat< RangeLat[2]))
              var.reg <- var[match(lon.reg, lon), match(lat.reg, lat),] # [longitud, latitud, tiempo]

              # Define netCDF dimensions
              londim   <- ncdim_def("lon","degrees_east", as.double(lon.reg))
              latdim   <- ncdim_def("lat","degrees_north", as.double(lat.reg))
              timedim  <- ncdim_def("time",'days since 2005-01-01 00:00:00',as.double(time))

              # Define netCDF variables
              fillvalue <- 1.00000002004088e+20
              VarDef    <- ncvar_def(Variable, Units, list(londim,latdim,timedim), fillvalue, NameVar, prec="single")

              # Create a netCDF
              ncout <- nc_create(file.path(Variable,ncfname),list(VarDef),force_v4=T)

              # Assign variables
              ncvar_put(ncout,VarDef,var.reg)

              # Assign attributes
              ncatt_put(ncout,"lon","axis","X")
              ncatt_put(ncout,"lat","axis","Y")
              ncatt_put(ncout,"time","axis","T")

              # Close opened files
              nc_close(ncout)
              nc_close(ncFile)
              unlink(file.path(Variable,'Temporal_file.nc'))
              message('Done!')
              message(' ')
              message(' ')
            }
        }
  }
# End (not run)
