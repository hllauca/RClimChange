#' Download CMIP6 daily data from NCCS THREDDS NEX-GDDP-CMIP6
#' @param location Work directory to store downloaded data.
#' @param model Model names from "https://ds.nccs.nasa.gov/thredds/catalog/AMES/NEX/GDDP-CMIP6/catalog.html". If NULL, all 35 models will be downloaded.
#' @param scenario Choose the scenario to be downloaded ('historical','ssp245', or 'ssp585). More information in "https://www.nccs.nasa.gov/sites/default/files/NEX-GDDP-CMIP6-Tech_Note.pdf".
#' @param variable Choose the variable to be downloaded ('pr':precipitation,'tas': mean air temperature ,'tasmin': minimum air temperature, or 'tasmax': maximum air temperature).
#' @param years Choose data years to be downloaded  (from 1950 to 2014 for the 'historical' scenario, and from 2015 to 2100 for 'ssp245' and 'ssp585' scenarios).
#' @param roi Region of interest coordinates for subsetting data. Please insert c(xmin, xmax, ymin, ymax). If NULL, data with the original extension will be downloaded.
#' @param method Method to be used for downloading files. Current download methods are 'internal', 'wininet' (Windows only), 'libcurl', 'wget' and 'curl'.'curl' as default.
#' @param mode character. The mode with which to write the file. Useful values are "w", "wb" (binary), "a" (append) and "ab". Not used for methods "wget" and "curl".  "wb" recommend for Windows.
#' @return CMIP6 daily data (in netCDF format) for the region of interest.
#' @export
#' @examples
#' # Load package
#' require(RClimChange)
#'
#' # Download daily precipitation (in mm/d) from the BCC-CSM2-MR model for 1990 and the Peruvian domain
#' gcm_download_data(location=getwd(),
#'                   model='BCC-CSM2-MR',
#'                   scenario='historical',
#'                   variable='pr',
#'                   years=1990,
#'                   roi=c(-86,-66,-20,2),
#'                   method='curl')
#'
#' @import  ncdf4
#' @import  RCurl
#' @import  tictoc

gcm_download_data <- function(location,
                              model=NULL,
                              scenario,
                              variable,
                              years,
                              roi,
                              method='curl',
                              mode=NULL){

  tic()
  # If "model=NULL", all available models will be downloaded
  if(is.null(model)==TRUE){
    model <- c('UKESM1-0-LL',
               'TaiESM1',
               'NorESM2-MM',
               'NorESM2-LM',
               'NESM3',
               'MRI-ESM2-0',
               'MPI-ESM1-2-LR',
               'MPI-ESM1-2-HR',
               'MIROC6',
               'MIROC-ES2L',
               'KIOST-ESM',
               'KACE-1-0-G',
               'IPSL-CM6A-LR',
               'INM-CM5-0',
               'INM-CM4-8',
               'IITM-ESM',
               'HadGEM3-GC31-MM',
               'HadGEM3-GC31-LL',
               'GISS-E2-1-G',
               'GFDL-ESM4',
               'GFDL-CM4_gr2',
               'GFDL-CM4',
               'FGOALS-g3',
               'EC-Earth3-Veg-LR',
               'EC-Earth3',
               'CanESM5',
               'CNRM-ESM2-1',
               'CNRM-CM6-1',
               'CMCC-ESM2',
               'CMCC-CM2-SR5',
               'CESM2-WACCM',
               'CESM2',
               'BCC-CSM2-MR',
               'ACCESS-ESM1-5',
               'ACCESS-CM2')
  }

  # Download data for the required  years
  nmodels <- length(model)
  nyrs    <- length(years)
  for(i in 1:nmodels){
    for(j in 1:nyrs){

      # Show message
      cat('\f')
      message(paste0('Downloading ',model[i],' (',i,'/',nmodels,')',' for ',years[j],' (',j,'/',nyrs,')'))

      # Assign auxiliary variables
      mod   <- model[i]
      yr    <- years[j]
      per   <- scenario
      var   <- variable

      # Show error message
      if((yr>2014 & per=='historical')==TRUE | (yr<=2014 & per!='historical')==TRUE){
        message('ERROR: There is no more data to download from this scenario')
      }else{
        # Create new directories
        dir.create(file.path(location,var))
        dir.create(file.path(location,var,per))
        dir.create(file.path(location,var,per,mod))

        # Select runs for each model
        if(mod %in% c('TaiESM1',
                      'NorESM2-MM',
                      'NorESM2-LM',
                      'NESM3',
                      'MRI-ESM2-0',
                      'MPI-ESM1-2-LR',
                      'MPI-ESM1-2-HR',
                      'MIROC6',
                      'KIOST-ESM',
                      'KACE-1-0-G',
                      'IPSL-CM6A-LR',
                      'INM-CM5-0',
                      'INM-CM4-8',
                      'IITM-ESM',
                      'GFDL-ESM4',
                      'GFDL-CM4_gr2',
                      'GFDL-CM4',
                      'EC-Earth3-Veg-LR',
                      'EC-Earth3',
                      'CanESM5',
                      'CMCC-CM2-SR5',
                      'CMCC-ESM2',
                      'CMCC-CM2-SR5',
                      'BCC-CSM2-MR',
                      'ACCESS-ESM1-5',
                      'ACCESS-CM2')){
          run <- 'r1i1p1f1'
        }
        if(mod %in% c('UKESM1-0-LL',
                      'MIROC-ES2L',
                      'GISS-E2-1-G',
                      'CNRM-ESM2-1',
                      'CNRM-CM6-1')){
          run <- 'r1i1p1f2'
        }
        if(mod %in% c('HadGEM3-GC31-MM',
                      'HadGEM3-GC31-LL')){
          run <- 'r1i1p1f3'
        }
        if(mod %in% c('FGOALS-g3',
                      'CESM2-WACCM')){
          run <- 'r3i1p1f1'
        }
        if(mod %in% c('CESM2')){
          run <- 'r4i1p1f1'
        }

        # Fix URLs from "https://ds.nccs.nasa.gov/thredds/catalog/AMES/NEX/GDDP-CMIP6/catalog.html"
        filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gn_', yr,'.nc')
        folder   <- paste0(mod,'/',per,'/',run,'/',var,'/')
        url      <- paste0('https://ds.nccs.nasa.gov/thredds/fileServer/AMES/NEX/GDDP-CMIP6/',
                           folder, filename)

        # Download and subsetting data
        if(is.null(roi)==FALSE){
          # Downloading raw data
          tempfile <- file.path(location,var,per,mod,'temp_file.nc')
          if(is.null(mode)==TRUE){
            switch <- try(download.file(url=url, destfile=tempfile, method=method))
          }else{
            switch <- try(download.file(url=url, destfile=tempfile, method=method, mode=mode))
          }

          # Reading data
          xmin    <- roi[1]
          xmax    <- roi[2]
          ymin    <- roi[3]
          ymax    <- roi[4]
          if(class(switch)!='try-error'){
            nc   <- nc_open(tempfile)
            if(ymin<0){
              lat      <- ncvar_get(nc, 'lat')
              lat_name <- "degrees_south"
            }else{
              lat      <- ncvar_get(nc, 'lat')
              lat_name <- "degrees_north"
            }
            if(xmax<0 & xmin<0){
              lon    <- ncvar_get(nc, 'lon')-360
              lon_name <- "degrees_west"
            }else{
              lon  <- ncvar_get(nc, 'lon')
              lon_name <- "degrees_east"
            }
            if(var=='pr'){
              dat      <- ncvar_get(nc, var)*86400
              units    <- 'mm/d'
              longname <- 'Precipitation'
              missval  <- 1e+20
            }
            if(var=='tas'){
              dat      <- ncvar_get(nc, var)-273
              units    <- 'Degrees Celsius'
              longname <- 'Daily Near-Surface Air Temperature'
              missval  <- 1e+20
            }
            if(var=='tasmin'){
              dat      <- ncvar_get(nc, var)-273
              units    <- 'Degrees Celsius'
              longname <- 'Daily Minimum Near-Surface Air Temperature'
              missval  <- 1e+20
            }
            if(var=='tasmin'){
              dat      <- ncvar_get(nc, var)-273
              units    <- 'Degrees Celsius'
              longname <- 'Daily Maximum Near-Surface Air Temperature'
              missval  <- 1e+20
            }
            time <- ncvar_get(nc, 'time')

            # Subsetting data
            lon_sub <- subset(lon, lon>=xmin & lon<=xmax)
            lat_sub <- rev(subset(lat, lat>=ymin & lat<=ymax))
            dat_sub <- dat[match(lon_sub, lon), match(lat_sub, lat),]

            # Create a new file
            londim    <- ncdim_def(name="lon",
                                   units=lon_name,
                                   vals=as.double(lon_sub))
            latdim    <- ncdim_def(name="lat",
                                   units="degrees_south",
                                   vals=as.double(lat_sub))
            timedim   <- ncdim_def(name="time",
                                   units='days since 1850-01-01',
                                   vals=as.double(time),
                                   calendar='360_day')
            vardef    <- ncvar_def(name=var,
                                   units=units,
                                   dim=list(londim,latdim,timedim),
                                   missval=missval,
                                   longname=longname,
                                   prec="float")
            destfile <- file.path(location,var,per,mod,filename)
            ncnew    <- nc_create(filename=destfile,
                                  vars=list(vardef),
                                  force_v4=TRUE)
            ncvar_put(nc=ncnew,
                      varid=vardef,
                      vals=dat_sub)
            nc_close(nc)
            nc_close(ncnew)
            unlink(tempfile)
            gc()
          }
        }else{
          # Download original raw data
          destfile <- file.path('.',var,per,mod,filename)
          if(is.null(mode)==TRUE){
            switch   <- try(download.file(url=url, destfile=destfile))
            gc()
          }else{
            switch   <- try(download.file(url=url, destfile=destfile, method=method))
            gc()
          }
        }
      }
    }
  }
message('Done!')
toc()
}
