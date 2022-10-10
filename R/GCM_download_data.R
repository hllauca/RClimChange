#' Download daily GCM data from NCCS THREDDS NEX-GDDP-CMIP6.
#' @param location Work directory to store downloaded data.
#' @param model Model names to download. If NULL, all available models will be selected.
#' @param scenario Choose the scenario to be downloaded ('historical','ssp126','ssp245', 'ssp370',or 'ssp585). Some models could haven't all scenarios.
#' @param variable Choose the variable to be downloaded ('hurs','huss','pr','rlds','rsds','sfcWind','tas','tasmax', or 'tasmin').
#' @param years Choose data years to be downloaded  (1950:2014 for 'historical' and 2015:2100 for 'ssp126', 'ssp245', 'ssp370' and 'ssp585').
#' @param roi Vector of coordinates for subsetting data (xmin, xmax, ymin, ymax). If NULL, original extension data will be downloaded.
#' @param method Method to be used for downloading files. Current download methods are 'internal', 'wininet' (Windows only), 'libcurl', 'wget' and 'curl'. The 'curl' method is recommended for Windows users.
#' @return CMIP6 daily data (in netCDF format).
#' @export
#' @examples
#' # Load package
#' require(RClimChange)
#'
#' # Download daily precipitation (in mm/d) from the BCC-CSM2-MR model; for 1990 and the Peruvian domain
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
                              method='curl'){


  tic()

  # Config timeout for big files
  options(timeout = max(300, getOption("timeout")))

  # Available models
  gcm   <- c('UKESM1-0-LL',
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

  # If model is NULL
  if(is.null(model)==TRUE){
     model <- gcm
  }

  # Download data for the required years
  nmodels <- length(model)
  nyrs    <- length(years)

  # Loop by models
  for(i in 1:nmodels){
    # Loop by years
    for(j in 1:nyrs){

      # Assign auxiliary variables
      mod   <- model[i]
      yr    <- years[j]
      per   <- scenario
      var   <- variable

      # Conditional by scenario names
      if(per %in% c('historical','ssp126','ssp245','ssp370','ssp585')){
        # Conditional by model names
        if(mod %in% gcm){
          # Conditional by variable names
          if(var %in% c('hurs','huss','pr','rlds','rsds','sfcWind','tas','tasmax','tasmin')){
            # Conditional by available years
            if((yr<=2014 & per=='historical')==TRUE | (yr>2014 & per!='historical')==TRUE){

              # Show message
              cat('\f')
              message(paste0('Downloading ',var,' from ',model[i],' (',i,'/',nmodels,')',' for ',years[j],' (',j,'/',nyrs,')'))

              # Create new directories
              dir.create(file.path(location,var), showWarnings=FALSE)
              dir.create(file.path(location,var,per), showWarnings=FALSE)
              dir.create(file.path(location,var,per,mod), showWarnings=FALSE)

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
              if(mod %in% c('KACE-1-0-G',
                            'IPSL-CM6A-LR',
                            'EC-Earth3-Veg-LR',
                            'EC-Earth3',
                            'CNRM-ESM2-1',
                            'CNRM-CM6-1')){
                filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gr_', yr,'.nc')
              }
              if(mod %in% c('KIOST-ESM',
                            'INM-CM5-0',
                            'INM-CM4-8',
                            'GFDL-ESM4',
                            'GFDL-CM4')){
                filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gr1_', yr,'.nc')
              }
              if(mod %in% c('GFDL-CM4_gr2')){
                filename <- paste0(var,'_day_GFDL-CM4_',per,'_',run,'_gr2_', yr,'.nc')
              }
              if(mod %in% c('UKESM1-0-LL',
                            'TaiESM1',
                            'NorESM2-MM',
                            'NorESM2-LM',
                            'NESM3',
                            'MRI-ESM2-0',
                            'MPI-ESM1-2-LR',
                            'MPI-ESM1-2-HR',
                            'MIROC6',
                            'MIROC-ES2L',
                            'IITM-ESM',
                            'HadGEM3-GC31-MM',
                            'HadGEM3-GC31-LL',
                            'GISS-E2-1-G',
                            'FGOALS-g3',
                            'CanESM5',
                            'CMCC-ESM2',
                            'CMCC-CM2-SR5',
                            'CESM2-WACCM',
                            'CESM2',
                            'BCC-CSM2-MR',
                            'ACCESS-ESM1-5',
                            'ACCESS-CM2')){
                filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gn_', yr,'.nc')
              }
              folder   <- paste0(mod,'/',per,'/',run,'/',var,'/')
              url      <- paste0('https://nex-gddp-cmip6.s3.us-west-2.amazonaws.com/NEX-GDDP-CMIP6/',
                                 folder, filename)
              # url      <- paste0('https://portal.nccs.nasa.gov/datashare/nexgddp_cmip6/',
              #                    folder, filename)

                # Download and subsetting data
                if(is.null(roi)==FALSE){

                    # Downloading raw data
                    tempfile <- file.path(location,var,per,mod,'temporal_file.nc')
                    download.file(url=url, destfile=tempfile, method=method, cacheOK=FALSE)

                    # Reading data
                    xmin    <- roi[1]
                    xmax    <- roi[2]
                    ymin    <- roi[3]
                    ymax    <- roi[4]

                    # Open netcdf
                    nc   <- nc_open(tempfile)

                    # First dimension
                    if(xmax<0 & xmin<0){
                      lon    <- ncvar_get(nc, 'lon')-360
                      lonunits <- "degrees_west"
                    }else{
                      lon  <- ncvar_get(nc, 'lon')
                      lonunits <- "degrees_east"
                    }

                    # Second dimension
                    if(ymin<0){
                      lat      <- ncvar_get(nc, 'lat')
                      latunits <- "degrees_south"
                    }else{
                      lat      <- ncvar_get(nc, 'lat')
                      latunits <- "degrees_north"
                    }

                    # Third dimension
                    time   <- ncvar_get(nc, 'time')
                    tunits <- ncatt_get(nc,"time","units")$value
                    ntime  <- length(time)
                    if(ntime==364){
                      calendar <- 'standard'
                    }
                    if(ntime==360){
                      calendar <- '360_day'
                    }
                    if(ntime==365){
                      calendar <- '365_day'
                    }
                    if(ntime==366){
                      calendar <- 'standard'
                    }

                    # Read variables
                    if(var=='hurs'){
                      dat      <- ncvar_get(nc, var)
                      units    <- ncatt_get(nc,"hurs","units")$value
                      longname <- ncatt_get(nc,"hurs","long_name")$value
                      missval  <- ncatt_get(nc,"hurs","_FillValue")$value
                    }
                    if(var=='huss'){
                      dat      <- ncvar_get(nc, var)
                      units    <- ncatt_get(nc,"huss","units")$value
                      longname <- ncatt_get(nc,"huss","long_name")$value
                      missval  <- ncatt_get(nc,"huss","_FillValue")$value
                    }
                    if(var=='pr'){
                      dat      <- ncvar_get(nc, var)*86400
                      units    <- 'mm/d'
                      longname <- ncatt_get(nc,"pr","long_name")$value
                      missval  <- ncatt_get(nc,"pr","_FillValue")$value*86400
                    }
                    if(var=='rlds'){
                      dat      <- ncvar_get(nc, var)
                      units    <- ncatt_get(nc,"rlds","units")$value
                      longname <- ncatt_get(nc,"rlds","long_name")$value
                      missval  <- ncatt_get(nc,"rlds","_FillValue")$value
                    }
                    if(var=='rsds'){
                      dat      <- ncvar_get(nc, var)
                      units    <- ncatt_get(nc,"rsds","units")$value
                      longname <- ncatt_get(nc,"rsds","long_name")$value
                      missval  <- ncatt_get(nc,"rsds","_FillValue")$value
                    }
                    if(var=='sfcWind'){
                      dat      <- ncvar_get(nc, var)
                      units    <- ncatt_get(nc,"sfcWind","units")$value
                      longname <- ncatt_get(nc,"sfcWind","long_name")$value
                      missval  <- ncatt_get(nc,"sfcWind","_FillValue")$value
                    }
                    if(var=='tas'){
                      dat      <- ncvar_get(nc, var)-273
                      units    <- 'Degrees Celsius'
                      longname <- ncatt_get(nc,"tas","long_name")$value
                      missval  <- ncatt_get(nc,"tas","_FillValue")$value-273
                    }
                    if(var=='tasmax'){
                      dat      <- ncvar_get(nc, var)-273
                      units    <- 'Degrees Celsius'
                      longname <- ncatt_get(nc,"tasmax","long_name")$value
                      missval  <- ncatt_get(nc,"tasmax","_FillValue")$value-273
                    }
                    if(var=='tasmin'){
                      dat      <- ncvar_get(nc, var)-273
                      units    <- 'Degrees Celsius'
                      longname <- ncatt_get(nc,"tasmin","long_name")$value
                      missval  <- ncatt_get(nc,"tasmin","_FillValue")$value-273
                    }

                    # Subsetting data
                    lon_sub <- subset(lon, lon>=xmin & lon<=xmax)
                    lat_sub <- rev(subset(lat, lat>=ymin & lat<=ymax))
                    dat_sub <- dat[match(lon_sub, lon), match(lat_sub, lat),]

                    # Create a new netcdf file
                    londim    <- ncdim_def(name="lon",
                                           units=lonunits,
                                           vals=as.double(lon_sub))
                    latdim    <- ncdim_def(name="lat",
                                           units=latunits,
                                           vals=as.double(lat_sub))
                    timedim   <- ncdim_def(name="time",
                                           units=tunits,
                                           vals=as.double(time),
                                           calendar=calendar)
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

                }else{
                  # Download original data for the entire globe
                  destfile <- file.path('.',var,per,mod,filename)
                  download.file(url=url, destfile=destfile)
                  gc()
                }
            }else{
              # Show error message
              message('ERROR: There is no more data to download from this scenario')
            }
          }else{
            # Show error message
            message('ERROR: Please write a valid variable name')
          }
        }else{
          # Show error message
          message('ERROR: Please write a valid model name')
        }
      }else{
        # Show error message
        message('ERROR: Please write a valid scenario name')
      }
    } # End years
  } # End models
# SHow message
message('Done!')
toc()
}
