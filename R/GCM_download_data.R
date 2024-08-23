#' Download daily GCM data from NCCS THREDDS NEX-GDDP-CMIP6 (https://www.nccs.nasa.gov/services/data-collections/land-based-products/nex-gddp-cmip6)
#' @param location Select the location for data downloading.
#' @param model Model name. If NULL, all available models will be selected.
#' @param scenario CMIP6 scenario ('historical','ssp126','ssp245', 'ssp370', or 'ssp585).
#' @param variable Variable ('hurs','huss','pr','rlds','rsds','sfcWind','tas','tasmax', or 'tasmin').
#' @param years Data years (1950 - 2014 for 'historical' and 2015 - 2100 for 'ssp126', 'ssp245', 'ssp370' and 'ssp585').
#' @param roi Extension for sub-setting gridded data (west, east, south, north). If NULL, original extension is considered.
#' @param version Select file version. Use NULL for first version, or 'v1.1' and 'v1.2'. Please read technical notes in NEX website.
#' @param method Method to be used for downloading files. Current download methods are 'internal', 'wininet' (Windows only), 'libcurl', 'wget' and 'curl'. The 'curl' method is recommended for Windows users.
#' @return CMIP6 daily data in netCDF format.
#' @export
#' @examples
#' # Load package
#' require(RClimChange)
#'
#' # Download daily precipitation flux from BCC-CSM2-MR model
#' # historical period
#' nex_download(location=getwd(),
#'              model='BCC-CSM2-MR',
#'              scenario='historical',
#'              variable='pr',
#'              years=1990:1992,
#'              version='v1.1',
#'              roi=c(-86,-66,-20,2),
#'              method='curl')
#'
#' # ssp585 scenario
#' nex_download(location=getwd(),
#'              model='BCC-CSM2-MR',
#'              scenario='ssp585',
#'              variable='pr',
#'              years=2050:2052,
#'              version='v1.1',
#'              roi=c(-86,-66,-20,2),
#'              method='curl')
#'
#' @import  ncdf4
#' @import  RCurl
#' @import  tictoc

nex_download <- function(location,
                         model=NULL,
                         scenario,
                         variable,
                         years,
                         roi,
                         version=NULL,
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
  if(is.null(model)){
     model <- gcm
  }

  # Download data for the required years
  nmodels <- length(model)
  nyrs    <- length(years)

  # Check subsetting area
  west  <- roi[1]
  east  <- roi[2]
  south <- roi[3]
  north <- roi[4]

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

              # Create filename
              if(mod %in% c('KACE-1-0-G',
                            'IPSL-CM6A-LR',
                            'EC-Earth3-Veg-LR',
                            'EC-Earth3',
                            'CNRM-ESM2-1',
                            'CNRM-CM6-1')){
                if(is.null(version)==TRUE){
                  filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gr_', yr,'.nc')
                }else{
                  if(version=='v1.1'){
                    filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gr_', yr,'_v1.1.nc')
                  }else{
                    if(version=='v1.2'){
                      filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gr_', yr,'_v1.2.nc')
                    }
                  }
                }
              }
              if(mod %in% c('KIOST-ESM',
                            'INM-CM5-0',
                            'INM-CM4-8',
                            'GFDL-ESM4',
                            'GFDL-CM4')){
                if(is.null(version)==TRUE){
                  filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gr1_', yr,'.nc')
                }else{
                  if(version=='v1.1'){
                    filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gr1_', yr,'_v1.1.nc')
                  }else{
                    if(version=='v1.2'){
                     filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gr1_', yr,'_v1.2.nc')
                    }
                  }
                }
              }
              if(mod %in% c('GFDL-CM4_gr2')){

                if(is.null(version)==TRUE){
                  filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gr2_', yr,'.nc')
                }else{
                  if(version=='v1.1'){
                    filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gr2_', yr,'_v1.1.nc')
                  }else{
                    if(version=='v1.2'){
                      filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gr2_', yr,'_v1.2.nc')
                    }
                  }
                }
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
                if(is.null(version)==TRUE){
                  filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gn_', yr,'.nc')
                }else{
                  if(version=='v1.1'){
                    filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gn_', yr,'_v1.1.nc')
                  }else{
                    if(version=='v1.2'){
                      filename <- paste0(var,'_day_',mod,'_',per,'_',run,'_gn_', yr,'_v1.2.nc')
                    }
                  }
                }
              }

              # Create URL
              folder   <- paste0(mod,'/',per,'/',run,'/',var,'/')
              if(is.null(roi)){
                url      <- paste0('https://nex-gddp-cmip6.s3.us-west-2.amazonaws.com/NEX-GDDP-CMIP6/', folder, filename)
              }else{
                url_base   <- 'https://ds.nccs.nasa.gov/thredds/ncss/grid/AMES/NEX/GDDP-CMIP6'
                cut_region <- paste0('?var=',var,'&north=',north,'&west=',west,'&east=',east,'&south=',south,'&horizStride=1&time_start=',yr,'-01-01T12:00:00Z&time_end=',yr,'-12-31T12:00:00Z&&&accept=netcdf3&addLatLon=true')
                url        <- paste0(url_base,'/',folder,filename,cut_region)
              }

              # Download netcdf file
              destfile <- file.path(location,var,per,mod,filename)
              download.file(url=url, destfile=destfile, method=method, cacheOK=FALSE)
              gc()

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
# Show message
message('Done!')
toc()
}
