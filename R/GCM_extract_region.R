#' Read subset and built a 5D array with netCDF GCM's files
#'
#' @param Path Directory where netCDF files are located
#' @param Region Lat/lon limits for subseting data (MinLon, MaxLon, MinLat, MaxLat)
#' @param Var Type of hidrometeorological variable to process GCM (Precipitation:'Pp', Maximum Temperature:'Tmax or Minimum Temperature:'Tmin'). 'Pp' as default
#' @return 5D array [lon,lat,day,year,model] with GCM subset data
#' @export
GCM_extract_region <- function(Path, Region, Var='Pp'){


    # Load  packages
    if("ncdf4" %in% rownames(installed.packages()) == FALSE){
      install.packages("ncdf4")
    }
    library(ncdf4)

    if("stringr" %in% rownames(installed.packages()) == FALSE){
      install.packages("stringr")
    }
    library(stringr)

    if("tictoc" %in% rownames(installed.packages()) == FALSE){
      install.packages("ticotc")
    }
    library(tictoc)
    tic()


    # Read filenames
    files.all <- list.files(Path, pattern='nc$')


    # Extract model's names
    names <- vector()
    for (k in 1:length(files.all)){
      y <- strsplit(files.all, '[_]')
      y <- strsplit(y[[k]], '[ ]')
      names[k] <- y[[6]]
    }
    model <- unique(names)


    # Extract initial years
    y <- strsplit(files.all[1], '[_]')
    y <- strsplit(y[[1]], '[ ]')
    y.ini <- as.numeric(str_extract(y[[7]], "...."))


    # Assign variables to numbers
    if (Var=='Pp')  {id.var <- 1}
    if (Var=='Tmax'){id.var <- 2}
    if (Var=='Tmin'){id.var <- 3}
    var.type <-c('pr', 'tasmax','tasmin')
    location <-c('lon', 'lat')
    

    # Extract array dimension
    #=========================
    # Extract latitudes and longitudes from a referential netCDF file
    ncin  <- nc_open(paste(Path, files.all[1], sep='/'))
    lon   <- ncvar_get(ncin, location[1])-360
    lat   <- ncvar_get(ncin, location[2])


    # Set extension of the study area
    min.lon <- Region[1]
    max.lon <- Region[2]
    min.lat <- Region[3]
    max.lat <- Region[4]


    # Subset lat and lon for the study area
    lon.Region <- subset(lon, (lon> min.lon) & (lon< max.lon))
    lat.Region <- subset(lat, (lat> min.lat) & (lat< max.lat))


    # Close netCDF
    nc_close(ncin)


    # Calculate no.years
    no.years  <- length(files.all[str_detect(files.all, paste(model[1],'_',sep=''))])


    # Create an empty 5D array [lon,lat,day,year,model]
    dta.reg <- array(NA, dim=c(length(lon.region), length(lat.region), 366, no.years, length(model)))


    # Read all netCDF files
    #======================
    # Read each GCM model
    for (m in 1:length(model)){
       files.model    <- files.all[str_detect(files.all, paste(model[m],'_',sep=''))]
       no.files.model <- length(files.model)

      # Read files for each GCM model
        for (w in 1:no.files.model){

          ## Show message
          count.all   <- round(((m)/length(model)*100),2)
          count.model <- round((w/no.files.model)*100,2)
          cat('\f')
          print(paste('Total de modelos:...', length(model), sep=''))
          print(paste('Modelos procesados:...', count.all,'%',sep=''))
          print(paste('Procesando modelo Nº-',m,' ',model[m],':...', count.model,'%',sep=''))

          # Open netCDF file
          ncin <- nc_open(paste(Path, files.model[w],sep='/'))

          # Read netCDF data
          lon.gcm <- ncvar_get(ncin, location[1])-360
          lat.gcm <- ncvar_get(ncin, location[2])
          lon.reg <- subset(lon.gcm, (lon.gcm> min.lon) & (lon.gcm< max.lon))
          lat.reg <- subset(lat.gcm, (lat.gcm> min.lat) & (lat.gcm< max.lat))

          # Identify variable
          if (var=='Pp'){
            dta <- ncvar_get(ncin, var.type[id.var])*86400
          } else {
            dta <- ncvar_get(ncin, var.type[id.var])-273
          }

          # Subset data for the study area [lat x lon x day x year x model]
            dta     <- dta[match(lon.reg,lon.gcm), match(lat.reg,lat.gcm),]
            dta.reg[c(1:dim(dta)[1]),c(1:dim(dta)[2]),c(1:dim(dta)[3]),w,m] <- dta
        }
      }


  # Output variables
   Yreturn <- list(data=dta.reg, lat=lat.region, lon=lon.region, model=model, yini=y.ini, var=var, per=period)
   return(Yreturn)


   # Show computing time
   toc()
   alarm()
}
