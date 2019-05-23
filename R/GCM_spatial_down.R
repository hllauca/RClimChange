#' Extract a downscaled GCM data for a point of interest
#'
#' @param Input A 5D array (lon,lat,day,year,model) from GCM_extract_region output
#' @param RegLon Vector of longitudes for the study area
#' @param RegLat Vector of latitudes for the study area
#' @param XYcoords Vector with station coordinates (lon,lat)
#' @param YearIni Initial year to compute leap years
#' @return GCM time series for a point of interest
#' @export
GCM_spatial_down <- function(Input, RegLon, RegLat, XYcoords, YearIni){


    # Create an empty matrix to store data [day x year x model]
    dta.gcm <- array(NA, dim=c(365, dim(Input)[4], dim(Input)[5]))


    # Read files for each model
    for (m in 1:dim(Input)[5]){

      # Create an empty matrix to store data [day x year]
      dta.gcm.ex <- matrix(NA, nrow=365, ncol=dim(Input)[4])

        # Read each model files
        for (w in 1:dim(Input)[4]){

          # Input data
          dta <- Input[,,,w,m]

          # Read station coordinates
          lon.est <- XYcoords[1]
          lat.est <- XYcoords[2]

          # Calculate lat/lon differences
          dlat <- abs(RegLat - lat.est)
          dlon <- abs(RegLon - lon.est)

          # Subset lat/lon cells
          min.lat1 <- which(dlat==min(dlat))
          min.lat2 <- which(dlat==min(dlat[dlat!=min(dlat)]))
          min.lon1 <- which(dlon==min(dlon))
          min.lon2 <- which(dlon==min(dlon[dlon!=min(dlon)]))

          # Calculate lat/lon distances
          dist.lat <- dlat[c(min.lat1, min.lat2, min.lat2, min.lat1)]
          dist.lon <- dlon[c(min.lon1, min.lon1, min.lon2, min.lon2)]

          # Extract the 4 closest points
          A <- dta[min.lon1, min.lat1, ]
          B <- dta[min.lon1, min.lat2, ]
          C <- dta[min.lon2, min.lat2, ]
          D <- dta[min.lon2, min.lat1, ]

          # Apply an IDW interpolation
          dist     <- (dist.lat^2 + dist.lon^2)^0.5
          dist.inv <- 1/dist^2
          dta.est  <- (A*dist.inv[1] + B*dist.inv[2]+
                       C*dist.inv[3] + D*dist.inv[4])/sum(dist.inv)

          # Fix leap years
          time     <- dta[1,1,]
          no.years <- length(time[!is.na(time)])

          if (no.years==366){
              dta.gcm.ex[,w] <- dta.est[-60]
          } else {
              dta.gcm.ex[,w] <- dta.est[-366]
          }
        }

        # Store data [day x year x model]
        dta.gcm[c(1:dim(dta.gcm.ex)[1]),c(1:dim(dta.gcm.ex)[2]),m] <- dta.gcm.ex
      }

    # Save data in a [time x model] matrix
    dta.gcm.est <- matrix(NA, nrow=365*dim(Input)[4], ncol=dim(Input)[5])
    for (ww in 1:dim(Input)[5]){
      dta.gcm.vec <- na.omit(as.vector(dta.gcm[,,ww]))
      dta.gcm.est[c(1:length(dta.gcm.vec)),ww] <- round(dta.gcm.vec,2)
    }


    # Create dates
    dates <- seq(as.Date(paste(YearIni,'1/1', sep='/')), as.Date(paste(YearIni+dim(Input)[4]-1,'12/31', sep='/')), by='day')


    # Remove leaps
    dates <- dates[format(dates, "%m %d") != "02 29"]

    # Output variables
    Yreturn <- list(est=dta.gcm.est, dates=dates)
    return(Yreturn)
}
