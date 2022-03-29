'RClimChange' package
========================

This is an experimental package with a function for downloading and subsetting daily Global Climate Models (GCM) from NASA Earth Exchange Global Daily Downscaled Projections (NEX-GDDP-CMIP6). Readers can find more data set details in: https://www.nccs.nasa.gov/services/data-collections/land-based-products/nex-gddp-cmip6

Future implementations will consider additional functions to post-process and bias-correct this data set.

Available GCMs
- UKESM1-0-LL
- TaiESM1
- NorESM2-MM
- NorESM2-LM
- NESM3
- MRI-ESM2-0
- MPI-ESM1-2-LR
- MPI-ESM1-2-HR
- MIROC6
- MIROC-ES2L
- KIOST-ESM
- KACE-1-0-G
- IPSL-CM6A-LR
- INM-CM5-0
- INM-CM4-8
- IITM-ESM
- HadGEM3-GC31-MM
- HadGEM3-GC31-LL
- GISS-E2-1-G
- GFDL-ESM4
- GFDL-CM4_gr2
- GFDL-CM4
- FGOALS-g3
- EC-Earth3-Veg-LR
- EC-Earth3
- CanESM5
- CNRM-ESM2-1
- CNRM-CM6-1
- CMCC-ESM2
- CMCC-CM2-SR5
- CESM2-WACCM
- CESM2
- BCC-CSM2-MR
- ACCESS-ESM1-5
- ACCESS-CM2

Available variables
- hurs: Near-Surface Relative Humidity in [%].
- huss: Near-Surface Relative Humidity in [kg/kg].
- pr: Precipitation (mean of the daily precipitation rate) in [kg m-2 s-1] or [mm/d].
- rlds: Surface Downwelling Longwave Radiation in [W m-2].
- rsds: Surface Downwelling Shortwave Radiation in [W m-2].
- sfcWind: Daily-Mean Near-Surface Wind Speed in [m s-1]
- tas: Daily Near-Surface Air Temperature in [K] or [ºC].
- tasmax: Daily Maximum Near-Surface Air Temperature in [K] or [ºC].
- tasmin: Daily Minimum Near-Surface Air Temperature in [K] or [ºC].

Available scenarios
- ssp126
- ssp245
- ssp370
- ssp585

Summary
=======
- Short Name: NEX-GDDP-CMIP6
- Version: 1
- Format: netCDF4
- West Bounding Coordinate: 180° W
- East Bounding Coordinate: 180° E
- North Bounding Coordinate: 90° N
- South Bounding Coordinate: 60° S
- Latitude Resolution: 0.25 degrees (25 km)
- Longitude Resolution: 0.25 degrees (25 km)
- Temporal Resolution: daily
- Total Dataset Size: 18 TB (without subsetting)


Instructions
============
Please take a look at the following instructions.

1. install.packages("devtools")

2. devtools::install_github("hllauca/RClimChange")

3. library(RClimChange)


Contact
========
Harold LLauca (hllauca@senamhi.gob.pe)
