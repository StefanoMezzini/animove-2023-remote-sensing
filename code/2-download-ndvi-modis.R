# see https://rspatialdata.github.io/vegetation.html for example code and more info
#' install `rgeoboundaries` with `remotes::install_github('wmgeolab/rgeoboundaries')`
#' install `MODIStsp` with `remotes::install_github('ropensci/MODIStsp')`
#' `MODIStsp` version >= 2.0.7 from github fixes login issue due to 
# library('rgeoboundaries') # for country/world boundaries
library('sf')       # for spatial features
library('dplyr')    # for data wrangling
library('MODIStsp') # for downloading NDVI rasters from MODIS servers
library('terra')    # faster and uses less RAM than the raster package
source('earthdata-login-info.R') # login credentials for EarthData

#' if needed, make an account on `https://urs.earthdata.nasa.gov`
#' and add your credentials to `'earthdata-login-info.R'`

# bc shapefile for downloading data
bc <- filter(canadianmaps::PROV, PRENAME == 'British Columbia')

# check available products and their names
MODIStsp_get_prodnames()

#' *NDVI* is a measure of greenness of a region
#' `NDVI` = (NIR - RED) / (NIR + RED)
#' `min`  = ( 0  - RED) / ( 0  + RED) = -1 # water
#' `max`  = (NIR -  0 ) / (NIR +  0 ) = +1 # high primary productivity
#' `med`  = ( x  -  x ) / ( x  +  x ) =  0 # barren ground

# download NDVI rasters
MODIStsp(
  gui = FALSE, # "do not use the browser GUI, only run in R"
  out_folder = 'data/modis-ndvi-rasters', # location of downloaded files
  selprod = 'Vegetation_Indexes_16Days_1Km (M*D13A2)', # can't specify Terra here
  prod_version = '061', # 2022 raster version with less calibration issues
  bandsel = 'NDVI', # Normalized Difference Vegetation Index layer only
  sensor = 'Terra', # only terrestrial values, ignore main bodies of water
  user = .USERNAME, # your Earthdata username (for urs.earthdata.nasa.gov/home)
  password = .PASSWORD, # your Earthdata password
  start_date = '2012.01.01', # 1st available day is 2001.02.18
  end_date = '2014.12.31', # last day with forward processing is 2023.02.17
  spatmeth = 'bbox', # download data within bounding box of BC
  bbox = st_bbox(bc), # bounding box of BC
  out_projsel = 'User Defined', # use specified projection instead of default
  output_proj = '+proj=longlat', # download unprojected raster
  resampling = 'bilinear', # method for resampling raster if changing projection
  delete_hdf = TRUE, # delete HDF files after download is complete
  scale_val = TRUE, # convert from integers to floats within [-1, 1]
  ts_format = 'R RasterStack', # also save as an R RasterStack in an Rda file
  out_format = 'GTiff', # output format: 'ENVI' (.hdr) or 'GTiff' (.tif)
  n_retries = 3, # number of times to try again if download fails before aborting
  verbose = TRUE, # print processing messages
  parallel = TRUE) # use TRUE for automatic number of cores (max 8), or specify

# check rasters ----
r <- rast('data/modis-ndvi-rasters/VI_16Days_1Km_v61/NDVI/MOD13A2_NDVI_2012_001.tif')
plot(r)
r %>%
  crop(bc) %>% # crop using the extent (no difference in this case)
  mask(bc) %>% # set points outside BC to NA
  plot()

int <- read_sf('data/BC-Government-data/bc-health-authorities/HLTH_ATH_B_polygon.shp') %>%
  filter(HA_NAME == 'Interior') %>%
  st_geometry() %>%
  st_as_sf()

# fails because of different projections!
r %>%
  crop(int) %>%
  # mask(int) %>% # set points outside BC to NA
  plot()

ext(r)
ext(int)
plot(int, axes = TRUE)

int <- st_transform(int, crs = '+proj=longlat') # unproject to lat/long

plot(int, axes = TRUE)
ext(r)
ext(int)

plot(r)
plot(mask(r, int)) # set points outside the HA to NA
plot(crop(r, int)) # crop using the extents of the HA
r %>%
  crop(int) %>% # crop using the extents of the HA
  mask(int) %>% # set points outside the HA to NA
  plot()

#' import the whole raster stack from an `Rda` file
#' *NOTE:* you cannot save `terra` `spatRast` objects with the Rda format
load('data/modis-ndvi-rasters/VI_16Days_1Km_v61/Time_Series/RData/Terra/NDVI/MOD13A2_NDVI_1_2012_353_2014_RData.RData')
class(raster_ts) # from raster package, not terra package
object.size(raster_ts)

#' convert to `spatRast` format
rast_ts <- rast(raster_ts)
class(rast_ts) # from raster package, not terra package
object.size(rast_ts)

#' `terra` uses much less RAM to store rasters than `raster`
as.numeric(object.size(raster_ts) / object.size(rast_ts))

rast_ts %>%
  crop(int) %>%
  mask(int) %>%
  plot()

# import each of the separate files as a single set of rasters
rast_ts <- list.files('data/modis-ndvi-rasters/VI_16Days_1Km_v61/NDVI',
                      pattern = '.\\.tif', # ends with ".tif"
                      full.names = TRUE) %>%
  rast() %>%
  crop(int) %>%
  mask(int)

plot(rast_ts)

#' *NOTE:* `terra` rasters should not be saved with `save()` or `saveRDS()`:
#' `https://github.com/rspatial/terra/issues/549`
#' Save them with `terra::writeRaster()`, instead. 
r <- rast_ts[[1]]
plot(r)
crs(r)

#' using `save()` and `Rda` format fails
save(r, file = 'data/r-test.rda')
load('data/r-test.Rda') # seems to work
class(r) # class remains
plot(r) # but CRS is lost
crs(r)
file.remove('data/r-test.rda')

#' using `saveRDS()` seems to work, but I would avoid it
r <- rast_ts[[1]]
saveRDS(r, file = 'data/r-test.rds')
r <- readRDS('data/r-test.rds')
class(r)
plot(r)
crs(r)
file.remove('data/r-test.rds')

#' use `writeRaster()`, instead:
r <- rast_ts[[1]]
writeRaster(r, 'data/r-test.tif')
plot(rast('data/r-test.tif'))
file.remove('data/r-test.tif')
