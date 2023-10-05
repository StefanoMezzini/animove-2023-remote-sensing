#' see the message when attaching `raster`
library('raster')    # only used to import raster time series (raster brick)
library('readr')     # for importing csv data 
library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('sf')        # for shapefiles
library('lubridate') # for working with dates (conflicts with terra)
library('terra')     # for rasters
library('ggplot2')   # for fancy plots
theme_set(theme_bw())

#' in this script, we use `terra::extract()`. If you need to extract large 
#' amounts of data from raster stacks, see `exactextractr::exact_extract()`:
#' https://tmieno2.github.io/R-as-GIS-for-Economists/extract-speed.html

# BC Albers projection
bc_albers <- 'EPSG:3005'

# fires with 2013 health authority population data (no spatial cropping)
fires <- read_csv('data/fires-and-population.csv',
                  col_types = cols(fire_id = 'c', data_source = 'c',
                                   ha_name = 'c', fire_date = 'D',
                                   .default = 'n')) %>%
  filter(year %in% 2012:2014) # since we have 2013 population estimates

# interior HA
int <-
  read_sf('data/BC-Government-data/bc-health-authorities/HLTH_ATH_B_polygon.shp') %>%
  filter(HA_NAME == 'Interior') %>%
  st_geometry() %>%
  st_as_sf() %>%
  st_transform('+proj=longlat')

# fire polygons
polys <-
  read_sf('data/BC-Government-data/fire-perimeters/H_FIRE_PLY_polygon.shp') %>%
  rename_with(tolower) %>% # make all colnames lower case 
  filter(fire_year %in% c(2012:2014)) %>%
  st_transform('+proj=longlat') %>%
  st_intersection(int) %>%
  st_geometry() %>%
  st_as_sf()

# NDVI rasters
rast_ts <- list.files('data/modis-ndvi-rasters/VI_16Days_1Km_v61/NDVI',
                      pattern = '.\\.tif', # ends with ".tif"
                      full.names = TRUE) %>%
  rast() %>%
  crop(int) %>% # use interior only for faster examples
  mask(int)

plot(rast_ts[[1]])
plot(int, add = TRUE, col = 'transparent', lwd = 2)
plot(polys, bgc = 'red3', border = 'red3', add = TRUE)
points(lat ~ long, fires)

#' *NOTE:* projecting raster stacks can take a long time, and it changes the
#' rasters because it has to estimate values between points.. Thus, use
#' unprojected rasters for analyses (if not calculating angles or
#' distances), and project the final maps, instead.

# estimated NDVI at ubco arts building (point = spatVect) ----
arts <- vect(data.frame(long = -119.396724, lat = 49.939362),
             geom = c('long', 'lat'),
             crs = '+proj=longlat')
plot(arts)
class(arts)

arts_ts <- extract(rast_ts, arts)
class(arts_ts)
colnames(arts_ts)
View(arts_ts)

# convert to a long tibble
arts_ts <- extract(rast_ts, arts) %>%
  pivot_longer(cols = - ID, values_to = 'ndvi', names_to = 'date') %>%
  select(-ID) %>% # unnecessary column
  mutate(date = substr(date, nchar('MOD13A2_NDVI_x'), nchar(date)) %>%
           as.Date(format = '%Y_%j'))

arts_ts

# plot the time series
ggplot(arts_ts, aes(date, ndvi)) +
  geom_point()

# kelowna and west kelowna bounding box (approximate) ----
kelowna <- matrix(c(-119.693257, 49.950877,
                    -119.693257, 49.783976,
                    -119.292061, 49.783976,
                    -119.292061, 49.950877),
                  ncol = 2, byrow = TRUE) %>%
  as.data.frame() %>%
  sfheaders::sf_polygon(x = 'V1', y = 'V2') %>%
  st_set_crs('+proj=longlat')

plot(kelowna)
class(kelowna)

rast_ts %>%
  crop(kelowna) %>%
  mask(kelowna) %>%
  plot()

# re-projecting rasters ----
layout(matrix(1:4, ncol = 2))

# unprojected, lat-long (areas and angles are distorted)
rast_ts[[9]] %>%
  crop(kelowna) %>%
  mask(kelowna) %>%
  plot(main = 'Unprojected')
plot(arts, add = TRUE)

# BC Albers projection (preserves areas, distorts angles, used by BC gov)
rast_ts[[9]] %>%
  crop(kelowna) %>%
  mask(kelowna) %>%
  project(bc_albers) %>%
  plot(main = 'BC Albers')

# sinusoidal projection (preserves areas, distorts angles)
rast_ts[[9]] %>%
  crop(kelowna) %>%
  mask(kelowna) %>%
  project('ESRI:54008') %>%
  plot(main = 'Sinusoidal')

# Universal Transverse Mercator projection (preserves angles, not areas)
rast_ts[[9]] %>%
  crop(kelowna) %>%
  mask(kelowna) %>%
  project('EPSG:32610') %>%
  plot(main = 'UTM 13')

# methods for interpolating values when reprojecting ----
ndvi_pal <- colorRampPalette(c('darkblue', 'dodgerblue', '#744700', '#d9bb94',
                               'darkgreen'))(100)
layout(matrix(1:6, nrow = 2))

# bilinear interpolation (default for numerical data)
rast_ts[[9]] %>%
  crop(kelowna) %>%
  mask(kelowna) %>%
  project('EPSG:32610', method = 'bilinear') %>%
  plot(main = 'bilinear', range = c(-1, 1), col = ndvi_pal)

# cubic polynomial interpolation
rast_ts[[9]] %>%
  crop(kelowna) %>%
  mask(kelowna) %>%
  project('EPSG:32610', method = 'cubic') %>%
  plot(main = 'cubic', range = c(-1, 1), col = ndvi_pal)

# cubic spline interpolation
rast_ts[[9]] %>%
  crop(kelowna) %>%
  mask(kelowna) %>%
  project('EPSG:32610', method = 'cubicspline') %>%
  plot(main = 'cubic spline', range = c(-1, 1), col = ndvi_pal)

# weighted sum of all non-NA contributing cells
rast_ts[[9]] %>%
  crop(kelowna) %>%
  mask(kelowna) %>%
  project('EPSG:32610', method = 'sum') %>%
  plot(main = 'sum', range = c(-1, 1), col = ndvi_pal)

# min within each cell
rast_ts[[9]] %>%
  crop(kelowna) %>%
  mask(kelowna) %>%
  project('EPSG:32610', method = 'min') %>%
  plot(main = 'min', range = c(-1, 1), col = ndvi_pal)

# median
rast_ts[[9]] %>%
  crop(kelowna) %>%
  mask(kelowna) %>%
  project('EPSG:32610', method = 'med') %>%
  plot(main = 'median', range = c(-1, 1), col = ndvi_pal)

layout(1)

# extracting NDVI for fires ----
# using coordinates of fire centers
extract(rast_ts, select(fires, long, lat)) %>%
  pivot_longer(-ID, names_to = 'date', values_to = 'ndvi',
               values_drop_na = TRUE) %>% # drop fires outside interior HA
  select(-ID) %>% # unnecessary column
  mutate(date = substr(date, nchar('MOD13A2_NDVI_x'), nchar(date)) %>%
           as.Date(format = '%Y_%j'))

# using fire polygons
extract(rast_ts, polys) %>%
  pivot_longer(-ID, names_to = 'date', values_to = 'ndvi') %>%
  select(-ID) %>% # unnecessary column
  mutate(date = substr(date, nchar('MOD13A2_NDVI_x'), nchar(date)) %>%
           as.Date(format = '%Y_%j'))

# but NDVI is only available at sampling dates

# next script: estimating NDVI on other days for kelowna
rast_ts %>%
  crop(kelowna) %>%
  writeRaster('data/modis-ndvi-rasters/kelowna-ndvi.tif', overwrite = TRUE)
