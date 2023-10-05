library('mgcv')      # for Generalized Additive Models (GAMs)
library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('sf')        # for shapefiles
library('lubridate') # for working with dates (conflicts with terra)
library('terra')     # for rasters
library('elevatr')   # to add elevation; version 1.0.0.9999 from GitHub
library('ggplot2')   # for fancy plots
theme_set(theme_bw())

# import NDVI rasters
rast_ts <- rast('data/modis-ndvi-rasters/kelowna-ndvi.tif')

# download Digital Elevation Model (DEM)
# using a high resolution to avoid artifacts during reprojection
ZOOM <- 8
(cos(50 * pi/180) * 2 * pi * 6378137) / (256 * 2^ZOOM) # resolution (m)
dem <- get_elev_raster(rast_ts[[1]], z = ZOOM) %>%
  project(rast_ts[[1]]) # reproject to the NDVI raster resolution

# create a rough shapefile of the Okanagan Lake
ok <-
  get_elev_raster(rast_ts[[1]], z = ZOOM, clip = 'bbox') %>%
  as.data.frame(xy = TRUE) %>%
  rename(elev = 3) %>%
  transmute(x, y, elev = if_else(elev < 350, 1, NA_real_)) %>%
  rast() %>%
  raster::raster() %>%
  raster::rasterToPolygons() %>%
  st_as_sf() %>%
  st_union() %>%
  st_as_sf() %>%
  st_set_crs('+proj=longlat')
plot(ok, col = 'cornflowerblue', axes = TRUE)
st_write(ok, 'data/okanagan-lake.shp', append = FALSE)

d <- rast_ts %>%
  mask(ok, inverse = TRUE) %>%
  as.data.frame(xy = TRUE) %>%
  left_join(as.data.frame(dem, xy = TRUE) %>%
              rename(elev = 3),
            by = c('x', 'y')) %>%
  pivot_longer(cols = ! c(x, y, elev), names_to = 'date', values_to = 'ndvi',
               values_drop_na = TRUE) %>% # drop fires outside interior HA
  rename(long = x, lat = y) %>%
  mutate(date = substr(date, nchar('MOD13A2_NDVI_x'), nchar(date)) %>%
           as.Date(format = '%Y_%j'),
         year = year(date),
         doy = yday(date),
         dec_date = decimal_date(date))

ggplot(d, aes(doy, ndvi, group = paste(long, lat, year))) +
  geom_line(alpha = 0.1)

ctrl <-
  gam.control(nthreads = min(4, parallel::detectCores(logical = FALSE)),
              trace = TRUE)

m <-
  bam(
    (ndvi + 1) / 2 ~
      s(elev, bs = 'tp', k = 5) + # thin plate regression splines (default)
      s(doy, bs = 'cc', k = 25) + # cubic regression splines
      s(long, lat, bs = 'ds', k = 100) + # 2D splines
      ti(elev, doy, k = c(3, 5), bs = c('cr', 'cc')),
    family = betar(link = 'logit'),
    data = d,
    subset = year == 2013,
    method = 'fREML',
    discrete = TRUE,
    knots = list(doy = c(0.5, 366.5)),
    control = ctrl)

plot(m, pages = 1, scheme = 2, scale = 0, too.far = 0.05)
layout(matrix(1:4, ncol = 2))
gam.check(m, type = 'pearson')
layout(1)
summary(m)

saveRDS(m, 'models/kelowna-ndvi-bam.rds')

# predict from the model
newd_rast <-
  expand_grid(long = seq(min(d$long), max(d$long), length.out = 500),
              lat = seq(min(d$lat), max(d$lat), length.out = 200),
              layer = 1) %>% # necessary to create raster
  rast(crs = '+proj=longlat') %>%
  mask(ok, inverse = TRUE)

plot(newd_rast)

newd <-
  get_elev_raster(newd_rast, z = 12) %>%
  project(newd_rast) %>%
  as.data.frame(xy = TRUE) %>%
  rename(elev = 3) %>%
  rename(long = x, lat = y) %>%
  mutate(dates = list(tibble(doy = round(seq(1, 365, length.out = 9)),
                             year = 2013))) %>%
  unnest(dates)

ggplot(newd) +
  facet_wrap(~ doy) +
  geom_raster(aes(long, lat, fill = elev)) +
  scale_fill_distiller('Elevation (m)', palette = 4)

preds <- mutate(newd, ndvi = predict(m, newdata = newd, type = 'response'))

ggplot(preds) +
  facet_wrap(~ doy) +
  geom_raster(aes(long, lat, fill = ndvi)) +
  scale_fill_gradientn('NDVI', limits = c(-1, 1),
                       colors = c('darkblue', 'dodgerblue', '#744700',
                                  '#d9bb94', 'darkgreen')) +
  scale_x_continuous('Longitude', expand = c(0, 0)) +
  scale_y_continuous('Latitude', expand = c(0, 0))

#' *NOTE:* ideally, you should evaluate the accuracy of your model using a
#'         dataset you did not use to fit the model. To do so, you could
#'         keep some rasters aside, but note that common cross-validation
#'         methods (e.g., standard CV and LOOCV) are not valid since the
#'         data is spatiotemporally autocorrelated. Instead, you should
#'         consider a form of block CV, like fitting the model to 2000-2020
#'         data and testing the model with 2021-2023 data. However, select
#'         testing datasets with care and make sure they do not include
#'         large outlier periods.
