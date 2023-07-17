library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('purrr')   # for functional programming (fancy apply() functions)
library('sf')      # for shapefiles
library('terra')   # for rasters
library('mgcv')    # for GAMs
library('ctmm')    # for simulating movement
library('elevatr') # to add elevation; version 1.0.0.9999 from GitHub
library('ggplot2') # for fancy plots
theme_set(theme_bw())

# import okanagan lake raster
ok <- read_sf('data/okanagan-lake.shp')

# import first NDVI rasters
r <- rast('data/modis-ndvi-rasters/kelowna-ndvi.tif')[[1]] %>%
  mask(ok, inverse = TRUE)

# simulate animal track ----
mov_m <- ctmm(tau = c(10 %#% 'days', 1 %#% 'hour'),
              mu = c(-119.35, 49.82),
              sigma = 1e-4)

times <- seq(as.POSIXct('2012-01-01 00:00:00'),
             as.POSIXct('2014-03-25 15:03:47'),
             by = 1 %#% 'days')
head(times)

track <- simulate(mov_m, seed = 1, t = as.numeric(times))
terra::plot(r)
plot(track, add = TRUE, col = 'black')

d <- data.frame(track) %>%
  as_tibble() %>%
  rename(long = x, lat = y) %>%
  mutate(timestamp = times,
         year = lubridate::year(timestamp),
         doy = lubridate::yday(timestamp))

# annotating variables from a temporally static raster: elevation ----
dem <- get_elev_raster(r, z = 12) # keeping z low for a fast download
d <- mutate(d,
            elev = tibble(long, lat) %>%
              extract(dem, .) %>%
              pull(2))
plot(elev ~ timestamp, d)

# annotating variables from a temporally dynamic raster: NDVI ----
# could use closest raster from the stack, but it includes measurement error
# (e.g, clouds) and has large temporal gaps
m_ndvi <- readRDS('models/kelowna-ndvi-bam.rds')
d <- mutate(d, ndvi = predict(m_ndvi, newdata = d, type = 'response'))
plot(ndvi ~ timestamp, d)

# do altitude and NDVI affect the animal's spatial needs?
periods <- mutate(d,
                  season = lubridate::quarter(timestamp), # ~ season
                  individual.local.identifier = paste(year, season)) %>%
  # group data by year and season (quarter)
  nest(dataset = ! c(year, season)) %>%
  mutate(
    #' summarize the raster data (*information loss!*)
    mean_ndvi = map_dbl(dataset, \(.d) mean(.d$ndvi)), # est. average NDVI
    mean_elev = map_dbl(dataset, \(.d) mean(.d$elev)), # est. average elev.
    # create a telemetry for each period
    tel = map(dataset, as.telemetry),
    # create variaograms for each period
    vg = map(tel, \(.tel) ctmm.guess(.tel, interactive = FALSE)),
    # select the best movement model for each period
    ctmm = map2(tel, vg, \(.tel, .vg) ctmm.select(data = .tel, CTMM = .vg,
                                                  trace = TRUE)),
    # estimate the animal's UD
    ud = map2(tel, ctmm, akde),
    # extract the animal's 95% HR with CIs
    hr_95_lwr = map_dbl(ud, \(.ud) summary(.ud)$CI[, 'low']),
    hr_95_est = map_dbl(ud, \(.ud) summary(.ud)$CI[, 'est']),
    hr_95_upr = map_dbl(ud, \(.ud) summary(.ud)$CI[, 'high']))

#' plot the results
ggplot(periods) +
  geom_point(aes(mean_ndvi, hr_95_est)) +
  geom_errorbar(aes(mean_ndvi, ymin = hr_95_lwr, ymax = hr_95_upr),
                width = 0) +
  geom_smooth(aes(mean_ndvi, hr_95_est), method = 'glm', formula = y ~ x,
              method.args = list(family = Gamma(link = 'log')))

ggplot(periods) +
  geom_point(aes(mean_elev, hr_95_est)) +
  geom_errorbar(aes(mean_elev, ymin = hr_95_lwr, ymax = hr_95_upr),
                width = 0) +
  geom_smooth(aes(mean_elev, hr_95_est), method = 'glm', formula = y ~ x,
              method.args = list(family = Gamma(link = 'log')))
