library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('sf')        # for spatial features
library('ggplot2')   # for fancy plots
library('terra')     # for rasters
theme_set(cowplot::theme_map())

# bc shapefile
bc <- filter(canadianmaps::PROV, PRENAME == 'British Columbia') %>%
  st_geometry() %>%
  st_as_sf()

# ubco coordinates
ubco <- data.frame(x = -119.39466, y = 49.93949) %>%
  vect(geom = c('x', 'y')) %>%
  set.crs('+proj=longlat')

# azimuthal equidistant projection from UBCO
proj_0 <- '+proj=aeqd +lon_0=-119.39466 +lat_0=49.93949 +datum=WGS84'
plot(st_transform(bc, proj_0))

# base raster
r_0 <- vect(bc) %>%
  project(proj_0) %>%
  rast(nrows = 500, ncol = 500)
r_0_df <- r_0 %>%
  `values<-`(1) %>%
  mask(st_transform(bc, proj_0)) %>%
  as.data.frame(xy = TRUE) %>%
  select(-3) %>%
  as_tibble()

# add a slide of different bc projections
purrr::map2(c('+proj=longlat', 'epsg:3005', 'EPSG:32610', 'ESRI:54008'),
            c('Unprojected', 'BC Albers', 'UTM zone 10N', 'Sinusoidal'),
            \(.proj, .title) {
              ggplot() +
                facet_grid(~ title) +
                geom_sf(data = mutate(bc, title = .title)) +
                coord_sf(crs = .proj)
            }) %>%
  cowplot::plot_grid(plotlist = ., align = 'hv', nrow = 1)

ggsave('figures/bc-projections.png', width = 13, height = 4, scale = 0.5,
       bg = 'white', dpi = 600)

find_distance <- function(projection = '+proj=longlat', point = ubco) {
  point <- project(point, projection)
  bc <- st_transform(bc, projection)
  r <- vect(bc) %>%
    project(projection) %>%
    rast(nrows = 500, ncol = 500) %>%
    distance(point) %>%
    project(r_0)
  
    return(r)
}

rasters <- tibble(
  proj = c('+proj=longlat', 'epsg:3005', 'EPSG:32610', 'ESRI:54008', proj_0),
  name = c('Unprojected', 'BC Albers', 'UTM zone 10N', 'Sinusoidal', 'Azimuthal') %>%
    factor(x = ., levels = .),
  raster = lapply(proj, find_distance))

d <- rasters %>%
  mutate(raster = purrr::map(raster, \(ras) {
    mutate(r_0_df, distance = extract(ras, r_0_df)[, 2]) %>%
      mutate(distance = distance / 1e3) %>% # convert from m to km
      as_tibble()
  })) %>%
  unnest(raster)

#' `facet_wrap()` can't use free scales with `coord_sf()`
ggplot() +
  coord_sf(crs = proj_0) +
  facet_wrap(~ name, nrow = 1) +
  geom_raster(aes(x, y, fill = distance), filter(d, name != 'Azimuthal')) +
  scale_fill_viridis_c(expression(Distance~('km,'~log[10])),
                       trans = 'log10', limits = c(1, NA)) +
  theme_void() +
  theme(legend.position = 'top', legend.key.width = unit(0.5, 'in'))

ggsave('figures/distances.png', width = 13, height = 5, scale = 0.5,
       bg = 'white', dpi = 600)

ubcv <- vect(matrix(c(-123.246005, 49.260535), ncol = 2)) %>%
  set.crs('+proj=longlat') %>%
  project(proj_0)

# true distance between ubc vcouver and ubc okanagan is ~280 km
d %>%
  group_by(name) %>%
  filter(! is.na(distance)) %>%
  slice(sqrt((x - geom(project(ubcv, proj_0))[1, 'x'])^2 +
               (y - geom(project(ubcv, proj_0))[1, 'y'])^2) %>%
          which.min()) %>%
  ungroup() %>%
  transmute(name,
            vanc_dist = round(distance, 2),
            vanc_err = abs(round(distance - last(distance), 2)))

# estimate error in distances for each projection
d %>%
  select(! proj) %>%
  pivot_wider(names_from = name, values_from = distance) %>%
  pivot_longer(-c(x, y, Azimuthal),
               names_to = 'name', values_to = 'distance') %>%
  group_by(name) %>%
  mutate(error = distance - Azimuthal, # calculate the error
         name = paste0(name, '\nRange: (', round(min(error, na.rm = TRUE)), ', ',
                       round(max(error, na.rm = TRUE)), ') km')) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = unique(name)),
         error = if_else(abs(error) > 2, sign(error) * 2, error)) %>%
  ggplot() +
  facet_wrap(~ name, nrow = 1) +
  geom_raster(aes(x, y, fill = error), na.rm = TRUE) +
  geom_sf(data = bc, fill = 'transparent') +
  coord_sf(crs = proj_0) +
  scale_fill_distiller('Error (km)', type = 'div', palette = 9,
                       values = (c(-2, -0.1, 0, 0.1, 2) + 2) / 4,
                       direction = 1, limits = c(-2, 2)) +
  theme_void() +
  theme(legend.position = 'top', legend.key.width = unit(0.5, 'in'))

ggsave('figures/distance-errors.png', width = 13, height = 5, scale = 0.5,
       bg = 'white', dpi = 600)
