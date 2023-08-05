library('dplyr')     # for data wrangling
library('sf')        # for spatial features
library('ggplot2')   # for fancy plots
library('terra')     # for rasters
theme_set(cowplot::theme_map())

# fire polygons
fires <- read_sf('data/BC-Government-data/fire-perimeters/H_FIRE_PLY_polygon.shp')

# bc shapefile
bc <- filter(canadianmaps::PROV, PRENAME == 'British Columbia') %>%
  st_geometry() %>%
  st_transform(st_crs(fires)) # use Albers projection

# fire polygons
ggplot() +
  geom_sf(data = bc, fill = 'white', color = 'black') +
  geom_sf(data = fires, fill = 'red', color = 'transparent', alpha = 0.75)

ggsave('figures/bc-polygons.png', width = 4.6, height = 3.8,
       bg = 'transparent')

# fire centroids
centers <- st_geometry(fires) %>% # only keep geometry; drop attributes
  st_centroid() # calculate centroids

ggplot() +
  geom_sf(data = bc, fill = 'white', color = 'black') +
  geom_sf(data = centers, color = 'red', size = 0.03, alpha = 0.5)

ggsave('figures/bc-centers.png', width = 4.6, height = 3.8,
       bg = 'transparent')

r <-
  rasterize(vect(centers), # convert to spatVect for terra package
            rast('data/modis-ndvi-rasters/VI_16Days_1Km_v61/NDVI/MOD13A2_NDVI_2012_001.tif') %>%
              aggregate(50) %>%
              project(bc),
            fun = 'sum', # count number of points in each cell
            background = 0) %>% # cells with no points are 0 rather than NA
  mask(st_as_sf(bc)) %>%
  as.data.frame(xy = TRUE) %>%
  rename(fires = 3)

ggplot() +
  geom_raster(aes(x, y, fill = fires), r, show.legend = FALSE) +
  geom_sf(data = bc, fill = 'transparent', lwd = 0.5, color = 'black') +
  labs(x = NULL, y = NULL) +
  scale_fill_distiller(palette = 7, direction = 1)

ggsave('figures/bc-raster.png', width = 4.6, height = 3.8,
       bg = 'transparent')
