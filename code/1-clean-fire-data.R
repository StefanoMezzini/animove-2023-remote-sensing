library('dplyr')     # for data wrangling
library('purrr')     # for functional programming
library('tidyr')     # for data wrangling
library('sf')        # for spatial features
library('lubridate') # makes working with dates smoother
library('ggplot2')   # for fancy plots
library('khroma')    # for colorblind-friendly palettes
theme_set(theme_bw())

# fire polygons ----
fires <- read_sf('data/BC-Government-data/fire-perimeters/H_FIRE_PLY_polygon.shp')

# provincial shapefiles ----
# canadian provinces
ca <- st_geometry(canadianmaps::PROV) %>% # geometry only; drop attributes
  st_transform(st_crs(fires)) # use Albers projection

# bc shapefile
bc <- filter(canadianmaps::PROV, PRENAME == 'British Columbia') %>%
  st_geometry() %>%
  st_transform(st_crs(fires)) # use Albers projection

# health authority data
ha <- read_sf('data/BC-Government-data/bc-health-authorities/HLTH_ATH_B_polygon.shp') %>%
  st_transform(st_crs(fires)) %>%
  # add 2013 population estimates from http://www.bccdc.ca/resource-gallery/Documents/Educational%20Materials/Epid/Other/DOAP2014Ch1.pdf
  left_join(
    tibble(HA_NAME = c('Fraser', 'Vancouver Coastal',
                       'Vancouver Island', 'Interior', 'Northern'),
           ha_pop_2013 = c(1689875, 1138657, 752144, 717466, 283836)),
    by = 'HA_NAME') %>%
  transmute(ha_id = 1:n(),
            ha_name = HA_NAME,
            ha_pop_2013,
            ha_area_km2 = AREA_SQM / 1e6,
            ha_dens_2013 = ha_pop_2013 / ha_area_km2)

# kelowna spatial point
kelowna <- st_as_sf(tibble(x = -119.394105, y = 49.940072),
                    coords = c('x', 'y'), crs = '+proj=longlat') %>%
  st_transform(crs = st_crs(fires))

# plot the data ----
# canada
ggplot() +
  geom_sf(data = ca, fill = 'white', color = 'black') +
  geom_sf(data = bc, fill = 'forestgreen', color = 'black') +
  geom_sf(data = kelowna, shape = 19, color = 'white')

# bc health authorities
ggplot() +
  geom_sf(data = bc) +
  geom_sf(aes(fill = ha_name), ha, color = 'black') +
  geom_sf(data = kelowna) +
  scale_fill_bright(name = 'Health Authority') +
  theme(legend.position = c(0.825, 0.825),
        legend.background = element_rect(fill = 'grey95',
                                         color = 'grey'))

# fire polygons
ggplot() +
  geom_sf(data = bc, fill = 'white', color = 'black') +
  geom_sf(data = fires, fill = 'red', color = 'transparent', alpha = 0.75) +
  geom_sf(data = kelowna, shape = 19, color = 'black')

# extract fire centroids ----
centers <- st_geometry(fires) %>% # only keep geometry; drop attributes
  st_centroid() # calculate centroids

if(! dir.exists('data/fire-centers')) dir.create('data/fire-centers')
st_write(centers, 'data/fire-centers/fire-centers.shp', append = FALSE)

ggplot() +
  geom_sf(data = bc, fill = 'white', color = 'black') +
  geom_sf(data = centers, pch = '\U1F525')

# convert fire centroids to tibble format ----
d <-
  fires %>%
  rename_with(.fn = tolower) %>% # makes wrangling easier
  transmute(fire_id = fire_no, # more understandable name
            size_km2 = size_ha / 100,
            data_source = source, # more understandable name
            fire_date = case_when( # date is currently a character
              nchar(fire_date) == 8 ~ as.Date(fire_date,
                                              format = '%Y%m%d'),
              nchar(fire_date) == 12 ~ as.Date(fire_date,
                                               tz = 'America/Vancouver',
                                               format = '%Y%m%d%H%M%S')),
            year = year(fire_date),
            doy = yday(fire_date)) %>%
  # add fire centers
  bind_cols(
    # with BC Albers projection
    centers %>%
      do.call(rbind, .) %>% # convert to a matrix
      as.data.frame() %>% # convert to a data.frame for column names
      as_tibble() %>% # for ease of use
      rename(x_albers = V1, y_albers = V2), # make names understandable
    # unprojected
    centers %>%
      st_transform('+proj=longlat') %>% # unproject the data
      do.call(rbind, .) %>%
      as.data.frame() %>%
      as_tibble() %>%
      rename(long = V1, lat = V2))

#' *NOTE:* data is not clean, as there are duplicates
filter(d, duplicated(paste(d$fire_id, d$size_km2)) |
         duplicated(paste(d$fire_id, d$size_km2), fromLast = TRUE)) %>%
  arrange(fire_id)

# assign fires to health authority by fire centroid ----
#' some centroids are outside bc, so `bind_cols(d, st_intersect())` fails
d <- mutate(d,
            ha_id = st_intersects(centers, ha) %>%
              as.numeric())

# some estimated centers and polygons are outside of BC
filter(d, is.na(ha_id)) # fire centers outside of BC
mean(is.na(d$ha_id)) # very few outside BC

# check which fires fall outside
ggplot() +
  geom_sf(data = bc) +
  geom_sf(aes(fill = ha_name), ha, color = 'black') +
  geom_sf(data = filter(d, is.na(ha_id)) %>%
            st_buffer(1e4), fill = 'blue') +
  geom_sf(data = filter(d, is.na(ha_id)), fill = 'red') +
  geom_sf(data = kelowna) +
  scale_fill_bright(name = 'Health Authority') +
  theme(legend.position = c(0.825, 0.825),
        legend.background = element_rect(fill = 'grey95',
                                         color = 'grey'))

# tedious data cleaning is important but beyond the scope of this workshop
d <- filter(d, ! is.na(ha_id))

# add health authority information
d <- left_join(d,
               as.data.frame(ha) %>%
                 select(-geometry),
               by = 'ha_id') %>%
  as.data.frame() %>%
  select(-geometry)

# save as csv
write.csv(d, file = 'data/fires-and-population.csv', row.names = FALSE)
head(read.csv('data/fires-and-population.csv')) # check

# only keep health authority columns
d %>%
  select(contains('ha_')) %>%
  group_by(ha_name) %>%
  slice(1) %>% # take first row for each ha
  arrange(ha_id) %>%
  write.csv(file = 'data/health-authority-data.csv', row.names = FALSE)
read.csv('data/health-authority-data.csv') # check
