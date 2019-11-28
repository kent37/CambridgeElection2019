# Exploratory analysis
library(gt)
library(leaflet)
library(mapview)
library(osmdata)
library(sf)
library(tmap)
source('DataSources.R')

ballots = all_ballots()
candidates = candidates()

lookup = deframe(candidates[, c(1, 3)])

# Duplicate R. Winters' ward/precinct table
wp_ones = ballots %>% 
  select(-choices) %>% 
  filter(!is.na(first), first %in% candidates$code) %>% 
  group_by(ward, precinct) %>% 
  count(first) %>% 
  ungroup() %>% 
  mutate(first=lookup[first])

wp_ones_wide = wp_ones %>% 
  pivot_wider(names_from=first, values_from=n, values_fill=list(n=0)) 

wp_ones_wide %>% gt()

# Map it
# Who got the most #1 votes in each precinct?
wp_most_ones = wp_ones %>% 
  group_by(ward, precinct) %>% 
  top_n(1, n) %>% 
  ungroup()

wards = st_read('ELECTIONS_WardsPrecincts.shp/ELECTIONS_WardsPrecincts.shp')

to_map = wp_most_ones %>% 
  mutate(WardPrecin=paste(ward, precinct, sep='-')) %>% 
  left_join(wards) %>% 
  st_as_sf()

colors = Polychrome::kelly.colors(length(unique(to_map$first)))
mapview(to_map, zcol='first', alpha.regions=0.4, col.regions=colors)

# How many #1 votes did each candidate get in each precinct?
to_map = wp_ones_wide %>% 
  mutate(WardPrecin=paste(ward, precinct, sep='-')) %>% 
  left_join(wards) %>% 
  st_as_sf()

roads <- opq(bbox = 'Cambridge, Massachusetts') %>%
  add_osm_feature(key = 'highway', 
                value = c('trunk', 'primary', 
                          'secondary', 'tertiary')) %>%
  osmdata_sf %>% 
  unique_osmdata

qtm(roads$osm_lines)

names = unique(wp_ones$first)

tm_shape(to_map) +
  tm_polygons(names, style='fixed', breaks=seq(0, 300, 25),
              border.col='steelblue',
              title='Number of #1 votes') +
  tm_facets(ncol=4, free.scales=FALSE) +
  # tm_shape(roads$osm_lines) +
  # tm_lines(col='black', alpha=0.4) + 
  tm_layout(
    main.title='#1 votes in Cambridge City election, Nov 2019',
    main.title.size=1,
            panel.show=TRUE, 
            panel.label.bg.color='white',
            panel.labels=names)
