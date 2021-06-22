# Script to extract BUAs to tiles

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
  require(sf)
})

# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c("/Users/hamishgibbs/Documents/Covid-19/ingest_fb/data/geometry/tiles/tiles.shp",
              "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/raw/geometry/Built-up_Areas_(December_2011)_Boundaries_V2-shp/Built-up_Areas_(December_2011)_Boundaries_V2.shp",
              "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/raw/geometry/settlements2016boundaries (2)/Settlements2016_MHW.shp",
              "/Users/hamishgibbs/Documents/Covid-19/facebook_uk_representative/data/processed/tile_pop.csv",
      "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/processed/geometry/tile_bua.csv")
} else {
  .args <- commandArgs(trailingOnly = T)
}

tiles <- st_read(.args[1]) %>% 
  st_transform(27700)

bua_eng <- st_read(.args[2])
bua_scot <- st_read(.args[3])

tile_pop <- read_csv(.args[4])

bua <- rbind(bua_eng %>% 
               rename(code = bua11cd, name = bua11nm) %>% 
               select(code, name), 
             bua_scot %>% 
               select(code, name))

tile_bua <- st_intersection(tiles, bua)

# get top BUAs by pop
high_pop_bua_name <- tile_bua %>% 
  mutate(area = units::set_units(st_area(geometry), 'km^2')) %>% 
  st_drop_geometry() %>% 
  group_by(quadkey) %>% 
  top_n(1, area) %>% 
  left_join(tile_pop, by = c('quadkey' = 'quadkey_12')) %>% 
  group_by(name) %>% 
  summarise(pop = sum(pop, na.rm = T), .groups = 'drop') %>% 
  arrange(-pop) %>% 
  slice(1:20) %>% 
  pull(name) %>% 
  as.character()
  
high_pop_bua_name

high_pop_bua <- bua %>% 
  filter(name %in% high_pop_bua_name)

high_pop_bua %>% 
  ggplot() + 
  geom_sf()

high_pop_tile <- st_intersection(tiles, high_pop_bua)
high_pop_tiles <- high_pop_tile$quadkey %>% unique()

tiles %>% 
  filter(quadkey %in% high_pop_tiles) %>% 
  ggplot() + 
  geom_sf(data = high_pop_bua, fill = 'red', color = 'black', size = 0.1) + 
  geom_sf(fill = 'transparent', color = 'black', size = 0.3) + 
  theme_void()

hpt <- high_pop_tile %>% 
  mutate(area = units::set_units(st_area(geometry), 'km^2')) %>% 
  st_drop_geometry() %>% 
  group_by(quadkey) %>% 
  top_n(1, area) %>% 
  select(quadkey, name) %>% 
  ungroup() %>% 
  mutate_all(as.character)


tiles %>% 
  left_join(hpt, by = 'quadkey') %>% 
  drop_na(bua11nm) %>% 
  ggplot() + 
  geom_sf(aes(fill = bua11nm)) + 
  theme_void() + 
  theme(legend.title = element_blank())

write_csv(hpt, tail(.args, 1))

