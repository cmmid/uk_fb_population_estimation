# Script to extract LADs to tiles

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
  require(sf)
})

# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c(here::here("data/raw/geometry/Local_Authority_Districts_(December_2019)_Boundaries_UK_BFC/Local_Authority_Districts_(December_2019)_Boundaries_UK_BFC.shp"),
              here::here("data/processed/tiles.shp"),
              here::here("data/processed/population_adjusted/Britain_TilePopulation_varying_population.csv"),
              here::here("data/processed/geometry/tile_lad.csv"))
} else {
  .args <- commandArgs(trailingOnly = T)
}

lad <- st_read(.args[1]) %>% 
  st_simplify(preserveTopology = T, dTolerance = 100) %>% 
  mutate(geometry == st_make_valid(geometry))

tiles <- st_read(.args[2]) %>% 
  st_transform(27700) %>% 
  mutate(area = as.numeric(units::set_units(st_area(geometry), "km^2")))

pop <- read_csv(.args[3])

tiles <- tiles %>% 
  filter(quadkey %in% unique(pop$quadkey_12))

tile_lad_int <- st_intersection(tiles, lad)

tile_lad_int <- tile_lad_int %>% 
  mutate(area_int = as.numeric(units::set_units(st_area(geometry), "km^2")))

tile_lad_int_lu <- tile_lad_int %>% 
  mutate(area_int_prop = area_int / area) %>% 
  select(quadkey, lad19cd, lad19nm, area_int_prop) %>% 
  st_drop_geometry()
  
write_csv(tile_lad_int_lu, tail(.args, 1))
