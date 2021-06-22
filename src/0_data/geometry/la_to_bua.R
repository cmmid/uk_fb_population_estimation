# Script to extract LAs to BUAs

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
  require(sf)
})

# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c("/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/raw/geometry/Local_Authority_Districts_(December_2019)_Boundaries_UK_BFC/Local_Authority_Districts_(December_2019)_Boundaries_UK_BFC.shp",
              "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/processed/geometry/bua_combined/bua_combined.shp",
      "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/processed/geometry/la_bua.csv")
} else {
  .args <- commandArgs(trailingOnly = T)
}

la <- st_read(.args[1]) %>%
  st_simplify(preserveTopology = T, dTolerance=75) %>%
  mutate(geometry = st_make_valid(geometry))

bua <- st_read(.args[2])

bua_int <- st_intersection(la, bua)

la_intersection <- bua_int %>%
  mutate(area = as.numeric(units::set_units(st_area(geometry), 'km^2'))) %>%
  group_by(lad19cd, lad19nm) %>%
  top_n(1, wt = area)


la_bua_lu <- la_intersection %>%
  select(lad19cd, lad19nm, code, name) %>%
  st_drop_geometry()

write_csv(la_bua_lu, tail(.args, 1))
