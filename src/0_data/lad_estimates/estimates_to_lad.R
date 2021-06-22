# Script to extract pop estimates to LADs

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
  require(sf)
})

# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c(here::here("data/processed/geometry/tile_lad.csv"),
              here::here("data/processed/population_adjusted/Britain_TilePopulation_varying_population.csv"),
              here::here("data/processed/lad_estimates/lad_estimates.csv"))
} else {
  .args <- commandArgs(trailingOnly = T)
}

tile_lad_lu <- read_csv(.args[1])
pop <- read_csv(.args[2])


pop_lad <- pop %>%
  filter(hour == 16) %>%
  left_join(tile_lad_lu, by = c("quadkey_12" = "quadkey")) %>% 
  mutate(total_pop_lad = total_pop * area_int_prop,
         date = as.Date(date_time)) %>%
  group_by(date, lad19cd, lad19nm) %>%
  summarise(total_pop_lad = round(sum(total_pop_lad, na.rm = T), 2),
            .groups = "drop") %>%
  filter(date < as.Date("2021-03-01")) %>%
  drop_na(lad19nm)

write_csv(pop_lad, tail(.args, 1))
