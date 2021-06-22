library(readr)
library(dplyr)

pop <- read_csv(here::here("data/processed/population_adjusted/Britain_TilePopulation_varying_population.csv"))

tile_bua <- read_csv("data/processed/geometry/tile_bua.csv")

tile_pop <- read_csv("data/processed/tile_pop.csv")

bua_pop <- tile_bua %>% 
  left_join(tile_pop %>% filter(hour == 8), by = c("quadkey" = "quadkey_12")) %>% 
  group_by(name) %>% 
  summarise(pop = sum(pop, na.rm = T), .groups = "drop")
  

bua_pop_dynamic <- pop %>% 
  filter(hour == 8) %>% 
  rename(date = date_time) %>% 
  mutate(date = as.Date(date)) %>% 
  drop_na(total_pop) %>% 
  left_join(tile_bua, by = c("quadkey_12" = "quadkey")) %>% 
  drop_na(name) %>% 
  group_by(date, name) %>% 
  summarise(total_pop = sum(total_pop, na.rm = T), .groups = "drop") %>% 
  left_join(bua_pop, by = c("name")) %>% 
  mutate(pop_proportion = total_pop / pop)


write_rds(bua_pop_dynamic, "/Users/hamishgibbs/Documents/Covid-19/movement_seir/bua_pop_dynamic.rds")
