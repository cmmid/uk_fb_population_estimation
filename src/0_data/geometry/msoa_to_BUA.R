# Script to extract MSOAs to BUAs

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
})

# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c("/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/raw/geometry/Middle_Layer_Super_Output_Areas_(December_2011)_Population_Weighted_Centroids/Middle_Layer_Super_Output_Areas_(December_2011)_Population_Weighted_Centroids.shp",
              "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/raw/geometry/Built-up_Areas_(December_2011)_Boundaries_V2-shp/Built-up_Areas_(December_2011)_Boundaries_V2.shp",
      "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/processed/geometry/msoa_bua.csv")
} else {
  .args <- commandArgs(trailingOnly = T)
}

msoa <- st_read(.args[1])
bua <- st_read(.args[2])


bua_int <- st_intersection(msoa, bua)

bua_msoa_lu <- bua_int %>% 
  select(msoa11cd, msoa11nm, bua11cd, bua11nm) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  as_tibble() %>% 
  mutate_all(as.character)

write_csv(bua_msoa_lu, tail(.args, 1))

