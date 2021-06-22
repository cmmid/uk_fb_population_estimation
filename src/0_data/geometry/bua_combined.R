# Script to extract BUAs to tiles

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
  require(sf)
})

# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c(here::here("data/raw/geometry/Built-up_Areas_(December_2011)_Boundaries_V2-shp/Built-up_Areas_(December_2011)_Boundaries_V2.shp"),
              here::here("data/raw/geometry/settlements2016boundaries (2)/Settlements2016_MHW.shp"),
              here::here("data/processed/geometry/bua_combined/bua_combined.shp"))
} else {
  .args <- commandArgs(trailingOnly = T)
}

bua_eng <- st_read(.args[1])
bua_scot <- st_read(.args[2])

bua <- rbind(bua_eng %>% 
               rename(code = bua11cd, name = bua11nm) %>% 
               select(code, name), 
             bua_scot %>% 
               select(code, name))

st_write(bua, tail(.args, 1))

