# Script to adjust the entire population dataset by population

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
})


# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c("/Users/hamishgibbs/Documents/Covid-19/ingest_fb/data/raw/Britain_TilePopulation",
              "/Users/hamishgibbs/Documents/Covid-19/facebook_uk_representative/data/processed/tile_pop.csv",
              here::here("data/processed/tile_level_reference.csv"),
              here::here("data/processed/population_adjusted/Britain_TilePopulation_varying_population"))
} else {
  .args <- commandArgs(trailingOnly = T)
}

pop_files <- list.files(.args[1], full.names = T)
tile_pop <- read_csv(.args[2])
tile_ref <- read_csv(.args[3], col_types = cols(quadkey_12 = col_character(), quadkey_13 = col_character()))

# define total population in baseline periods for each window
total_baseline_pop <- tile_pop %>% 
  mutate(baseline_total_pop = n_baseline / prop) %>% 
  group_by(hour) %>% 
  summarise(baseline_total_pop = sum(baseline_total_pop, na.rm = T))

drop_x1 <- function(x){if('X1' %in% colnames(x)){return(x %>% select(-X1))} else {return(x)}}

process_pop_file <- function(in_fn, out_path, tile_ref, dummy = F){
  
  out_fn <- paste0(out_path, '/', strp_fn(in_fn))
  
  if (!out_fn %in% list.files(out_path, full.names = T)){
    
    print(in_fn)
    
    pop <- read_csv(in_fn, col_types = cols()) %>% 
      select(date_time, country, quadkey, n_crisis)
    
    # drop X1 column  
    pop <- drop_x1(pop)
    
    pop <- pop %>% 
      filter(country == 'GB') %>%
      mutate(n_crisis = ifelse(n_crisis == '\\N', NA, n_crisis)) %>% 
      mutate(n_crisis = as.numeric(n_crisis))
    
    pop <- pop %>% 
      mutate(quadkey = stringr::str_pad(quadkey, 13, 'left', '0'))
    
    pop_adj <- pop %>% 
      # Transform pop to tile 12
      left_join(tile_ref, by = c('quadkey' = 'quadkey_13')) %>% 
      group_by(date_time, quadkey_12) %>% 
      summarise(n_crisis = sum(n_crisis, na.rm = T), .groups = 'drop') %>% 
      # Join tile proportion estimates
      mutate(hour = lubridate::hour(date_time)) %>% 
      left_join(tile_pop, by = c('quadkey_12', 'hour')) %>% 
      mutate(total_pop = n_crisis / prop) %>% 
      select(date_time, hour, quadkey_12, n_crisis, total_pop)
    
    if (dummy){
      return(pop_adj)
    } else {
      write_csv(pop_adj, out_fn)  
    }
    
  }
  
}


strp_fn <- function(fn){
  
  fn <- tail(stringr::str_split(fn, '/')[[1]], 1)
  
  return(fn)
  
}

lapply(pop_files, process_pop_file, out_path = tail(.args, 1), tile_ref = tile_ref, dummy = F)
