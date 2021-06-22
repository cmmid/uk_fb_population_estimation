# Script to combine population estimates into a single file

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
})

# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c("/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/processed/population_adjusted/Britain_TilePopulation_varying_population",
      "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/processed/population_adjusted/Britain_TilePopulation_varying_population.csv")
} else {
  .args <- commandArgs(trailingOnly = T)
}

pop_files <- list.files(.args[1], pattern = '*.csv', full.names = T)

pop <- lapply(pop_files, read_csv, col_types = cols())

pop <- do.call(rbind, pop)

write_csv(pop, tail(.args, 1))


