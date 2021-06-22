# Script to compare baseline and census populations

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
  require(sf)
})

if(interactive()){
  .args <-  c("/Users/hamishgibbs/Documents/Covid-19/facebook_uk_representative/data/processed/tile_pop.csv",
              here::here("data/processed/tiles.shp"),
              "output")
} else {
  .args <- commandArgs(trailingOnly = T)
}

tile_pop <- read_csv(.args[1])

tiles <- st_read(.args[2])

lower <- median(tile_pop$prop, na.rm = T) - 3 * mad(tile_pop$prop, constant = 1, na.rm = T)
upper <- median(tile_pop$prop, na.rm = T) + 10 * mad(tile_pop$prop, constant = 1, na.rm = T)

qks <- tile_pop %>% 
  filter(prop > 1) %>% 
  group_by(hour, quadkey_12) %>% 
  summarise(fill_label = "Facebook\nusers >100%") %>% 
  name_hours()

p_outlier <- qks %>% 
  left_join(tiles, by = c("quadkey_12" = "quadkey")) %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(size = 0, aes(fill = fill_label)) + 
  scale_fill_manual(values = c("Facebook\nusers >100%" = "red")) + 
  ggutils::plot_basemap() + 
  theme_void() + 
  ylim(c(50, 58.5)) + 
  xlim(c(-9, 2)) + 
  facet_wrap(~hour) + 
  labs(fill = NULL)

ggutils::ggsave_png_pdf(p_outlier, 
                        here::here("output/figs/outlier_tiles.png"), 
                        8.5, 3.5)

# number
n_tiles <- qks %>% pull(quadkey_12) %>% unique() %>% length()

n_tiles

# and percentage of all tiles
(n_tiles / tile_pop$quadkey_12 %>% unique() %>% length()) * 100

