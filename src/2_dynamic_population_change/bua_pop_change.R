# Script to plot population change in BUAs

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
  require(sf)
})

# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c(here::here("data/processed/tiles.shp"),
              here::here("data/processed/population_adjusted/Britain_TilePopulation_varying_population.csv"),
              "/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/quick/regions/tile_regions.csv",
              "/Users/hamishgibbs/Documents/Covid-19/facebook_uk_representative/data/processed/tile_pop.csv",
              here::here("data/processed/geometry/tile_bua.csv"),
              here::here("data/config/periods.rds"),
      "output")
} else {
  .args <- commandArgs(trailingOnly = T)
}

tiles <- st_read(.args[1]) 

pop <- read_csv(.args[2])

tile_regions <- read_csv(.args[3]) %>% 
  select(-area)

tile_pop <- read_csv(.args[4])

tile_bua <- read_csv(.args[5])

periods <- read_rds(.args[6])

period_rectangles <- lapply(periods, function(x){
  return(
    annotate("rect", xmin = x$period_start, 
             xmax = x$period_end, 
             ymin = -Inf, ymax = Inf, 
             alpha = .2) 
  )
})

period_lines <- lapply(periods, function(x){
  return(
    geom_vline(aes(xintercept = x$i_date), size = 0.1, color = "red")
  )
})

bua_oa_pop <- tile_pop %>% 
  left_join(tile_bua, by = c('quadkey_12' = 'quadkey')) %>% 
  filter(hour == 16) %>% 
  drop_na(name) %>% 
  group_by(name) %>% 
  summarise(pop = sum(pop, na.rm = T), .groups = 'drop') %>% 
  arrange(-pop)

tile_bua_top <- tile_bua %>% 
  filter(name %in% bua_oa_pop$name)

p_data <- pop %>% 
  filter(hour == 16) %>% 
  left_join(tile_bua, by = c('quadkey_12' = 'quadkey')) %>% 
  drop_na(name) %>% 
  group_by(date_time, name) %>% 
  summarise(total_pop = sum(total_pop, na.rm = T), .groups = 'drop') %>% 
  left_join(bua_oa_pop, by = 'name') %>% 
  pivot_longer(!c(date_time, name), names_to = 'type', values_to = 'value') %>% 
  mutate(type = ifelse(type == 'pop', 'Static\nPopulation', 'Dynamic\nPopulation'))

p_data$name <- factor(p_data$name, levels = bua_oa_pop$name)

plot_buas <- function(bua_names){
  
  p <-  p_data %>% 
    filter(name %in% bua_names) %>% 
    ggplot() + 
    period_rectangles +
    period_lines +
    geom_path(aes(x = date_time, y = value, linetype = type), size = 0.21) + 
    facet_wrap(~name, scales = 'free') + 
    scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6, accuracy = 0.01)) + 
    theme_classic() + 
    theme(legend.title = element_blank(),
          axis.title.x = element_blank(),
          strip.background=element_rect(colour="white", 
                                        fill="white")) + 
    ylab('Population')
  
  return(p)
  
}

p_top_buas <- plot_buas(bua_oa_pop$name[1:6])
p_all_buas <- plot_buas(bua_oa_pop$name)

ggutils::ggsave_png_pdf(p_top_buas,
                        here::here("output/figs/bua_population_change.png"),
                        10, 5)
 
write_rds(p_top_buas, here::here("output/figs/bua_population_change.rds"))

ggutils::ggsave_png_pdf(p_all_buas,
                        here::here("output/figs/bua_population_change_all.png"),
                        17, 9)
p_data_perc <- p_data %>% 
  filter(type == "Dynamic\nPopulation") %>% 
  left_join(bua_oa_pop, by = "name") %>% 
  mutate(value_perc = ((value - pop) / pop))
  
p_data_perc$name <- factor(p_data_perc$name, levels = bua_oa_pop$name)

p_perc_all_buas <- p_data_perc %>% 
  ggplot() + 
  period_rectangles +
  period_lines +
  geom_path(aes(x = date_time, y = value_perc), size = 0.21) + 
  facet_wrap(~name, scales = 'free') + 
  scale_y_continuous(labels = scales:::percent) + 
  theme_classic() + 
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        strip.background=element_rect(colour="white", 
                                      fill="white")) + 
  ylab('Population')

ggutils::ggsave_png_pdf(p_perc_all_buas,
                        here::here("output/figs/bua_population_change_perc_all.png"),
                        17, 9)
