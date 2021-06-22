# Script to compare baseline and census populations

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
  require(sf)
  require(ggpubr)
})

if(interactive()){
  .args <-  c("/Users/hamishgibbs/Documents/Covid-19/facebook_uk_representative/data/processed/tile_pop.csv",
              here::here("data/processed/tiles.shp"),
              here::here("data/processed/population_adjusted/Britain_TilePopulation_varying_population.csv"),
              here::here("data/config/period_rectangles_inf.rds"),
              here::here("data/config/period_lines.rds"),
      "output")
} else {
  .args <- commandArgs(trailingOnly = T)
}

tile_pop <- read_csv(.args[1])

tiles <- st_read(.args[2]) %>%
  st_transform(27700)

pop <- read_csv(.args[3])

period_rectangles <- read_rds(.args[4])
period_lines <- read_rds(.args[5])

name_hours <- function(x){

  x <- x %>%
    mutate(hour = as.character(hour),
         hour = ifelse(hour == '0', '00:00-08:00', hour),
         hour = ifelse(hour == '8', '08:00-16:00', hour),
         hour = ifelse(hour == '16', '16:00-00:00', hour))

  return(x)

}

hour_pal <- c('00:00-08:00' = 'darkblue',
              '08:00-16:00' = 'orange',
              '16:00-00:00' = 'darkred')

base_pops <- tile_pop %>%
  group_by(hour) %>%
  summarise(n_baseline = sum(n_baseline, na.rm = T), .groups = 'drop') %>%
  name_hours

scale_limits <- c(
  min(tile_pop$prop * 100, na.rm = T),
  max(tile_pop$prop * 100, na.rm = T)
)

plot_percentage <- function(hour_sel, title, legend.position){

  p <- tile_pop %>%
    left_join(tiles %>% st_transform(4326), by = c("quadkey_12" = "quadkey")) %>%
    drop_na(prop) %>%
    filter(hour == hour_sel) %>%
    name_hours %>%
    st_as_sf() %>%
    ggplot() +
    ggutils::plot_basemap('United Kingdom', country_size = 0.1) +
    geom_sf(aes(fill = prop * 100), size = 0) +
    scale_fill_viridis_c(trans="log10", limits = scale_limits) +
    theme_void() +
    labs(fill = "Facebook\nUser %", title = title) +
    theme(legend.position = legend.position) +
    ylim(c(50, 58.5)) +
    xlim(c(-9, 2))

  return(p)

}

p_0 <- plot_percentage(0, "a", "none")
p_8 <- plot_percentage(8, "b", "none")
p_16 <- plot_percentage(16, "c", "none")
legend <- cowplot::get_legend(p_0 + theme(legend.position = "right"))

p_percentage <- cowplot::plot_grid(p_0, p_8, p_16, legend, nrow = 1, rel_widths = c(0.3, 0.3, 0.3, 0.1))

ggutils::ggsave_png_pdf(p_percentage,
                        here::here("output/figs/prop_map.png"),
                        10, 4)

p_n_crisis <- pop %>%
  group_by(date_time, hour) %>%
  summarise(n_crisis = sum(n_crisis, na.rm = T), .groups = 'drop') %>%
  name_hours %>%
  ggplot() +
  period_rectangles +
  period_lines +
  geom_line(aes(x = date_time, y = n_crisis, color = hour), size = 0.3, alpha = 0.8) +
  geom_hline(data = base_pops, aes(yintercept = n_baseline, color = hour), linetype = 'dashed', size = 0.5) +
  scale_color_manual(values = hour_pal) +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6, accuracy = 0.1)) +
  annotate("text", x = as.POSIXct("2020-02-15 00:00"), y = 6500000, label = "Baseline Period", size = 3) +
  annotate("segment",
           x = as.POSIXct("2020-01-20 00:00"),
           xend = as.POSIXct("2020-03-09 00:00"),
           y = 6300000, yend = 6300000, colour = "black", size = 0.5) +
  theme_classic() +
  labs(color = 'Time Window') +
  ylab('Number of users') +
  theme(axis.title.x = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.position = "none",
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0.5, unit = 'cm'))) +
  xlim(c(as.POSIXct("2020-01-20 00:00"), max(pop$date_time))) +
  ggtitle('b')

p_n_crisis

p_dens <- tile_pop %>%
  name_hours() %>%
  ggplot() +
  geom_density(aes(x = prop * 100, color = as.character(hour))) +
  scale_color_manual(values = hour_pal) +
  scale_x_continuous(trans="log10") +
  theme_classic() +
  xlab("Percentage Facebook Users") +
  ylab("Density") +
  theme(legend.position = "none",
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  labs(color = "Time Window") +
  ggtitle('c')

uk <- rnaturalearth::ne_states(country = 'United Kingdom', returnclass = 'sf')

p_map <-  tiles %>%
  st_transform(4326) %>%
  left_join(tile_pop %>% filter(hour == 16), by =c('quadkey' = 'quadkey_12')) %>%
  drop_na(pop) %>%
  ggplot() +
  ggutils::plot_basemap('United Kingdom', country_size = 0.1) +
  geom_sf(aes(fill = pop), size = 0) +
  scale_fill_viridis_c(trans="log10", label = scales::comma) +
  labs(fill = 'Population', title = "a", subtitle = "Baseline Period") +
  theme_void() +
  theme(legend.position = c(0.85, 0.8)) +
  theme(plot.margin = margin(0, 0, 0, 0)) +
  ylim(c(50, 58.5)) +
  xlim(c(-9, 2))

time_legend <- cowplot::get_legend(p_n_crisis + theme(legend.position = "right"))

p_comp <- cowplot::plot_grid(p_n_crisis, p_dens, nrow = 2, rel_heights = c(0.6, 0.4))

p_comp_leg <- cowplot::plot_grid(p_comp, time_legend, rel_widths = c(0.8, 0.2))

p <- cowplot::plot_grid(p_map, p_comp_leg, ncol = 2, rel_widths = c(0.4, 0.6))

ggutils::ggsave_png_pdf(p,
                        here::here("output/figs/pop_intro.png"),
                        11, 6)

p_rel <- tile_pop %>%
  filter(hour == 16) %>%
  ggplot() +
  geom_point(aes(x = pop, y = n_baseline), size = 0.05, alpha = 0.5) +
  scale_y_continuous(trans = "log10", labels = scales::unit_format(unit = "k", scale = 1e-3, accuracy = 0.01)) +
  scale_x_continuous(trans = "log10", labels = scales::unit_format(unit = "k", scale = 1e-3, accuracy = 0.01)) +
  theme_classic() +
  labs(y = "Facebook users", x = "Census population")


ggutils::ggsave_png_pdf(p_rel,
                        here::here("output/figs/pop_facebook_census.png"),
                        8, 4)


# weekly variance within time periods (supplement)
p_var <- pop %>%
  group_by(date_time, hour) %>%
  summarise(n_crisis = sum(n_crisis, na.rm = T),
            .groups = 'drop') %>%
  mutate(week = lubridate::floor_date(date_time, "week")) %>%
  group_by(week, hour) %>%
  summarise(variance = var(n_crisis),
            n = length(n_crisis),
            n_crisis = sum(n_crisis, na.rm= T)) %>%
  name_hours() %>%
  ggplot() +
  geom_point(aes(x = week, y = variance, color = hour), size = 0.2) +
  scale_color_manual(values = hour_pal) +
  theme_classic() +
  labs(y = "Variance", x = NULL, color = "Period")

ggutils::ggsave_png_pdf(p_var,
                        here::here("output/figs/time_period_variance.png"),
                        8, 4)

# difference from baseline all time (supplement)
pop_difference <- pop %>%
  filter(hour == 16) %>%
  left_join(tile_pop %>% filter(hour == 16), by = c("quadkey_12", "hour")) %>%
  mutate(difference_abs = total_pop - pop,
         difference_perc = ((total_pop - pop) / pop) * 100)

get_breaks <- function(v, factor = 15){
  min_val <- min(v, na.rm = T)
  max_val <- max(v, na.rm = T)
  return(c(min_val, min_val / factor, 0, max_val / factor, max_val))
}

breaks_perc <- get_breaks(pop_difference$difference_perc, 15)
breaks_abs <- get_breaks(pop_difference$difference_abs, 100)

p_perc <- pop_difference %>%
  ggplot() +
  geom_density(aes(x = difference_perc)) +
  scale_x_continuous(trans = "pseudo_log",
                     breaks = breaks_perc,
                     labels = scales::comma(breaks_perc)) +
  geom_vline(xintercept = 0, size = 0.2, linetype = "dashed") +
  theme_classic() +
  labs(y = "Density", title = "a", x = "Percent difference")

p_abs <- pop_difference %>%
  ggplot() +
  geom_density(aes(x = difference_abs)) +
  scale_x_continuous(trans = "pseudo_log",
                     breaks = breaks_abs,
                     labels = scales::comma(breaks_abs)) +
  geom_vline(xintercept = 0, size = 0.2, linetype = "dashed") +
  theme_classic() +
  labs(y = "Density", title = "b", x = "Absolute difference")

p_diff <- cowplot::plot_grid(p_perc, p_abs)

ggutils::ggsave_png_pdf(p_diff,
                        here::here("output/figs/population_variance_all_time.png"),
                        10, 3.5)
