# Script to plot the travel difference for all tiles through time

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
  require(sf)
})

# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c("/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/processed/tiles.shp",
              "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/processed/population_adjusted/Britain_TilePopulation_varying_population.csv",
              "/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/quick/regions/tile_regions.csv",
              "/Users/hamishgibbs/Documents/Covid-19/facebook_uk_representative/data/processed/tile_pop.csv",
              "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/processed/geometry/tile_bua.csv",
              "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/config/periods.rds",
              "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/config/period_rectangles.rds",
              "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/config/period_lines.rds",
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
period_rectangles <- read_rds(.args[7])
period_lines <- read_rds(.args[8])

interventions <- list()

interventions[["second_lockdown"]] <- define_period(as.POSIXct('2020-10-31 00:00', tz='UTC'))
interventions[["school_open_2021"]] <- define_period(as.POSIXct('2021-03-08 00:00', tz='UTC'))

pop_lagged <- pop %>%
  filter(hour == 16) %>%
  replace_na(list(total_pop = 0)) %>%
  mutate(week = lubridate::floor_date(date_time, unit = "week")) %>%
  group_by(week, quadkey_12) %>%
  summarise(total_pop = mean(total_pop, na.rm = T), .groups = "drop") %>%
  group_by(quadkey_12) %>%
  mutate(total_pop_lag = lag(total_pop, 1, default=NA)) %>%
  ungroup()

pop_lagged_week <- pop_lagged %>%
  mutate(total_pop_diff = total_pop - total_pop_lag) %>%
  group_by(week) %>%
  summarise(total_pop_diff = sum(abs(total_pop_diff), na.rm = T))

p_dis <- pop_lagged_week %>%
  ggplot() +
  period_rectangles +
  period_lines +
  geom_path(aes(x = week, y = total_pop_diff), size = 0.25) +
  ylab("Weekly population change") +
  theme_classic() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  ggtitle("a")

ggutils::ggsave_png_pdf(p_dis,
                        '/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/output/figs/pop_movement.png',
                        8, 4)

plot_intervention <- function(int, label){
  return (
    list(geom_vline(aes(xintercept = int$i_date), linetype = "dashed", size = 0.2),
         annotate(geom = "label", x = int$i_date, y = 2e6, label = label))

  )
}

# repeat in daily periods
pop_lagged <- pop %>%
  filter(hour == 16) %>%
  replace_na(list(total_pop = 0)) %>%
  arrange(date_time) %>%
  group_by(quadkey_12) %>%
  mutate(total_pop_lag = lag(total_pop, 1, default=NA)) %>%
  ungroup()

p_int <- pop_lagged %>%
  mutate(total_pop_diff = total_pop - total_pop_lag) %>%
  group_by(date_time) %>%
  summarise(total_pop_diff = sum(abs(total_pop_diff), na.rm = T),
            .groups = "drop") %>%
  mutate(date_time = as.Date(date_time)) %>%
  rename(Daily = total_pop_diff) %>%
  left_join(pop_lagged_week, by = c("date_time" = "week")) %>%
  rename(Weekly = total_pop_diff) %>%
  pivot_longer(!date_time) %>%
  drop_na(value) %>%
  ggplot() +
  period_rectangles +
  period_lines +
  geom_path(aes(x = date_time, y = value, color = name, size = name)) +
  scale_color_manual(values = c("Weekly" = "red", "Daily" = "black")) +
  scale_size_manual(values = c("Weekly" = 0.5, "Daily" = 0.25)) +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  plot_intervention(interventions[[1]], "1") +
  plot_intervention(interventions[[2]], "2") +
  labs(color = "Period") +
  guides(size = FALSE) +
  theme_classic() +
  theme(axis.title.x = element_blank()) +
  ylab("Absolute population change")

ggutils::ggsave_png_pdf(p_int,
                        '/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/output/figs/pop_movement.png',
                        9, 4)


day_seconds <- function(day){
  return(day * 60 * 60 * 24)
}

define_period <- function(i_date){
  return(
    list(i_date = i_date,
         period_start = i_date - day_seconds(14),
         period_end = i_date + day_seconds(13))
  )
}

pop_london <- pop %>%
  filter(hour == 8) %>%
  filter(quadkey_12 %in% c(tile_bua %>% filter(name == "Greater London BUA") %>% pull(quadkey))) %>%
  mutate(date = as.Date(date_time))

get_pop_change <- function(pop, type, intervention){

  i_data <- pop %>%
    mutate(days_from = date - as.Date(intervention$i_date),
           type = type) %>%
    filter((days_from > -14 & days_from < 14)) %>%
    group_by(days_from, type) %>%
    summarise(total_pop = sum(total_pop, na.rm = T), .groups='drop')

  i_data_pop <- i_data %>% filter(days_from == 0) %>% pull(total_pop)

  i_data <- i_data %>% mutate(change = total_pop - i_data_pop)

  return(i_data)

}

l1 <- get_pop_change(pop_london, 'First Lockdown', interventions$first_lockdown)
l2 <- get_pop_change(pop_london, 'Second Lockdown', interventions$second_lockdown)
t4 <- get_pop_change(pop_london, 'Tier 4 Announcement', interventions$tier_4)

p_intervention <- rbind(l1, l2, t4) %>%
  ggplot() +
  geom_vline(aes(xintercept=0), linetype='dashed') +
  geom_path(aes(x = days_from, y = total_pop, color = type)) +
  scale_color_brewer(palette='Set2') +
  theme_classic() +
  ylab('Total Population') +
  xlab('Days from intervention') +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.85)) +
  ggtitle("b", 'Population Change Greater London')

ggutils::ggsave_png_pdf(p_intervention,
                        '/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/output/figs/pop_change_london_interventions.png',
                        8, 3.5)

p_int <- cowplot::plot_grid(p_dis, p_intervention, ncol = 1, rel_heights = c(0.4, 0.6))

ggutils::ggsave_png_pdf(p_int,
                        '/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/output/figs/pop_change_interventions.png',
                        8, 6)
