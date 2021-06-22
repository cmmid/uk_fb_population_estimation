# Script to plot population change in populaiton deciles

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

get_period_rectangles <- function(ymin = -Inf, date_trans = F){

  period_rectangles <- lapply(periods, function(x){

    if(date_trans){
      xmin <- as.Date(x$period_start)
      xmax <- as.Date(x$period_end)
    } else {
      xmin <- x$period_start
      xmax <- x$period_end
    }

    return(
      annotate("rect", xmin = xmin,
               xmax = xmax,
               ymin = ymin,
               ymax = Inf,
               alpha = .2)
    )
  })

  return(period_rectangles)

}

period_rectangles <- get_period_rectangles(0)

write_rds(period_rectangles, here::here("data/config/period_rectangles.rds"))

period_rectangles <- get_period_rectangles(-Inf)

write_rds(period_rectangles, here::here("data/config/period_rectangles_inf.rds"))

period_lines <- lapply(periods, function(x){
  return(
    geom_vline(aes(xintercept = x$i_date), size = 0.1, color = "red")
  )
})

write_rds(period_lines, here::here("data/config/period_lines.rds"))

label_pop_q <- function(vect){

  return (
    factor(as.character(vect),
           levels = as.character(c(1:10)),
           labels = c("1 (low)", "2", "3", "4", "5", "6", "7", "8", "9", "10 (high)"))
  )
}

label_pop_q_dir <- function(vect, asc = T){
  if (asc){
    fct <- factor(as.character(vect),
           levels = as.character(c(1:10)),
           labels = c("1 (low)", "2", "3", "4", "5", "6", "7", "8", "9", "10 (high)"))
  } else {
    fct <- factor(as.character(vect),
                  levels = as.character(c(10:1)),
                  labels = rev(c("1 (low)", "2", "3", "4", "5", "6", "7", "8", "9", "10 (high)")))
  }
  return(fct)
}

tile_pop_q <- tile_pop %>%
  filter(hour == 16) %>%
  mutate(pop_q = ntile(pop, 10)) %>%
  drop_na(pop) %>%
  select(quadkey_12, pop, pop_q)

period_data <- list()

get_difference <- function(pop, i_date, period_start, period_end, period){

  data <- pop %>%
    mutate(period = NA,
           period = ifelse(date_time >= period_start &
                             date_time < i_date, 'Pre', period),
           period = ifelse(date_time >= i_date &
                             date_time <= period_end, 'Post', period)) %>%
    drop_na(period) %>%
    filter(hour == 16) %>%
    group_by(period, quadkey_12) %>%
    summarise(total_pop = mean(total_pop, na.rm = T), .groups = 'drop') %>%
    left_join(tile_pop_q, by = c("quadkey_12")) %>%
    group_by(period, pop_q) %>%
    summarise(total_pop = sum(total_pop, na.rm = T), .groups = 'drop') %>%
    pivot_wider(names_from = period, values_from = total_pop) %>%
    mutate(diff = Post - Pre) %>%
    drop_na(diff) %>%
    mutate(negative = ifelse(diff < 0, T, F),
           diff_log = log(abs(diff), 10),
           diff_log = ifelse(negative, diff_log * -1, diff_log))  %>%
    mutate(period = period)

  return(data)

}

for (period in names(periods)){

  period_data[[period]] <- get_difference(pop = pop,
                                          i_date = periods[[period]]$i_date,
                                          period_start = periods[[period]]$period_start,
                                          period_end = periods[[period]]$period_end,
                                          period = period)

}

bar_data <- do.call(rbind, period_data) %>%
  mutate(negative = diff < 0) %>%
  mutate(period = ifelse(period == "first_lockdown", "First lockdown", period),
         period = ifelse(period == "summer", "Summer", period),
         period = ifelse(period == "school", "Return to school", period),
         period = ifelse(period == "christmas", "Christmas", period)) %>%
  drop_na(pop_q)

bar_data$period <- factor(bar_data$period, levels=c(
  "First lockdown",
  "Summer",
  "Return to school",
  "Christmas"
))

p_bar <- bar_data %>%
  mutate(pop_q = label_pop_q_dir(pop_q, asc = T)) %>%
  ggplot() +
  geom_bar(aes(x = diff, y = pop_q, fill = negative), stat="identity") +
  scale_fill_manual(values = c("TRUE"="red", "FALSE"="blue")) +
  facet_wrap(~period, nrow = 1) +
  theme_classic() +
  scale_x_continuous(labels = scales::label_comma(scale = 1e-3)) +
  ylab("Population Decile") +
  xlab("Population Change (Thousands)") +
  theme(legend.position = "none",
        strip.background = element_blank()) +
  ggtitle("b")

ggutils::ggsave_png_pdf(p_bar,
                        here::here("output/figs/pop_change_period.png"),
                        10, 4.4)

map_data <- tiles %>%
  left_join(tile_pop_q, by = c("quadkey" = "quadkey_12")) %>%
  drop_na(pop_q)

pop_pal <-  c('#e31a1c', '#ff7f00','#fdb462','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#0868ac','#084081', '#081d58')
names(pop_pal) <- c("1 (low)", "2", "3", "4", "5", "6", "7", "8", "9", "10 (high)")


p_map <- map_data %>%
  mutate(pop_q = label_pop_q_dir(pop_q, asc = F)) %>%
  ggplot() +
  geom_sf(aes(fill = pop_q), size = 0) +
  scale_fill_manual(values=pop_pal) +
  ggutils::plot_basemap("United Kingdom", country_size = 0.1) +
  ylim(c(50, 58.5)) +
  xlim(c(-9, 2)) +
  labs(fill = "Population\nDecile") +
  theme_void() +
  theme(legend.position = c(0.1, 0.5))

ggutils::ggsave_png_pdf(p_map,
                        here::here("output/figs/pop_decile_map.png"),
                        4, 5.5)
pop_q_data <- pop %>%
  filter(hour == 16) %>%
  left_join(tile_pop_q, by = c("quadkey_12")) %>%
  drop_na(pop_q, total_pop) %>%
  group_by(pop_q, date_time) %>%
  summarise(total_pop = sum(total_pop, na.rm = T), .groups = "drop")

annot <- tile_pop_q %>% group_by(pop_q) %>%
  summarise(pop = sum(pop, na.rm = T), .groups = "drop")

p_q_ts <- pop_q_data %>%
  mutate(pop_q = label_pop_q_dir(pop_q, asc = T)) %>%
  ggplot() +
  period_rectangles +
  period_lines +
  geom_path(aes(x = date_time, y = total_pop, color = pop_q), size = 0.3) +
  scale_color_manual(values=pop_pal) +
  scale_y_continuous(trans = "log10", labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(margin =  margin(t = 0, r = 0, b = 0.5, l = 0, unit = 'cm'))) +
  ylab("Population (log scale)") +
  ggtitle("a") +
  guides(color=F)

ggutils::ggsave_png_pdf(p_q_ts,
                        here::here("output/figs/pop_q_ts.png"),
                        10, 4.4)

p <- cowplot::plot_grid(p_q_ts, p_bar, nrow = 2, rel_heights = c(0.4, 0.6))

ggutils::ggsave_png_pdf(p,
                        here::here("output/figs/pop_decile.png"),
                        8, 5.5)

p_q_ts_norm <- pop_q_data %>%
  left_join(annot, by = c("pop_q")) %>%
  mutate(total_pop_norm = (total_pop / pop) * 100) %>%
  mutate(pop_q = label_pop_q_dir(pop_q, asc = F),
         date_time = as.Date(date_time)) %>%
  ggplot() +
  geom_path(aes(x = date_time, y = total_pop_norm, color=pop_q), size = 0.2) +
  scale_color_manual(values=pop_pal) +
  geom_hline(aes(yintercept = 100), linetype = "dashed", size = 0.1) +
  get_period_rectangles(-Inf, T) +
  period_lines +
  facet_wrap(~pop_q, scales = "free_y", nrow = 2) +
  theme_classic() +
  ylab("Population relative to baseline (%)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(margin =  margin(t = 0, r = 0, b = 0, l = 0.5, unit = 'cm')),
        strip.background = element_blank()) +
  ggtitle("c") +
  guides(color=F)

ggutils::ggsave_png_pdf(p_q_ts_norm + facet_wrap(~pop_q, scales = "free_y", nrow = 4),
                        here::here("output/figs/pop_q_ts_norm.png"),
                        8, 6)

p_row <- cowplot::plot_grid(p_q_ts, p_bar, nrow = 1)
p <- cowplot::plot_grid(p_row, p_q_ts_norm + scale_x_date(date_breaks = "3 month") +
                          theme(axis.text.x = element_text(angle = -35, vjust = 0.5, hjust=0,
                                                           size = 8)),
                        ncol = 1)

ggutils::ggsave_png_pdf(p,
                        here::here("output/figs/pop_q.png"),
                        10, 6)
