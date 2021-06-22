# Script to plot population change accross UK in 4 periods

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
  require(sf)
  require(scales)
})

# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c("/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/processed/tiles.shp",
              "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/processed/population_adjusted/Britain_TilePopulation_varying_population.csv",
              "/Users/hamishgibbs/Documents/Covid-19/facebook_mobility_uk/data/quick/regions/tile_regions.csv",
              "/Users/hamishgibbs/Documents/Covid-19/facebook_uk_representative/data/processed/tile_pop.csv",
              "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/processed/geometry/tile_bua.csv",
              "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/processed/geometry/la_bua.csv",
              "/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/data/raw/geometry/Local_Authority_Districts_(December_2019)_Boundaries_UK_BFC/Local_Authority_Districts_(December_2019)_Boundaries_UK_BFC.shp",
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

tile_la <- read_csv(.args[6])

la <- st_read(.args[7])

london_qks <- tile_bua %>% filter(name %in% c("Greater London BUA")) %>% pull(quadkey)
london <- tiles %>% filter(quadkey %in% london_qks)
london_las <- tile_la %>% filter(name %in% "Greater London BUA") %>% pull(lad19nm)
london_bbox <- st_as_sfc(st_bbox(london))

day_seconds <- function(day){
  return(day * 60 * 60 * 24)
}

get_difference <- function(pop, i_date, period_start, period_end, scale_breaks){
  
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
    pivot_wider(names_from = period, values_from = total_pop) %>% 
    mutate(diff = Post - Pre) %>% 
    drop_na(diff) %>% 
    ggutils::classify_intervals(., "diff", breaks = scale_breaks) %>% 
    mutate(value = fct_rev(value),
           diff_perc = ((Pre - Post) / Pre) * 100)
  
  return(data)
  
}

plot_difference <- function(data, title, subtitle=NA, 
                            scale_fill, legend.position = c(0.2, 0.8), 
                            lims = list(xlim(-8, 2), ylim(50.4, 58.4)),
                            country_size = 0.1){
  
  if (class(lims) != "list") {
    lims = ggutils::geo_lims(lims)
  }
  
  p <- tiles %>% 
    left_join(data, by = c('quadkey' = 'quadkey_12')) %>% 
    drop_na(diff) %>% 
    ggplot() + 
    ggutils::plot_basemap('United Kingdom', country_size = country_size, world_fill = '#EFEFEF',
                          country_fill = 'white') + 
    geom_sf(aes(fill = value), size = 0) + 
    scale_fill + 
    theme_void() + 
    lims + 
    labs(fill = 'Population\nchange') + 
    theme(legend.position = legend.position,
          plot.margin = margin(0, 0, 0, 0)) + 
    ggtitle(title, subtitle)
  
  return(p)
  
}

plot_london <- function(period_data, scale_fill){
  
  p <- plot_difference(period_data %>% filter(quadkey_12 %in% london_qks), 
                  title=NULL, 
                  subtitle=NULL, 
                  scale_fill = scale_fill, 
                  legend.position = "none",
                  lims = london,
                  country_size=0) + 
    geom_sf(data = la %>% filter(lad19nm %in% london_las), 
            fill = "transparent", color="black", size=0.05)
  
  return(p)
  
}

define_period <- function(i_date){
  return(
    list(i_date = i_date,
         period_start = i_date - day_seconds(14),
         period_end = i_date + day_seconds(13))
  )
}

periods <- list()

periods[["first_lockdown"]] <- define_period(as.POSIXct('2020-03-23 00:00', tz='UTC'))
periods[["summer"]] <- define_period(as.POSIXct('2020-07-21 00:00', tz='UTC'))
periods[["school"]] <- define_period(as.POSIXct('2020-09-01 00:00', tz='UTC'))
periods[["christmas"]] <- define_period(as.POSIXct('2020-12-25 00:00', tz='UTC'))

write_rds(periods, here::here("data/config/periods.rds"))

# Define custom color scale breaks
scale_breaks <- c(-110000, -50000, -10000, -5000, -1000, -250, 0, 250, 1000, 5000, 22000)

# Define change before and after reference dates
period_data <- list()

for (period in names(periods)){
  
  period_data[[period]] <- get_difference(pop = pop, 
                                          i_date = periods[[period]]$i_date, 
                                          period_start = periods[[period]]$period_start, 
                                          period_end = periods[[period]]$period_end,
                                          scale_breaks = scale_breaks)
  
  
}

diff <- lapply(period_data, function(x){return(x$diff)}) %>% unlist() %>% as.numeric()
diff_perc <- lapply(period_data, function(x){return(x$diff_perc)}) %>% unlist() %>% as.numeric()

scale_limits <- c(floor(min(diff)), ceiling(max(diff)))

# check that color scale is still within appropriate limits...
testthat::expect_lt(min(scale_breaks), scale_limits[1])
testthat::expect_gt(max(scale_breaks), scale_limits[2])

colors <- rev(c('#67001f', '#890029', '#b2182b','#d6604d','#f4a582','#fddbc7', '#E2F9FF', '#d1e5f0','#92c5de','#4393c3'))
scale_levs <- levels(period_data[['first_lockdown']]$value)
names(colors) <- scale_levs
scale_fill <- scale_fill_manual(values = colors, breaks = scale_levs)

p <- list()

p[["first_lockdown"]] <- plot_difference(period_data[["first_lockdown"]], title="a", subtitle="First lockdown: Mar 23, 2020", scale_fill = scale_fill, legend.position = "none")
p[["summer"]] <- plot_difference(period_data[["summer"]], title="b", subtitle="Summer: July 21, 2020", scale_fill = scale_fill, legend.position = "none")
p[["school"]] <- plot_difference(period_data[["school"]], title="c", subtitle="Return to school: Sept 1, 2020", scale_fill = scale_fill, legend.position = "none")
p[["christmas"]] <- plot_difference(period_data[["christmas"]], title="d", subtitle="Christmas: Dec 25, 2020", scale_fill = scale_fill, legend.position = "none")

legend <- cowplot::get_legend(p[["first_lockdown"]] + theme(legend.position = NULL))

p_map <- cowplot::plot_grid(plotlist = p,
                            ncol = 4)

p_map <- cowplot::plot_grid(ggplot() + theme_void(), p_map, legend,
                        nrow = 1, rel_widths = c(0.05, 0.8, 0.1))

ggutils::ggsave_png_pdf(p_map,
                        here::here('output/figs/period_pop_change.png'),
                        11, 4)

p_london <- list()

p_london[["first_lockdown"]] <- plot_london(period_data[["first_lockdown"]], scale_fill = scale_fill)
p_london[["summer"]] <- plot_london(period_data[["summer"]], scale_fill = scale_fill)
p_london[["school"]] <- plot_london(period_data[["school"]], scale_fill = scale_fill)
p_london[["christmas"]] <- plot_london(period_data[["christmas"]], scale_fill = scale_fill)

p_combined <- list()

for (name in names(p_london)){
  
  p_altered <- p[[name]] + geom_sf(data = london_bbox, fill = "transparent", size = 0.25, color = "black")
  
  p_combined[[name]] <- cowplot::ggdraw(p_altered) + 
    cowplot::draw_plot(p_london[[name]] + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)),
                       x = 0.63, y = 0.5, 
                       width = 0.37, height = 0.30)
}

p_map <- cowplot::plot_grid(plotlist = p_combined,
                            ncol = 4)

p_map <- cowplot::plot_grid(ggplot() + theme_void(), p_map, legend,
                            nrow = 1, rel_widths = c(0.06, 0.75, 0.14))


p_bua <- read_rds('/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/output/figs/bua_population_change.rds')

p_bua <- p_bua + ggtitle("e")

p_period_bua <- cowplot::plot_grid(p_map, p_bua, nrow = 2)

ggutils::ggsave_png_pdf(p_period_bua,
                        '/Users/hamishgibbs/Documents/Covid-19/fb_population_paper/output/figs/period_bua_pop_change.png',
                        12, 8)

p_hist <- ggplot() + 
  geom_histogram(aes(x = diff_perc)) + 
  scale_x_continuous() + 
  labs(x = "% Population Change (All 4 periods)")

p_hist

ggsave(here::here("output/figs/hist.png"), p_hist,
       width = 10, height = 5, units = "in")

classify_percent <- function(data){
  
  breaks_perc <- c(-100, -50, -10, -5, 0, 5, 10, 50, 100, 240)
  
  data <- data %>% 
    mutate(diff_perc = ((Post - Pre) / Pre) * 100) %>% 
    filter(diff_perc < Inf & diff_perc > -Inf) %>% 
    drop_na(diff_perc) %>% 
    arrange(diff_perc) %>% 
    ggutils::classify_intervals(., "diff_perc", breaks_perc, sym_sep = "% to ", sym_end = "%]") %>% 
    mutate(value = fct_rev(value))
  
  return(data)
}

plot_period_perc <- function(data, title, subtitle, scale_fill, legend.position){
  
  p <- tiles %>% 
    left_join(data, by = c('quadkey' = 'quadkey_12')) %>% 
    drop_na(diff) %>% 
    ggplot() + 
    ggutils::plot_basemap('United Kingdom', world_fill = '#EFEFEF',
                          country_fill = 'white') + 
    geom_sf(aes(fill = value), size = 0) + 
    scale_fill + 
    theme_void() + 
    xlim(-8, 2) + 
    ylim(50.4, 58.4) + 
    labs(fill = 'Population\nchange') + 
    theme(legend.position = legend.position,
          plot.margin = margin(0, 0, 0, 0)) + 
    ggtitle(title, subtitle)
  
  return(p)
}

colors <- rev(RColorBrewer::brewer.pal(9, "RdBu"))
scale_levs <- levels(test$value)
names(colors) <- scale_levs
scale_fill <- scale_fill_manual(values = colors, breaks = scale_levs)

p_perc <- list()

p_perc[["first_lockdown"]] <- plot_period_perc(classify_percent(period_data[["first_lockdown"]]),
                                               title="a", subtitle="First lockdown: Mar 23, 2020", 
                                               scale_fill = scale_fill, legend.position = "none")
p_perc[["summer"]] <- plot_period_perc(classify_percent(period_data[["summer"]]),
                                       title="b", subtitle="Summer: July 21, 2020", 
                                       scale_fill = scale_fill, legend.position = "none")
p_perc[["school"]] <- plot_period_perc(classify_percent(period_data[["school"]]),
                                       title="c", subtitle="Return to school: Sept 1, 2020",
                                       scale_fill = scale_fill, legend.position = "none")
p_perc[["christmas"]] <- plot_period_perc(classify_percent(period_data[["christmas"]]),
                                          title="d", subtitle="Christmas: Dec 25, 2020",
                                          scale_fill = scale_fill, legend.position = "none")
                 
p_perc[["first_lockdown"]] + 
  theme(legend.position = "right")


legend <- cowplot::get_legend(p_perc[["first_lockdown"]] + theme(legend.position = NULL))

p_map <- cowplot::plot_grid(plotlist = p_perc,
                            ncol = 4)

p_map <- cowplot::plot_grid(p_map, legend,
                            nrow = 1, rel_widths = c(0.85, 0.15))

ggutils::ggsave_png_pdf(p_map,
                        here::here('output/figs/period_pop_change_perc.png'),
                        11, 4)
