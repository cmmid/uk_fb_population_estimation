# Script to compare cases rate with dynamic population

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(ggplot2)
  require(sf)
})

# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c(here::here("data/processed/population_adjusted/Britain_TilePopulation_varying_population.csv"),
              "/Users/hamishgibbs/Documents/Covid-19/ingest_fb/data/geometry/tiles/tiles.shp",
              "/Users/hamishgibbs/Downloads/NHS_England_Regions__April_2020__Boundaries_EN_BFC-shp/NHS_England_Regions__April_2020__Boundaries_EN_BFC.shp",
              "/Users/hamishgibbs/Documents/Covid-19/facebook_uk_representative/data/processed/tile_pop.csv",
              here::here("data/processed/cases/cases_bua.csv"),
              here::here("data/processed/geometry/tile_bua.csv"),
              here::here("data/processed/geometry/la_bua.csv"),
              "output")
} else {
  .args <- commandArgs(trailingOnly = T)
}

pop <- read_csv(.args[1])

tiles <- st_read(.args[2]) %>%
  st_transform(27700)

nhs <- st_read(.args[3]) %>%
  st_simplify(dTolerance = 150, preserveTopology = T)

tile_pop <- read_csv(.args[4])

cases_bua <- read_csv(.args[5])

tile_bua <- read_csv(.args[6])

la_bua <- read_csv(.args[7]) %>%
  filter(name %in% tile_bua$name)

cases <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&format=csv')

#cases %>% mutate(country = substr(areaCode, 1, 1)) %>% pull(country) %>% unique()

bua_cases <- cases %>%
  rename(cases = newCasesBySpecimenDate) %>%
  filter(areaCode %in% la_bua$lad19cd) %>%
  inner_join(la_bua, by = c('areaCode' = 'lad19cd')) %>%
  group_by(date, name) %>%
  summarise(cases = sum(cases, na.rm = T), .groups = 'drop')

high_pop <- tile_bua %>%
  left_join(tile_pop %>% filter(hour == 16), by = c("quadkey" = "quadkey_12")) %>%
  group_by(name) %>%
  summarise(pop = sum(pop, na.rm = T)) %>%
  arrange(-pop) %>%
  slice(1: 8)

bua_static_pop <- tile_bua %>%
  left_join(tile_pop, by = c('quadkey' = 'quadkey_12')) %>%
  filter(hour == 16) %>%
  group_by(name) %>%
  summarise(pop = sum(pop, na.rm = T), .groups = 'drop')

bua_dynamic_pop <- pop %>%
  filter(quadkey_12 %in% tile_bua$quadkey) %>%
  filter(hour == 16) %>%
  left_join(tile_bua, by = c('quadkey_12' = 'quadkey')) %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(name, date) %>%
  summarise(total_pop = sum(total_pop, na.rm = T), .groups = 'drop')

p_data <- bua_cases %>%
  left_join(bua_static_pop, by = c('name')) %>%
  left_join(bua_dynamic_pop, by = c('date', 'name')) %>%
  drop_na(total_pop) %>%
  mutate(rate_static = (cases / pop) * 100000,
         rate_dynamic = (cases / total_pop) * 100000,
         rate_difference = ((rate_dynamic - rate_static) / rate_static) * 100) #%>%
  #replace_na(list(rate_difference = 0))

p_data$name <- factor(p_data$name, levels = bua_static_pop %>% arrange(-pop) %>% pull(name))

first_lockdown <- as.Date('2020-03-23')
second_lockdown <- as.Date('2020-10-31')
tier_4 <- as.Date('2020-12-19')

p_bua_cases <- p_data %>%
  #filter(name %in% high_pop$name) %>%
  ggplot() +
  geom_hline(aes(yintercept = 0), size = 0.3, color = "red") +
  geom_path(aes(x = date, y = rate_difference), size = 0.3) +
  facet_wrap(~name, scales = 'free_y', ncol = 4) +
  geom_vline(xintercept = first_lockdown, linetype="dashed", size=0.2) +
  geom_vline(xintercept = second_lockdown, linetype="dashed", size=0.2) +
  geom_vline(xintercept = tier_4, linetype="dashed", size=0.2) +
  theme_classic() +
  ylab('Cases rate difference (%)') +
  theme(axis.title.x = element_blank())

ggutils::ggsave_png_pdf(p_bua_cases,
                        here::here("output/figs/bua_cases_rate_diff.png"),
                        width = 11, height = 5)

ggutils::ggsave_png_pdf(p_bua_cases,
                        here::here("output/figs/bua_cases_rate_diff.png"),
                        width = 12, height = 12)

p_data %>% arrange(-rate_difference)
p_data %>% arrange(rate_difference) %>% pull(rate_difference) %>% max(na.rm = T)
p_data %>% arrange(rate_difference) %>% pull(rate_difference) %>% min(na.rm = T)

p_data %>% pull(rate_difference) %>% length()
p_data %>% filter(rate_difference < 10 & rate_difference > -10) %>% pull(rate_difference) %>% length()

6520 /7700

bua_dynamic_pop %>% filter(name == "Bournemouth/Poole BUA") %>% ggplot() + geom_path(aes(x = date, y = total_pop))
bua_dynamic_pop %>% filter(name == "Edinburgh") %>% ggplot() + geom_path(aes(x = date, y = total_pop))

sd(p_data %>% pull(rate_difference), na.rm = T)


max(p_data %>% filter(name %in% high_pop$name) %>% pull(rate_difference), na.rm = T)
min(p_data %>% filter(name %in% high_pop$name) %>% pull(rate_difference), na.rm = T)

p_data2 <- p_data %>%
  group_by(name) %>%
  mutate(rate_static = c(rep(NA, 6), zoo::rollmean(rate_static, 7)),
         rate_dynamic = c(rep(NA, 6), zoo::rollmean(rate_dynamic, 7)))


p_bua_cases <- p_data2 %>%
  ggplot() +
  geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 0.3) +
  geom_path(aes(x = date, y = rate_static), size = 0.3) +
  geom_path(aes(x = date, y = rate_dynamic), size = 0.3, color="red") +
  facet_wrap(~name, scales = 'free_y') +
  theme_classic() +
  ylab('Cases per 100k') +
  theme(axis.title.x = element_blank())

ggutils::ggsave_png_pdf(p_bua_cases,
                        here::here("output/figs/bua_cases_rate_abs.png"),
                        width = 10, height = 5)

tile_nhs_int <- st_intersection(tiles, nhs)

tile_nhs_int <- tile_nhs_int %>%
  mutate(area = as.numeric(units::set_units(st_area(geometry), 'km^2'))) %>%
  group_by(quadkey) %>%
  top_n(1, area) %>%
  st_drop_geometry() %>%
  select(quadkey, nhser20nm) %>%
  rename(region = nhser20nm) %>%
  mutate(region = as.character(region))

qks <- unique(pop$quadkey_12)

admissions <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&metric=newAdmissionsRollingRate&metric=newAdmissionsRollingSum&format=csv')

pop_region <- pop %>%
  left_join(tile_nhs_int, by = c('quadkey_12' = 'quadkey')) %>%
  drop_na(region) %>%
  group_by(date_time, region) %>%
  summarise(total_pop = sum(total_pop, na.rm = T), .groups = 'drop') %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(date, region) %>%
  summarise(total_pop = mean(total_pop, na.rm = T), .groups = 'drop')

setdiff(tile_nhs_int$region %>% unique(), admissions$areaName %>% unique())
setdiff(admissions$areaName %>% unique(), tile_nhs_int$region %>% unique())


p_hosp_rate <- admissions %>%
  arrange(date) %>%
  left_join(pop_region, by = c('date', 'areaName' = 'region')) %>%
  mutate(dynamic_rate = (newAdmissionsRollingSum / total_pop) * 100000,
         dynamic_difference = dynamic_rate - newAdmissionsRollingRate) %>%
  ggplot() +
  geom_path(aes(x = date, y = dynamic_difference, group = areaName), color = 'red',
            size = 0.4) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 0.2) +
  facet_wrap(~areaName, nrow = 2) +
  theme_classic() +
  ylab('Hospitalisation Rate Difference') +
  theme(axis.title.x = element_blank(),
        plot.margin = margin(0, 0, 0, 0))

ggutils::ggsave_png_pdf(p_hosp_rate,
                        here::here("output/figs/hosp_rate_difference.png"),
                        10,5)


a3_eng <- a3 %>%
  mutate(country = stringr::str_sub(lad19cd, 1, 1)) %>%
  filter(country == 'E')

a3_int <- st_intersection(tiles %>% filter(quadkey %in% qks), a3_eng)

cases <- readr::read_csv('https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv') %>%
  rename(date = `Specimen date`) %>%
  rename(code = `Area code`) %>%
  rename(name = `Area name`) %>%
  rename(count = `Daily lab-confirmed cases`) %>%
  mutate(area_type = stringr::str_sub(code, 1, 2)) %>%
  filter(area_type == 'E0') %>%
  filter(date >= as.Date('2020-03-10'))

cases_names <- cases$name %>% unique()
tile_names <- a3_int_top$lad19nm %>% unique()
setdiff(cases_names, tile_names)
setdiff(tile_names, cases_names)

a3_int_top <- a3_int %>%
  mutate(area = as.numeric(units::set_units(st_area(geometry), 'km^2'))) %>%
  group_by(quadkey) %>%
  mutate(top_area = area == max(area, na.rm = T)) %>%
  ungroup() %>%
  #top_n(1, area) %>%
  mutate(lad19nm = as.character(lad19nm),
         quadkey = as.character(quadkey)) %>%
  select(lad19nm, quadkey, top_area, area) %>%
  st_drop_geometry()

missing_names <- setdiff(a3_eng$lad19nm, a3_int_top %>% filter(top_area) %>% pull(lad19nm))

a3_int_top <- a3_int_top %>%
  group_by(lad19nm) %>%
  mutate(missing_top = ifelse(lad19nm %in% missing_names & area == max(area, na.rm = T),
                              T, F)) %>%
  ungroup() %>%
  filter(top_area | missing_top) %>%
  group_by(quadkey) %>%
  mutate(la_count = length(unique(lad19nm)),
         la_name = paste(unique(lad19nm), collapse = ', ')) %>%
  ungroup()

a3_ref <- a3_int_top %>%
  select(lad19nm, la_name) %>%
  distinct()

a3_int_top <- a3_int_top %>%
  group_by(quadkey) %>%
  summarise(la_name = unique(la_name))


la_cases <- cases %>%
  left_join(a3_ref, by = c('name' = 'lad19nm')) %>%
  mutate(la_name = ifelse(is.na(la_name), name, la_name)) %>%
  group_by(date, la_name) %>%
  summarise(count = sum(count, na.rm = T), .groups = 'drop')

la_tile_pop <- tile_pop %>%
  group_by(quadkey_12) %>%
  summarise(pop = mean(pop, na.rm = T), .groups = 'drop') %>%
  left_join(a3_int_top, by = c('quadkey_12' = 'quadkey')) %>%
  mutate(la_name = ifelse(la_name %in% c("Cornwall", "Isles of Scilly"), "Cornwall and Isles of Scilly", la_name)) %>%
  drop_na(la_name) %>%
  group_by(la_name) %>%
  summarise(pop = sum(pop, na.rm = T), .groups = 'drop')

la_tile_pop

la_pop_dynamic <- pop %>%
  left_join(a3_int_top, by = c('quadkey_12' = 'quadkey')) %>%
  mutate(la_name = ifelse(la_name %in% c("Cornwall", "Isles of Scilly"), "Cornwall and Isles of Scilly", la_name)) %>%
  group_by(la_name, date_time) %>%
  summarise(total_pop = sum(total_pop, na.rm = T), .groups = 'drop') %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(la_name, date) %>%
  summarise(total_pop = mean(total_pop, na.rm = T), .groups = 'drop')

cases_rates <- la_cases %>%
  left_join(la_tile_pop, by = 'la_name') %>%
  left_join(la_pop_dynamic, by = c('date', 'la_name')) %>%
  mutate(obs_rate = (count / pop) * 100000,
         dyn_rate = (count / total_pop) * 100000,
         rate_diff = dyn_rate - obs_rate)


p <- cases_rates %>%
  ggplot() +
  geom_path(aes(x = date, y = rate_diff, group = la_name),
            alpha = 0.5, size = 0.2) +
  theme_classic() +
  ylab('Cases Rate Difference') +
  theme(axis.title.x = element_blank())

ggutils::ggsave_png_pdf(p,
                        here::here("output/figs/cases_rate_difference.png"),
                        10, 5)
