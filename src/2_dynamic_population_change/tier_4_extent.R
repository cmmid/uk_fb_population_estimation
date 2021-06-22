suppressPackageStartupMessages({
  require(tidyverse)
  require(sf)
})

if(interactive()){
  .args <-  c("/Users/hamishgibbs/Documents/Covid-19/tier_four_restrictions/data/tier_la_2020_12_21.shp",
              "output")
} else {
  .args <- commandArgs(trailingOnly = T)
}

tier_4 <- st_read(.args[1]) %>% 
  st_transform(4326)

uk_countries <- rnaturalearth::ne_states("United Kingdom", returnclass = "sf") %>% 
  group_by(geonunit) %>% 
  summarise(n = n())

p_tier_4 <- tier_4 %>% 
  filter(Value == "Tier 4") %>% 
  ggplot() + 
  ggutils::plot_basemap('United Kingdom', country_size = 0, world_fill = '#EFEFEF',
                        country_fill = 'white') + 
  geom_sf(data = uk_countries, fill = "white", color = "black", size = 0.1) + 
  geom_sf(fill = "darkgreen", color = "darkgreen", size = 0.1) + 
  ylim(c(50, 58.5)) + 
  xlim(c(-9, 2)) + 
  theme_void()


ggutils::ggsave_png_pdf(p_tier_4, 
                        here::here("output/figs/tier_4_extent.png"),
                        4, 6)
