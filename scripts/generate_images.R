
# images used in VicBioCon 2023 presentation slides

library(galah)
library(ggplot2)
library(ggridges)
library(gganimate)
library(dplyr)
library(sf)
library(hull2spatial)
library(alphahull)
library(ozmaps)
library(rmapshaper)
library(purrr)
library(tidyr)

# 01. title (ridgeline) -----

marine <- galah_call() |> 
  galah_identify("gastropoda") |> 
  galah_group_by(cl966, month) |> 
  atlas_counts() |> 
  rename(region = cl966)

terrestrial <- galah_call() |> 
  galah_identify("gastropoda") |> 
  galah_group_by(cl1048, month) |> 
  atlas_counts() |> 
  rename(region = cl1048)

gastropoda <- bind_rows(marine, terrestrial)

ggplot(gastropoda, 
       aes(x = count, y = reorder(region, count))) +
  geom_density_ridges(scale = 10, 
                      fill = "#7E98A8", 
                      colour = "#273b50", 
                      size = 0.2, 
                      rel_min_height = 0.01) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(2, 0, 2, 0, "cm"),
        plot.background = element_rect(fill = "#E9EEF5"))

ggsave("images/title_ridges.tiff",
       device = "tiff",
       width = 5, 
       height = 4, 
       units = "in")


# 02. acknowledgement (ridgeline) -----

eyr <- galah_call() |> 
  galah_identify("eopsaltria australis") |> 
  galah_group_by(cl1048, month) |> 
  atlas_counts() |> 
  filter(cl1048 %in% c("South Eastern Queensland",
                       "Sydney Basin",
                       "South Eastern Highlands",
                       "South East Coastal Plain",
                       "NSW North Coast",
                       "Victorian Midlands", 
                       "South East Corner",
                       "NSW South Western Slopes",
                       "Brigalow Belt South"))


ggplot(eyr, 
       aes(x = count, y = reorder(cl1048, count))) +
  geom_density_ridges(scale = 2, 
                      fill = "#a47823", 
                      colour = "#fffaf3", 
                      size = 0.2, 
                      rel_min_height = 0.01) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(0.5, 0, 0, 0, "cm"),
        plot.background = element_rect(fill = "#fffaf3", colour = "NA"))

ggsave("images/ack_ridges.tiff",
       device = "tiff",
       width = 6, 
       height = 1.5, 
       units = "in")


# 03. records --------

# counts_by_year <- galah_call() |> 
#   filter(year >= 1900) |> 
#   group_by(year) |> 
#   atlas_counts() |> 
#   mutate(year = as.integer(year)) |> 
#   arrange(year) |> 
#   mutate(cumsum_records = cumsum(count))
# 
# cumsum_anim <- ggplot(counts_by_year,
#        aes(x = year, y = cumsum_records)) +
#   geom_line(colour = "#273b50", linewidth = 2) +
#   geom_point(colour = "#273b50", size = 8) + 
#   scale_y_continuous(labels = scales::comma) +
#   theme_minimal() +
#   theme(plot.background = element_rect(fill = "#fffaf3", colour = "NA"),
#         panel.grid = element_blank(),
#         axis.title = element_blank(),
#         axis.text = element_text(colour = "#273b50", size = rel(3))) + 
#   transition_reveal(year)
# 
# animate(cumsum_anim, 
#         height = 1000, 
#         width = 1500, 
#         start_pause = 10, 
#         end_pause = 50)
# 
# anim_save("images/cumsum_records.gif")


# 04. metro melb outline (geolocate) -----

st_read("data/metro_region.shp") |> 
  ggplot() + 
  geom_sf(fill = NA, colour = "#7e98a8") + 
  theme_void()

ggsave("images/metro_outline.tiff", 
       device = "tiff", 
       width = 2, 
       height = 2, 
       units = "in")


# 05. alpha hull (Fonti + Margot) ----
dfly <- galah_call() |> 
  galah_identify("Austroargiolestes calcaris") |> 
  galah_filter(profile="ALA") |> 
  galah_select(group = "basic") |> 
  atlas_occurrences() |> 
  filter(!duplicated(decimalLongitude),
         !duplicated(decimalLatitude),
         !is.na(decimalLongitude),
         !is.na(decimalLatitude))

aus <- st_transform(ozmap_country, 4326)

dfly_sf <- dfly |> 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
           crs = 4326)

dfly_sf_ahull <- dfly |> 
  select(decimalLongitude, decimalLatitude) |> 
  ahull(alpha = 2) |> 
  ahull2poly() |> 
  st_as_sf() |> 
  st_set_crs(st_crs(aus))
  
ggplot() + 
  geom_sf(data = aus, colour = "black", fill = "white")  +
  geom_sf(data = dfly_sf, colour = "black", size = 0.5) +  
  geom_sf(data = dfly_sf_ahull, fill = "orange", alpha = 0.5) +
  coord_sf(xlim=c(142, 152), ylim=c(-32, -44)) +
  labs(x = "", y = "") + 
  theme_bw() +
  theme(plot.background = element_rect(fill = "#fffaf3", colour = "NA"),
        panel.background = element_rect(fill = "#fffaf3", colour = "NA"))

ggsave("images/hulls.tiff", 
       device = "tiff",
       width = 10, 
       height = 15, 
       units = "cm")


# 06. choropleth (Olivia + Dax) -----
actsuburbs <- st_read("data/act_localities.shp") |>
  ms_simplify(keep = 0.1) |>
  st_transform(crs = st_crs("WGS84")) |>
  st_make_valid() |>
  filter(LOC_CLASS != "District")

birdocc <- galah_call() |> 
  galah_identify("Aves") |> 
  galah_apply_profile(ALA) |>
  galah_filter(stateProvince == "Australian Capital Territory",
               dataProviderName == "BirdLife Australia") |>  
  atlas_occurrences()

bird_points_sf <- birdocc |> 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
           crs = st_crs("WGS84"))

act_counts <- actsuburbs |>
  mutate(bird_count = pmap(
    .l = list(x = actsuburbs$geometry),
    .f = function(x) {
      lengths(st_intersects(x, bird_points_sf))
    })) |> 
  rowwise() |> 
  mutate(log_counts = log10(bird_count)) |>
  mutate(counts_discrete = cut(log_counts, 
                               breaks = c(0, 1, 2, 3, 4, 5), 
                               labels = c(0, 10, 100, 1000, 10000),
                               include.lowest = TRUE)) |> 
  replace_na(list(counts_discrete = "0")) 
  
galah <- colorRampPalette(c("#FFD2CF",
                            "#EC9FAC",
                            "#D96D89",
                            "#B55474",
                            "#80556D"))(5)
                                     
ggplot() +
  geom_sf(data = act_counts,
          mapping = aes(fill = counts_discrete),
          colour = "NA") +
  scale_fill_manual(drop = FALSE,
                    values = galah) +
  theme_void() +
  theme(legend.position = "none")

ggsave("images/choropleth.tiff", 
       device = "tiff",
       width = 10, 
       height = 15, 
       units = "cm")


# 07. whale -------

megaptera <- galah_call() |> 
  galah_identify("megaptera novaeangliae") |>  
  galah_select(eventDate, decimalLatitude, decimalLongitude) |>  
  atlas_occurrences() |> 
  slice_sample(n = 1)

# saveRDS(megaptera, "data/megaptera.rds")
# megaptera <- readRDS("data/megaptera.rds")

ggplot() +
  geom_sf(data = st_transform(ozmap_country, 4326),
          fill = NA, colour = "#a47823") +
  geom_sf(data = st_as_sf(megaptera, 
                          coords = c("decimalLongitude", "decimalLatitude"), 
                          crs = 4326),
          size = 5, colour = "#7E98A8") +
  annotate("text", x = 145, y = -28, colour = "#273b50",
           label = paste0({round(megaptera$decimalLatitude, 2)}, 
                          ", ", 
                          {round(megaptera$decimalLongitude, 2)})) +
  coord_sf() +
  theme_void()

ggsave("images/whale_map.tiff", 
       device = "tiff",
       width = 15, 
       height = 10, 
       units = "cm")













