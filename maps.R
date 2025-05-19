library(sf)
library(RColorBrewer)

# Reading in the shapefile
# https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/LGA_2021_AUST_GDA2020_SHP.zip
lga_sf <- read_sf("raw_data/LGA_2021_AUST_GDA2020.shp")

# Merging all relevant dataframes with the shapefile
seifa_sf <- lga_sf |>
  rename("LGA" = "LGA_NAME21") |>
  inner_join(seifa_df, by = "LGA")

diab_seifa_sf <- lga_sf |>
  rename("LGA" = "LGA_NAME21") |>
  inner_join(diab_seifa_df, by = "LGA")

leading_cause_sf <- lga_sf |>
  rename("LGA" = "LGA_NAME21") |>
  inner_join(leading_cause_df, by = "LGA")

diab_seifa_filled_sf <- lga_sf |>
  rename("LGA" = "LGA_NAME21") |>
  inner_join(diab_seifa_filled_df, by = "LGA")

# Figure 2: Most common cause of death in each LGA
png("figs/leading_cause_map.png", 2000, 1200)
ggplot(leading_cause_sf) +
  geom_sf(aes(fill = Leading_cause)) +
  scale_fill_brewer(palette = "Paired", na.value = "grey") +
  labs(title = "Leading Cause of Death by LGA", fill = "Leading Cause") +
  theme(plot.title = element_text(face = "bold", size = 50, hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 28),
        legend.title = element_text(size = 36))
dev.off()

# Figure 4a: IRSAD of LGAs
png("figs/irsad_map.png", 2000, 1200)
ggplot(seifa_sf) +
  geom_sf(aes(fill = IRSAD), color = "white") +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  labs(title = "IRSAD of LGAs", fill = "IRSAD") +
  theme(plot.title = element_text(face = "bold", size = 50, hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.5),
        legend.text = element_text(size = 28),
        legend.title = element_text(size = 36),
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.key.spacing.y = unit(1, "cm"))
dev.off()

# Figure 4b: Crude diabetes death rates of LGAs 
# (if there is <20 deaths in total for an LGA there is no data)
png("figs/diabetes_map.png", 2000, 1200)
ggplot(diab_seifa_sf) +
  geom_sf(aes(fill = Crude_rate), color = "white") +
  scale_fill_viridis_c(option = "viridis") +
  labs(title = "Crude Diabetes Death Rate of LGAs", fill = "Crude Rate (per 100k)") +
  theme(plot.title = element_text(face = "bold", size = 50, hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.5),
        legend.text = element_text(size = 28),
        legend.title = element_text(size = 36),
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.key.spacing.y = unit(1, "cm"))
dev.off()

# Figure 7: Crude diabetes death rates of LGAs (missing values imputed)
png("figs/diabetes_filled_map.png", 2000, 1200)
ggplot(diab_seifa_filled_sf) +
  geom_sf(aes(fill = Crude_rate), color = "white") +
  scale_fill_viridis_c(option = "viridis") +
  labs(title = "Crude Diabetes Death Rate of LGAs (missing values imputed)", 
       fill = "Crude Rate (per 100k)") +
  theme(plot.title = element_text(face = "bold", size = 50, hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.5),
        legend.text = element_text(size = 28),
        legend.title = element_text(size = 36),
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.key.spacing.y = unit(1, "cm"))
dev.off()
