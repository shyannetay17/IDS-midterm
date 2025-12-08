library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)

# FILE PATHS
continents_file <- "/Users/julie/Desktop/IDS project/continents-according-to-our-world-in-data.csv"
neet_file <- "/Users/julie/Desktop/IDS project/youth-not-in-education-employment-training.csv"

# LOAD DATA
continents <- read_csv(continents_file, show_col_types = FALSE)
neet <- read_csv(neet_file, show_col_types = FALSE)

# FILTER NEET FOR 2020 + CLEAN COLUMN NAME
neet_2020 <- neet %>%
  filter(Year == 2020) %>%
  rename(NEET = 4) %>%  # Using column position instead of full name
  select(Entity, Code, NEET)

# LOAD WORLD SHAPEFILE
world <- ne_countries(scale = "medium", returnclass = "sf")

# KEEP ONLY REAL ISO3 COUNTRY CODES
valid_iso <- world$iso_a3
neet_2020_clean <- neet_2020 %>%
  filter(Code %in% valid_iso)

# JOIN MAP + NEET DATA
world_neet <- world %>%
  left_join(neet_2020_clean, by = c("iso_a3" = "Code"))

# CREATE AND DISPLAY PLOT
p <- ggplot(world_neet) +
  geom_sf(aes(fill = NEET), color = "white", size = 0.1) +
  scale_fill_viridis(option = "C", na.value = "grey90",
                     name = "NEET\n(% of youth)",
                     limits = c(0, 55)) +
  labs(
    title = "Youth Not in Employment, Education or Training (NEET) – 2020",
    subtitle = "Proportion of youth aged 15–24",
    caption = "Source: OWID / World Bank"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# Display the plot
print(p)

# Also save it to your desktop
ggsave("/Users/julie/Desktop/IDS project/neet_heatmap_2020.png", 
       plot = p, width = 14, height = 8, dpi = 300)

cat("\nPlot saved to your desktop!\n")
