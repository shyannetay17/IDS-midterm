library(tidyverse)
# FILE PATHS
continents_file <- "/Users/julie/Desktop/IDS project/continents-according-to-our-world-in-data.csv"
neet_file <- "/Users/julie/Desktop/IDS project/youth-not-in-education-employment-training.csv"
youth_pop_file <- "/Users/julie/Desktop/IDS project/population-by-age-group.csv"
# LOAD DATA
continents <- read_csv(continents_file, show_col_types = FALSE)
neet <- read_csv(neet_file, show_col_types = FALSE)
youth_pop <- read_csv(youth_pop_file, show_col_types = FALSE)
# FILTER YOUTH POPULATION FOR AGES 15-24
youth_pop_filtered <- youth_pop %>%
  filter(Year %in% c(2015, 2020)) %>%
  rename(Youth_Population = `Population - Sex: all - Age: 15-24 - Variant: estimates`) %>%
  select(Entity, Code, Year, Youth_Population) %>%
  filter(!is.na(Code), !is.na(Youth_Population))
# PREPARE NEET DATA FOR 2015 AND 2020
neet_both_years <- neet %>%
  filter(Year %in% c(2015, 2020)) %>%
  rename(NEET = 4) %>%
  select(Entity, Code, Year, NEET) %>%
  filter(!is.na(Code), !is.na(NEET))
# JOIN WITH CONTINENTS
neet_with_continents <- neet_both_years %>%
  left_join(continents %>% select(Entity, Code, Continent) %>% distinct(), 
            by = c("Entity", "Code"))
# JOIN WITH YOUTH POPULATION
neet_complete <- neet_with_continents %>%
  left_join(youth_pop_filtered, by = c("Code", "Year"))
# CALCULATE POPULATION-WEIGHTED NEET BY CONTINENT AND YEAR
neet_weighted <- neet_complete %>%
  filter(!is.na(Continent), !is.na(Youth_Population)) %>%
  group_by(Continent, Year) %>%
  summarise(
    weighted_NEET = sum(NEET * Youth_Population) / sum(Youth_Population),
    n_countries = n(),
    .groups = "drop"
  ) %>%
  mutate(Year = factor(Year, levels = c("2020", "2015")))

# CREATE GROUPED BAR CHART
p_comparison <- ggplot(neet_weighted, aes(x = Continent, y = weighted_NEET, fill = Year)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("2015" = "#8a2999", "2020" = "#FDE724"),
                    name = "Year") +
  labs(
    title = "Population-Weighted Youth NEET Level by Continent",
    subtitle = "Comparison between 2015 and 2020",
    x = "Continent",
    y = "Weighted NEET Level (% of youth population)",
    caption = "Source: OWID / World Bank\nWeighted by youth population (ages 15-24)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    legend.position = "right"
  )
print(p_comparison)
# Save the plot
ggsave("/Users/julie/Desktop/IDS project/neet_weighted_comparison_2015_2020.png", 
       plot = p_comparison, width = 10, height = 6, dpi = 300)
cat("\nPlot saved to your desktop!\n")