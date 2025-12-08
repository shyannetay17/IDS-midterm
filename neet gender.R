library(tidyverse)

setwd("/Users/margauxbonnaffoux/Desktop/data sets")
continents <- read_csv("continents-according-to-our-world-in-data.csv") %>%
  select(Entity, Code, Continent) %>%
  distinct() %>%
  rename(
    country_name = Entity,
    iso3c        = Code
  )

neet_total <- read_csv("youth-not-in-education-employment-training.csv") %>%
  rename(
    country_name = Entity,
    iso3c        = Code,
    year         = Year,
    neet_total   = `Share of youth not in education, employment or training, total (% of youth population)`
  )

neet_male_raw   <- read_csv("neet_male_worldbank.csv",   skip = 4) #skip bc dont contain data
neet_female_raw <- read_csv("neet_female_worldbank.csv", skip = 4)

wb_to_long <- function(df, value_name) {
  df %>%
    pivot_longer(
      cols      = matches("^[0-9]{4}$"),
      names_to  = "year",
      values_to = value_name
    ) %>%
    mutate(year = as.integer(year)) %>%
    rename(
      iso3c        = `Country Code`,
      country_name = `Country Name`
    ) %>%
    select(country_name, iso3c, year, all_of(value_name))
}

neet_male <- wb_to_long(neet_male_raw,   "neet_male")
neet_female <- wb_to_long(neet_female_raw, "neet_female")

#merge male + female + total NEET + continent 
neet_gender_full <- neet_total %>%
  full_join(neet_male,   by = c("iso3c", "country_name", "year")) %>%
  full_join(neet_female, by = c("iso3c", "country_name", "year")) %>%
  left_join(continents,  by = c("iso3c", "country_name"))

#keep only the 6 continents 
neet_gender_full <- neet_gender_full %>%
  filter(!is.na(Continent), Continent != "Antarctica")

#save the merged CSV
write_csv(neet_gender_full, "neet_gender_merged.csv")

#read base data
continents   <- read.csv("continents-according-to-our-world-in-data.csv")
youth_neet   <- read.csv("youth-not-in-education-employment-training.csv")


#we pick 2015 where the continent is always
#written in the table to make sure the continent is the same each year 
continent_lookup <- continents %>%
  filter(Year == 2015) %>%
  select(Code, Continent) %>%
  distinct()

#join NEET data with continent info
neet <- youth_neet %>%
  rename(
    Country    = Entity,
    neet_share = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.
  ) %>%
  left_join(continent_lookup, by = "Code") %>%
  filter(!is.na(Continent))    # keep only countries with a continent

#we add the gender specific NEET 
neet_male_raw   <- read.csv("neet_male_worldbank.csv",   skip = 4)
neet_female_raw <- read.csv("neet_female_worldbank.csv", skip = 4) #skip first four lines bc not data 

wb_to_long <- function(df, value_name) {
  df %>%
    pivot_longer(
      cols = matches("^X[0-9]{4}$"),   # X1960, X1961, ...
      names_to  = "Year",
      values_to = value_name
    ) %>%
    mutate(
      Year = as.integer(sub("^X", "", Year))
    ) %>%
    rename(
      Country = Country.Name,
      Code    = Country.Code
    ) %>%
    select(Country, Code, Year, all_of(value_name))
}

neet_male   <- wb_to_long(neet_male_raw,   "neet_male")
neet_female <- wb_to_long(neet_female_raw, "neet_female")

#join male and female NEET to total NEET + continent
neet_gender <- neet %>%
  left_join(neet_male,   by = c("Country", "Code", "Year")) %>%
  left_join(neet_female, by = c("Country", "Code", "Year"))

#summarise each year 
neet_continent_year <- neet_gender %>%
  group_by(Continent, Year) %>%
  summarise(
    neet_total  = mean(neet_share,  na.rm = TRUE),
    neet_male   = mean(neet_male,   na.rm = TRUE),
    neet_female = mean(neet_female, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Year >= 2015, Year <= 2020)

# save continent–year dataset
write.csv(neet_continent_year, "neet_continent_year_2015_2020.csv", row.names = FALSE)

#wide format: one row per continent, four columns
neet_2015_2020 <- neet_continent_year %>%
  filter(Year %in% c(2015, 2020)) %>%
  select(Continent, Year, neet_male, neet_female) %>%
  pivot_wider(
    names_from  = Year,
    values_from = c(neet_male, neet_female),
    names_glue  = "{.value}_{Year}"
  )

#data table to see numerical values for NEET for each continent in 2015 and 2020
continent_neet_table <- neet_2015_2020 %>%
  select(
    Continent,
    Boys_2015   = neet_male_2015,
    Girls_2015  = neet_female_2015,
    Boys_2020   = neet_male_2020,
    Girls_2020  = neet_female_2020
  )

print(continent_neet_table)

write.csv(continent_neet_table,
          "continent_neet_table_2015_2020.csv",
          row.names = FALSE)

#make the stacked bar chart
neet_long_stacked <- neet_2015_2020 %>%
  pivot_longer(
    cols = c(neet_male_2015, neet_male_2020,
             neet_female_2015, neet_female_2020),
    names_to = c("gender", "year"),
    names_pattern = "neet_(.*)_(.*)",
    values_to = "neet_level"
  ) %>%
  mutate(
    gender = if_else(gender == "male", "Boys", "Girls"),
    year   = as.character(year)
  )

ggplot(neet_long_stacked,
       aes(x = Continent, y = neet_level,
           fill = interaction(gender, year))) +
  geom_col() +
  scale_fill_manual(
    name = "Legend",
    values = c(
      "Boys.2015"  = "yellow",
      "Girls.2015" = "magenta",
      "Boys.2020"  = "cyan",
      "Girls.2020" = "violet"
    ),
    labels = c(
      "Boys 2015",
      "Girls 2015",
      "Boys 2020",
      "Girls 2020"
    )
  ) +
  labs(
    title = "Youth NEET levels by continent,\ngender and year (2015 vs 2020)",
    x = "Continent",
    y = "NEET (% of youth)"
  ) +
  theme_minimal()
