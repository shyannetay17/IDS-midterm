library(tidyverse)
library(ggplot2)
library(dplyr)
library(esquisse)

setwd("C:/EFDS/Data science/group project/data sets/")

#GDP growth histogram and density plot creation

#Opening the data sets
GDP_capita <- read.csv("gdp-per-capita-worldbank.csv")
continents <- read.csv("continents-according-to-our-world-in-data.csv")

#renaming the columns for easier use
colnames(GDP_capita) <- c("Entity", "Code", "Year", "GDP_per_capita")
colnames(youth) <- c("Entity", "Code", "Year", "Share")

# removing year column from the data set
continents <- continents %>%
  select(Entity, Code, Continent)

# filtering in the data only in our time frame and joining the data sets
GDP_analysis <- GDP_capita %>%
  left_join(continents, by = c("Entity", "Code"))

# Clean data from columns with NA in continents
GDP_analysis1 <- GDP_analysis %>%
  filter(!Entity %in% c("World",
                        "European Union (27)",
                        "East Asia and Pacific (WB)",
                        "Europe and Central Asia (WB)",
                        "Latin America and Caribbean (WB)",
                        "Middle East and North Africa (WB)",
                        "North America (WB)",
                        "South Asia (WB)",
                        "Sub-Saharan Africa (WB)",
                        "High-income countries",
                        "Low-income countries",
                        "Lower-middle-income countries",
                        "Middle-income countries",
                        "Upper-middle-income countries"))

# Computing GDP per capita growth
GDP_growth <- GDP_analysis1 %>%
  select(Entity, Year, GDP_per_capita, Continent) %>%
  filter(Year %in% c(2010, 2020)) %>%
  pivot_wider(names_from = Year, values_from = GDP_per_capita) %>%
  mutate(
    growth_rate = (`2020` - `2010`) / `2010` * 100
  )

# filter out NA values from growth rate
GDP_growth <- GDP_growth %>%
  filter(is.finite(growth_rate))

# plotting the Histogram
ggplot(GDP_growth, aes(x = growth_rate, fill = Continent)) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  labs(
    title = "Distribution of GDP per capita Growth by continent (2010–2020)",
    x = "GDP per capita Growth (%)",
    y = "Number of countries"
  ) +
  theme_minimal()

ggplot(GDP_growth, aes(x = growth_rate, fill = Continent)) +
  geom_histogram(alpha = 0.7, bins = 30) +
  facet_wrap(~Continent) +
  labs(
    title = "Distribution of GDP per capita Growth by continent (2010–2020)",
    x = "GDP per capita Growth (%)",
    y = "Number of countries"
  ) +
  theme_minimal()

# creating a list for least developed countries
ldc_list <- c(
  "Afghanistan","Angola","Bangladesh","Benin",
  "Burkina Faso","Burundi","Cambodia","Central African Republic","Chad",
  "Comoros","Democratic Republic of the Congo",
  "Djibouti","Eritrea","Ethiopia","Gambia","Guinea",
  "Guinea-Bissau","Haiti","Kiribati","Lao People's Democratic Republic",
  "Lesotho","Liberia","Madagascar","Malawi","Mali","Mauritania","Mozambique",
  "Myanmar","Nepal","Niger","Rwanda","Senegal","Sierra Leone","Solomon Islands",
  "Somalia","South Sudan","Sudan","Timor-Leste","Togo","Tuvalu","Uganda",
  "United Republic of Tanzania", "Yemen","Zambia"
)

#creating a data set for LDC
LDC_analysis <- GDP_analysis1 %>%
  filter(Entity %in% ldc_list)

# computing growth values for LDC
LDC_growth <- LDC_analysis %>%
  select(Entity, Year, GDP_per_capita, Continent) %>%
  filter(Year %in% c(2010, 2020)) %>%
  pivot_wider(names_from = Year, values_from = GDP_per_capita) %>%
  mutate(
    growth_rate = (`2020` - `2010`) / `2010` * 100
  )

#calculating target GDP growth rate over the decade for LDC
target <- ((1 + 0.07)^10 - 1)* 100  

# filter out NA values from growth rate
LDC_growth <- LDC_growth %>%
  filter(is.finite(growth_rate))

ggplot(LDC_growth, aes(x = growth_rate, fill = Continent)) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  geom_vline(xintercept = target, linetype = "dashed", color = "black") +
  labs(
    title = "Distribution of GDP per capita Growth by continent (2010–2020)",
    subtitle = paste0("Dashed line = UN target (≈", round(target, 1), 
                      "% cumulative growth over decade)"),
    x = "GDP per capita Growth (%)",
    y = "Number of countries"
  ) +
  theme_minimal()

ggplot(LDC_growth, aes(x = growth_rate, fill = Continent)) +
  geom_histogram(alpha = 0.7, bins = 30) +
  facet_wrap(~Continent) +
  geom_vline(xintercept = target, linetype = "dashed", color = "black") +
  labs(
    title = "GDP per Capita Growth Distribution (2010–2020) by Continent",
    subtitle = paste0("Dashed line = UN target (≈", round(target, 1), 
                      "% cumulative growth over decade)"),
    x = "GDP per Capita Growth (%)",
    y = "Number of Countries"
  ) +
  theme_minimal()

ggplot(LDC_growth, aes(x = growth_rate, fill = Continent)) +
  geom_histogram(alpha = 0.7, bins = 30) +
  facet_wrap(~Continent) +
  geom_vline(xintercept = target, linetype = "dashed", color = "black") +
  labs(
    title = "Distribution of GDP per capita Growth by continent (2010–2020)",
    subtitle = paste0("Dashed line = UN target (≈", round(target, 1), 
                      "% cumulative growth over decade)"),
    x = "GDP per capita Growth (%)",
    y = "Number of countries"
  ) +
  theme_minimal()

# annualised approach
LDC_growth_annual <- LDC_analysis %>%
  filter(Year %in% c(2010, 2020)) %>%
  pivot_wider(names_from = Year, values_from = GDP_per_capita) %>%
  mutate(
    CAGR = ((`2020` / `2010`)^(1/10) - 1) * 100
  )


target1 <- 7  # UN target = 7% annual growth

#plotting the histogram
ggplot(LDC_growth, aes(x = CAGR, fill = Continent)) +
  geom_histogram(alpha = 0.7, bins = 30) +
  facet_wrap(~Continent) +
  geom_vline(xintercept = target1, linetype = "dashed", color = "black") +
  labs(
    title = "Annualised GDP per Capita Growth (2010–2020) in LDCs",
    subtitle = "Dashed line = UN target (7% annual growth)",
    x = "Annualised Growth Rate (%)",
    y = "Number of LDCs"
  ) +
  theme_minimal()

# removing NA values
LDC_growth <- LDC_growth %>%
  filter(!is.na(CAGR), is.finite(CAGR))

#removing continents with just one country
LDC_growth_density <- LDC_growth_annual %>%
  filter(Entity != "Haiti")
LDC_growth_density1 <- LDC_growth %>%
  filter(Entity != "Haiti")

#density plot for GDP per capita growth cumulative approach World
ggplot(GDP_growth, aes(x = growth_rate, 
                                fill = Continent, color = Continent)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~Continent) +
  labs(
    title = "Density of Cumulative GDP per Capita Growth (2010–2020)",
    x = "Cumulative Growth Rate (%)",
    y = "Density"
  ) +
  theme_minimal()

# density plot for GDP per capita growth cumulative approach LDC
ggplot(LDC_growth_density1, aes(x = growth_rate, 
                       fill = Continent, color = Continent)) +
  geom_density(alpha = 0.3) +
  geom_vline(xintercept = target, linetype = "dashed", color = "black") +
  labs(
    title = "Density of Cumulative GDP per Capita Growth (2010–2020) in LDCs",
    subtitle = paste0("Dashed line = UN target (≈", round(target, 1), 
                      "% cumulative growth over decade)"),
    x = "Cumulative Growth Rate (%)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(LDC_growth_density1, aes(x = growth_rate, 
                                fill = Continent, color = Continent)) +
  geom_density(alpha = 0.3) +
  geom_vline(xintercept = target, linetype = "dashed", color = "black") +
  facet_wrap(~Continent) +
  labs(
    title = "Density of Cumulative GDP per Capita Growth (2010–2020) in LDCs",
    subtitle = paste0("Dashed line = UN target (≈", round(target, 1), 
                      "% cumulative growth over decade)"),
    x = "Cumulative Growth Rate (%)",
    y = "Density"
  ) +
  theme_minimal()

# density plot for GDP per capita growth annual approach
ggplot(LDC_growth_density, aes(x = CAGR, fill = Continent, color = Continent)) +
  geom_density(alpha = 0.3) +
  geom_vline(xintercept = 7, linetype = "dashed", color = "black") +
  labs(
    title = "Density of Annualized GDP per Capita Growth (2010–2020) in LDCs",
    subtitle = "Dashed line = UN target (7% annual growth)",
    x = "Annualized Growth Rate (%)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(LDC_growth_density, aes(x = CAGR, fill = Continent, color = Continent)) +
  geom_density(alpha = 0.3) +
  geom_vline(xintercept = 7, linetype = "dashed", color = "black") +
  facet_wrap(~Continent) +
  labs(
    title = "Density of Annualized GDP per Capita Growth (2010–2020) in LDCs",
    subtitle = "Dashed line = UN target (7% annual growth)",
    x = "Annualized Growth Rate (%)",
    y = "Density"
  ) +
  theme_minimal()

# NEET Histogram and density plot computation

youth <- read.csv("youth-not-in-education-employment-training.csv")

view(youth)

#renaming the columns for easier use
colnames(youth) <- c("Entity", "Code", "Year", "Share")

# Removing any possible NA in share column and converting to a numeric value
youth <- youth %>%
  filter(!is.na(Share)) %>%
  mutate(Share = as.numeric(Share))

#joining two datasets
youth <- youth %>%
  left_join(continents, by = c("Entity", "Code"))

# Removing any regions from the dataset
youth1 <- youth %>%
  filter(!is.na(Continent))

# FOcusing on the chosen time frame
youth1 <- youth1 %>%
  filter(Year >= 2010, Year <= 2020)     

#Plotting it on a density plot
ggplot(youth1, aes(x = Share, fill = Continent, color = Continent)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~Continent) +
  labs(
    title = "Density of Youth NEET Rates by Continent (2010-2020)",
    x = "NEET Share (%)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(youth1, aes(x = Share, fill = Continent, color = Continent)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Density of Youth NEET Rates by Continent (2010-2020)",
    x = "NEET Share (%)",
    y = "Density"
  ) +
  theme_minimal()

# Density overtime
ggplot(youth1, aes(x = Share, fill = factor(Year), color = factor(Year))) +
  geom_density(alpha = 0.3) +
  facet_wrap(~Continent) +
  labs(
    title = "Youth NEET Rate Distributions: 2010 vs 2020",
    x = "NEET Share (%)",
    y = "Density",
    fill = "Year",
    color = "Year"
  ) +
  theme_minimal()

# Histogram
ggplot(youth1, aes(x = Share, fill = Continent)) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  labs(
    title = "Distribution of NEET by continent (2010–2020)",
    x = "NEET Share (%)",
    y = "Number of countries"
  ) +
  theme_minimal()

ggplot(GDP_growth, aes(x = growth_rate, fill = Continent)) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  facet_wrap(~Continent) +
  labs(
    title = "Distribution of NEET by continent (2010–2020)",
    x = "NEET Share (%)",
    y = "Number of countries"
  ) +
  theme_minimal()

# creating a data set for LDC countries
youth_LDC <- youth1 %>%
  filter(Entity %in% ldc_list)

# Histogram
ggplot(youth_LDC, aes(x = Share, fill = Continent)) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  labs(
    title = "Distribution of NEET for LDCs by continent (2010–2020)",
    x = "NEET Share (%)",
    y = "Number of countries"
  ) +
  theme_minimal()

ggplot(youth_LDC, aes(x = Share, fill = Continent)) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  facet_wrap(~Continent) +
  labs(
    title = "Distribution of NEET for LDCs by continent (2010–2020)",
    x = "NEET Share (%)",
    y = "Number of countries"
  ) +
  theme_minimal()

#removing continents with just one country
youth_LDC_density <- youth_LDC %>%
  filter(Entity != "Haiti")
LDC_growth_density1 <- LDC_growth %>%
  filter(Entity != "Haiti")

# Density plot
ggplot(youth_LDC_density, aes(x = Share, fill = Continent, color = Continent)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~Continent) +
  labs(
    title = "Density of Youth NEET Rates for LDCs by Continent (2010-2020)",
    x = "NEET Share (%)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(youth_LDC_density, aes(x = Share, fill = Continent, color = Continent)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Density of Youth NEET Rates by Continent (2010-2020)",
    x = "NEET Share (%)",
    y = "Density"
  ) +
  theme_minimal()

# COMPARISON

GDP_capita1 <- GDP_capita %>%
  arrange(Entity, Year) %>% 
  group_by(Entity) %>%
  mutate(
    annual_growth = (GDP_per_capita - lag(GDP_per_capita)) / 
      lag(GDP_per_capita) * 100
  ) %>%
  ungroup() %>%
 filter(!is.na(annual_growth))


compare <- youth1 %>%
  left_join(GDP_capita1, by = c("Entity", "Code", "Year"))

compare <- compare %>% 
  filter(!is.na(annual_growth))

view(compare)

ggplot(compare, aes(x = annual_growth, y = Share, group = Year)) +
  geom_point(alpha = 0.6) +
  geom_path(alpha = 0.4) +
  facet_wrap(~Continent) +
  labs(
    title = "Relationship Between GDP Growth and Youth NEET Rates (2010–2020)",
    x = "Annual GDP Growth (%)",
    y = "Youth NEET Share (%)"
  ) +
  theme_minimal()

compare <- compare %>%
  group_by(Continent) %>%
  mutate(
    avg_share_continent  = mean(Share, na.rm = TRUE),
    avg_growth_continent = mean(annual_growth, na.rm = TRUE)
  ) %>%
  ungroup()
