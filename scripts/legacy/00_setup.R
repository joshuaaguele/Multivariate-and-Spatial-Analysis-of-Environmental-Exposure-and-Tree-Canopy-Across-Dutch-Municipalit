# Multivariate Analysis of Environmental and Socioeconomic
# Vulnerability Across Dutch Municipalities
# Project setup

getwd()

# Create project folders

dir.create("data_raw", showWarnings = FALSE)
dir.create("data_clean", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
dir.create("report", showWarnings = FALSE)
# Install packages for importing and preparing data

packages <- c("tidyverse", "janitor", "readxl")

new_packages <- packages[
  !(packages %in% installed.packages()[, "Package"])
]

if (length(new_packages) > 0) {
  install.packages(
    new_packages,
    repos = "https://cloud.r-project.org"
  )
}
# Load packages for the current R session

library(tidyverse)
library(janitor)
library(readxl)
# Assess availability of candidate indicators

candidate_availability <- municipality_long |>
  filter(measure_code %in% candidate_measures$measure_code) |>
  mutate(
    numeric_value = parse_number(
      value,
      locale = locale(decimal_mark = ",")
    )
  ) |>
  group_by(measure_code, indicator, unit) |>
  summarise(
    municipalities = n(),
    available_values = sum(!is.na(numeric_value)),
    missing_values = sum(is.na(numeric_value)),
    missing_percent = round(
      mean(is.na(numeric_value)) * 100,
      1
    ),
    .groups = "drop"
  )

print(candidate_availability, n = Inf)
selected_indicator_details <- dashboard_metadata |>
  dplyr::filter(
    indicator %in% c(
      "Boomkroonoppervlakte",
      "Extreem lage grondwaterstand",
      "Gevoelstemperatuur - AHN4",
      "Maximale overstromingsdiepte",
      "Waterdiepte bij hevige bui"
    )
  ) |>
  dplyr::select(
    indicator,
    attribuutnaam,
    eenheid,
    omschrijving_kort
  ) |>
  dplyr::arrange(indicator, attribuutnaam)

View(selected_indicator_details)