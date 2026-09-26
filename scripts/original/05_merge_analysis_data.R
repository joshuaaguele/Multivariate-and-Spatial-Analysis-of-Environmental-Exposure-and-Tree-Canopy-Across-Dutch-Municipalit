# 05_merge_analysis_data.R
# Purpose:
# Select socioeconomic indicators and merge them with the
# municipality-level environmental dataset.

# -------------------------------------------------------------------
# 1. Load packages
# -------------------------------------------------------------------

library(tidyverse)

# -------------------------------------------------------------------
# 2. Import cleaned intermediate datasets
# -------------------------------------------------------------------

environmental_data <- readr::read_csv(
  "tables/municipality_environmental_indicators.csv",
  show_col_types = FALSE
)

cbs_long <- readr::read_csv(
  "tables/cbs_municipality_long_2024.csv",
  show_col_types = FALSE
)

# -------------------------------------------------------------------
# 3. Verify imported datasets
# -------------------------------------------------------------------

print(dim(environmental_data))
print(names(environmental_data))

print(dim(cbs_long))
print(names(cbs_long))

# -------------------------------------------------------------------
# 4. Select and convert socioeconomic indicators
# -------------------------------------------------------------------

selected_measure_codes <- c(
  "M000222",  # Average standardised income
  "M008349",  # People in poverty
  "80200",    # People aged 65 years or older
  "D006842",  # Social-assistance recipients
  "M000100"   # Population density
)

selected_cbs_long <- cbs_long |>
  dplyr::filter(
    measure_code %in% selected_measure_codes
  ) |>
  dplyr::mutate(
    value_numeric = readr::parse_double(
      value,
      locale = readr::locale(decimal_mark = ","),
      na = c("", "NA")
    )
  )

selected_cbs_check <- selected_cbs_long |>
  dplyr::group_by(
    measure_code,
    indicator,
    unit
  ) |>
  dplyr::summarise(
    rows = dplyr::n(),
    municipalities =
      dplyr::n_distinct(municipality_code),
    numeric_values =
      sum(!is.na(value_numeric)),
    missing_values =
      sum(is.na(value_numeric)),
    .groups = "drop"
  )

print(
  selected_cbs_check,
  n = Inf,
  width = Inf
)

# -------------------------------------------------------------------
# 5. Reshape CBS indicators to one row per municipality
# -------------------------------------------------------------------

cbs_socioeconomic <- selected_cbs_long |>
  dplyr::select(
    municipality_code,
    municipality_name,
    measure_code,
    value_numeric
  ) |>
  tidyr::pivot_wider(
    names_from = measure_code,
    values_from = value_numeric
  ) |>
  dplyr::rename(
    age_65plus_count = `80200`,
    social_assistance_count = D006842,
    population_density_per_km2 = M000100,
    standardised_income_1000_eur = M000222,
    poverty_percent = M008349
  )

cbs_duplicate_check <- cbs_socioeconomic |>
  dplyr::count(municipality_code) |>
  dplyr::filter(n > 1)

print(dim(cbs_socioeconomic))
print(names(cbs_socioeconomic))
print(cbs_duplicate_check)

# -------------------------------------------------------------------
# 6. Check municipality-code matching before merging
# -------------------------------------------------------------------

environmental_not_in_cbs <- environmental_data |>
  dplyr::anti_join(
    cbs_socioeconomic,
    by = "municipality_code"
  )

cbs_not_in_environmental <- cbs_socioeconomic |>
  dplyr::anti_join(
    environmental_data,
    by = "municipality_code"
  )

municipality_match_check <- tibble::tibble(
  check = c(
    "Environmental municipalities",
    "CBS municipalities",
    "Environmental codes not in CBS",
    "CBS codes not in environmental"
  ),
  count = c(
    nrow(environmental_data),
    nrow(cbs_socioeconomic),
    nrow(environmental_not_in_cbs),
    nrow(cbs_not_in_environmental)
  )
)

print(municipality_match_check)

print(
  environmental_not_in_cbs,
  n = Inf,
  width = Inf
)

print(
  cbs_not_in_environmental,
  n = Inf,
  width = Inf
)

# -------------------------------------------------------------------
# 7. Merge environmental and socioeconomic datasets
# -------------------------------------------------------------------

analysis_data <- environmental_data |>
  dplyr::left_join(
    cbs_socioeconomic |>
      dplyr::select(-municipality_name),
    by = "municipality_code"
  ) |>
  dplyr::mutate(
    age_65plus_percent =
      100 * age_65plus_count /
      municipality_population,
    
    social_assistance_per_1000 =
      1000 * social_assistance_count /
      municipality_population
  )

print(dim(analysis_data))

summary(
  analysis_data |>
    dplyr::select(
      municipality_population,
      age_65plus_count,
      age_65plus_percent,
      social_assistance_count,
      social_assistance_per_1000,
      population_density_per_km2,
      standardised_income_1000_eur,
      poverty_percent
    )
)

# -------------------------------------------------------------------
# 8. Validate the merged analysis dataset
# -------------------------------------------------------------------

analysis_quality_check <- analysis_data |>
  dplyr::summarise(
    rows = dplyr::n(),
    municipalities =
      dplyr::n_distinct(municipality_code),
    duplicate_codes =
      dplyr::n() - dplyr::n_distinct(municipality_code),
    zero_population =
      sum(municipality_population <= 0, na.rm = TRUE),
    missing_canopy =
      sum(is.na(canopy_percent)),
    missing_drought =
      sum(is.na(drought_depth_m)),
    missing_heat =
      sum(is.na(heat_exposure_41plus_pct)),
    missing_flood =
      sum(is.na(flood_exposure_over50cm_pct)),
    missing_heavy_rain =
      sum(is.na(heavy_rain_exposure_20plus_pct)),
    missing_age =
      sum(is.na(age_65plus_percent)),
    missing_social_assistance =
      sum(is.na(social_assistance_per_1000)),
    missing_density =
      sum(is.na(population_density_per_km2)),
    missing_income =
      sum(is.na(standardised_income_1000_eur)),
    missing_poverty =
      sum(is.na(poverty_percent)),
    invalid_age_percent =
      sum(
        age_65plus_percent < 0 |
          age_65plus_percent > 100,
        na.rm = TRUE
      ),
    negative_social_assistance =
      sum(social_assistance_per_1000 < 0, na.rm = TRUE)
  )

highest_income_municipalities <- analysis_data |>
  dplyr::select(
    municipality_code,
    municipality_name,
    standardised_income_1000_eur,
    poverty_percent
  ) |>
  dplyr::arrange(
    dplyr::desc(standardised_income_1000_eur)
  ) |>
  dplyr::slice_head(n = 10)

print(analysis_quality_check, width = Inf)

print(
  highest_income_municipalities,
  n = 10,
  width = Inf
)