# Import CBS municipality data for 2024

observations <- read_csv(
  "data_raw/85984NED_2024/Observations.csv",
  show_col_types = FALSE
)

region_codes <- read_csv(
  "data_raw/85984NED_2024/WijkenEnBuurtenCodes.csv",
  show_col_types = FALSE
)

measure_codes <- read_csv(
  "data_raw/85984NED_2024/MeasureCodes.csv",
  show_col_types = FALSE
)

# Inspect the structure of each table
# -------------------------------------------------------------------
# Diagnose CBS parsing issues
# -------------------------------------------------------------------

print(dim(municipality_long))

print(
  dplyr::n_distinct(
    municipality_long$municipality_code
  )
)

cbs_parsing_problems <- readr::problems(observations)

print(nrow(cbs_parsing_problems))

print(
  cbs_parsing_problems,
  n = 20,
  width = Inf
)

# Import CBS municipality data for 2024

observations <- read_delim(
  "data_raw/85984NED_2024/Observations.csv",
  delim = ";",
  show_col_types = FALSE,
  trim_ws = TRUE
)

region_codes <- read_delim(
  "data_raw/85984NED_2024/WijkenEnBuurtenCodes.csv",
  delim = ";",
  show_col_types = FALSE,
  trim_ws = TRUE
)

measure_codes <- read_delim(
  "data_raw/85984NED_2024/MeasureCodes.csv",
  delim = ";",
  show_col_types = FALSE,
  trim_ws = TRUE
)

# Inspect the corrected tables

glimpse(observations)
glimpse(region_codes)
glimpse(measure_codes)
# Examine regional identifiers before filtering

names(region_codes)

region_codes |>
  select(Identifier, Title, Description, DetailRegionCode) |>
  slice_head(n = 12)
# Keep only Dutch municipalities

municipality_codes <- region_codes |>
  filter(str_starts(Identifier, "GM")) |>
  transmute(
    municipality_code = Identifier,
    municipality_name = Title
  )

# Check the number of municipalities

nrow(municipality_codes)

head(municipality_codes)
# Create a catalogue of all available CBS indicators

measure_catalogue <- measure_codes |>
  transmute(
    measure_code = Identifier,
    indicator = Title,
    description = Description,
    data_type = DataType,
    unit = Unit,
    decimals = Decimals
  )

View(measure_catalogue)

write_csv(
  measure_catalogue,
  "tables/cbs_measure_catalogue_2024.csv"
)
# Search the CBS catalogue for vulnerability-related indicators

candidate_measures <- measure_catalogue |>
  filter(
    str_detect(
      indicator,
      regex(
        "65 jaar|inkomen|armoede|bijstand|opleiding|dichtheid|oppervlakte|werkloos",
        ignore_case = TRUE
      )
    )
  ) |>
  select(measure_code, indicator, unit)
print(candidate_measures, n = Inf)

print(candidate_measures, n = Inf)
candidate_measures |>
  slice_head(n = 10)
# Create municipality-level data in long format

measure_lookup <- measure_codes |>
  transmute(
    measure_code = Identifier,
    indicator = Title,
    unit = Unit
  )

municipality_long <- observations |>
  filter(
    WijkenEnBuurten %in% municipality_codes$municipality_code
  ) |>
  transmute(
    municipality_code = WijkenEnBuurten,
    measure_code = Measure,
    value = Value,
    string_value = StringValue,
    value_attribute = ValueAttribute
  ) |>
  left_join(
    municipality_codes,
    by = "municipality_code"
  ) |>
  left_join(
    measure_lookup,
    by = "measure_code"
  )

# Validate the result

nrow(municipality_long)

n_distinct(municipality_long$municipality_code)

glimpse(municipality_long)
# Assess availability across all municipalities

total_municipalities <- nrow(municipality_codes)

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
    municipalities_recorded = n_distinct(municipality_code),
    available_values = n_distinct(
      municipality_code[!is.na(numeric_value)]
    ),
    .groups = "drop"
  ) |>
  mutate(
    missing_values = total_municipalities - available_values,
    missing_percent = round(
      missing_values / total_municipalities * 100,
      1
    )
  ) |>
  arrange(desc(missing_percent))

View(candidate_availability)
# Display indicators with incomplete municipality coverage

candidate_availability |>
  filter(missing_values > 0) |>
  select(
    measure_code,
    indicator,
    available_values,
    missing_values,
    missing_percent
  )
write_csv(
  candidate_availability,
  "tables/cbs_candidate_availability_2024.csv"
)

# -------------------------------------------------------------------
# Diagnose CBS parsing issues
# -------------------------------------------------------------------

print(dim(municipality_long))

print(
  dplyr::n_distinct(
    municipality_long$municipality_code
  )
)

cbs_parsing_problems <- readr::problems(observations)

print(nrow(cbs_parsing_problems))

print(
  cbs_parsing_problems,
  n = 20,
  width = Inf
)

# -------------------------------------------------------------------
# Save cleaned municipality-level CBS data
# -------------------------------------------------------------------

readr::write_csv(
  municipality_long,
  "tables/cbs_municipality_long_2024.csv",
  na = ""
)