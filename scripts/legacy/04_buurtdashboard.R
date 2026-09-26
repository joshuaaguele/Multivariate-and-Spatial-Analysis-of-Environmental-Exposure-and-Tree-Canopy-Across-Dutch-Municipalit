# 04_buurtdashboard.R
# Purpose:
# Inspect and document neighbourhood-level environmental data from the
# Dutch Climate Impact Atlas Buurtdashboard workbook.
#
# Source: Klimaateffectatlas Buurtdashboard
# Raw workbook: Downloadbuurtdashboard_maa26-2.xlsx

# -------------------------------------------------------------------
# 1. Load packages
# -------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)

# -------------------------------------------------------------------
# 2. Define and verify the raw-data file
# -------------------------------------------------------------------

dashboard_path <- file.path(
  "data_raw",
  "Downloadbuurtdashboard_maa26-2.xlsx"
)

stopifnot(file.exists(dashboard_path))

# -------------------------------------------------------------------
# 3. Inspect workbook structure
# -------------------------------------------------------------------

dashboard_sheets <- readxl::excel_sheets(dashboard_path)

print(dashboard_sheets)

# Expected worksheets:
# "Voorblad", "Metadata", "Data"

# -------------------------------------------------------------------
# 4. Import and clean the metadata
# -------------------------------------------------------------------

dashboard_metadata <- readxl::read_excel(
  dashboard_path,
  sheet = "Metadata"
) |>
  janitor::clean_names()

print(dim(dashboard_metadata))
print(names(dashboard_metadata))

# Expected dimensions for this workbook version:
# 87 rows and 9 columns

stopifnot(
  nrow(dashboard_metadata) == 87,
  ncol(dashboard_metadata) == 9
)

# Save a reproducible copy of the cleaned metadata
readr::write_csv(
  dashboard_metadata,
  "tables/buurtdashboard_metadata.csv"
)

# -------------------------------------------------------------------
# 5. Preview the main data sheet
# -------------------------------------------------------------------

# Read only five rows initially because the workbook is relatively large.

dashboard_preview <- readxl::read_excel(
  dashboard_path,
  sheet = "Data",
  n_max = 5
) |>
  janitor::clean_names()

print(dim(dashboard_preview))
print(names(dashboard_preview)[1:20])

# This workbook version contains 227 data columns.
stopifnot(ncol(dashboard_preview) == 227)

# -------------------------------------------------------------------
# 6. Broad search for climate-related metadata
# -------------------------------------------------------------------

climate_candidates <- dashboard_metadata |>
  dplyr::filter(
    dplyr::if_any(
      dplyr::everything(),
      ~ stringr::str_detect(
        stringr::str_to_lower(as.character(.x)),
        "boom|groen|hitte|warm|droog|waterdiepte|overstrom|neerslag|regen"
      )
    )
  ) |>
  dplyr::select(
    indicator,
    categorie,
    attribuutnaam,
    eenheid,
    omschrijving_kort
  )

print(climate_candidates, n = Inf)

# -------------------------------------------------------------------
# 7. Search specifically within official indicator names
# -------------------------------------------------------------------

climate_indicator_matches <- dashboard_metadata |>
  dplyr::filter(
    stringr::str_detect(
      stringr::str_to_lower(indicator),
      "boom|groen|hitte|temperatuur|droog|waterdiepte|overstrom|neerslag|regen"
    )
  ) |>
  dplyr::select(
    indicator,
    categorie,
    attribuutnaam,
    eenheid
  ) |>
  dplyr::arrange(indicator, attribuutnaam)

print(climate_indicator_matches, n = Inf)

print(
  sort(unique(climate_indicator_matches$indicator))
)

# -------------------------------------------------------------------
# 8. Search for possible drought indicators
# -------------------------------------------------------------------

drought_candidates <- dashboard_metadata |>
  dplyr::filter(
    dplyr::if_any(
      dplyr::everything(),
      ~ stringr::str_detect(
        stringr::str_to_lower(as.character(.x)),
        "droog|grondwater|bodemvocht|verdroging|laagste grondwater"
      )
    )
  ) |>
  dplyr::select(
    indicator,
    categorie,
    attribuutnaam,
    eenheid,
    omschrijving_kort
  ) |>
  dplyr::distinct()

print(drought_candidates, n = Inf, width = Inf)

# Selected drought-exposure proxy:
# Extreem lage grondwaterstand, attribute LG3MaxHuidig.
#
# Interpretation:
# Current groundwater level during an extremely dry year, measured in
# metres below ground level. Larger values represent deeper groundwater
# and potentially greater drought exposure.
#
# Source identified in the metadata:
# Klimaateffectatlas / Nationaal Water Model (2019).
#
# Important limitation:
# This is a modelled, indicative groundwater statistic rather than a
# direct observation of drought damage.

drought_choice <- dashboard_metadata |>
  dplyr::filter(
    indicator == "Extreem lage grondwaterstand"
  )

print(
  t(as.data.frame(drought_choice)),
  quote = FALSE
)

# -------------------------------------------------------------------
# 9. Inspect the five proposed environmental dimensions
# -------------------------------------------------------------------

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

print(
  selected_indicator_details,
  n = Inf,
  width = Inf
)

# Save the candidate-variable documentation
readr::write_csv(
  selected_indicator_details,
  "tables/buurtdashboard_selected_indicator_details.csv"
)

# -------------------------------------------------------------------
# 10. Display the available heat categories
# -------------------------------------------------------------------

heat_options <- selected_indicator_details |>
  dplyr::filter(
    indicator == "Gevoelstemperatuur - AHN4"
  ) |>
  dplyr::select(
    attribuutnaam,
    eenheid,
    omschrijving_kort
  )

print(
  heat_options,
  n = Inf,
  width = Inf
)
# -------------------------------------------------------------------
# 11. Define the proposed heat-exposure measure
# -------------------------------------------------------------------

# Heat exposure will be calculated as the combined percentage of area
# in the following apparent-temperature categories:
# 41–45°C, 46–50°C, and 51°C or higher.

heat_attributes <- c(
  "pet41tm45p",
  "pet46tm50p",
  "pet51plusp"
)

# -------------------------------------------------------------------
# 13. Inspect the definition and source of the heavy-rain measure
# -------------------------------------------------------------------

heavy_rain_definition <- dashboard_metadata |>
  dplyr::filter(
    indicator == "Waterdiepte bij hevige bui"
  ) |>
  dplyr::select(
    bron,
    omschrijving_lang
  ) |>
  dplyr::distinct()

print(
  t(as.data.frame(heavy_rain_definition)),
  quote = FALSE
)
# -------------------------------------------------------------------
# 14. Define and verify the final environmental columns
# -------------------------------------------------------------------

# Tree-canopy protection: latest AHN5 version, whole neighbourhood
canopy_attribute <- "bk_bgraad_tot_perc_land_ahn4"

# Drought exposure: extreme low groundwater in the current climate
drought_attribute <- "lg3max_huidig"

# Heat exposure: apparent temperature of at least 41°C, AHN4
heat_attributes <- c(
  "pet41tm45p_ahn4",
  "pet46tm50p_ahn4",
  "pet51plusp_ahn4"
)

# Flood exposure: maximum depth greater than 50 cm during an
# extremely low-probability flood event
flood_attributes <- c(
  "p_50tot200cm",
  "p_200pluscm"
)

# Heavy-rain exposure: water depth of at least 20 cm
# Note: column names use "mm", but metadata descriptions use centimetres.
heavy_rain_attributes <- c(
  "perc20_30mm",
  "perc30mm_me"
)

selected_environmental_columns <- c(
  canopy_attribute,
  drought_attribute,
  heat_attributes,
  flood_attributes,
  heavy_rain_attributes
)

missing_environmental_columns <- setdiff(
  selected_environmental_columns,
  names(dashboard_preview)
)

print(selected_environmental_columns)
print(missing_environmental_columns)

stopifnot(length(missing_environmental_columns) == 0)
# -------------------------------------------------------------------
# 15. Find the actual environmental column names in the Data sheet
# -------------------------------------------------------------------

possible_environmental_names <- names(dashboard_preview)[
  stringr::str_detect(
    names(dashboard_preview),
    "bkb|boom|graad|pet|gevoel|perc.*20|perc.*30|lg3|p_50|p_200"
  )
]

print(possible_environmental_names)
# Display tree-canopy column names separately.

canopy_data_names <- names(dashboard_preview)[
  stringr::str_detect(
    names(dashboard_preview),
    "bkb|boom|graad"
  )
]

print(canopy_data_names)

# Display heavy-rain column names separately.

heavy_rain_data_names <- names(dashboard_preview)[
  stringr::str_detect(
    names(dashboard_preview),
    "^perc(5|10|15|20|30|nodata)"
  )
]

print(heavy_rain_data_names)
print(canopy_data_names)
# -------------------------------------------------------------------
# 16. Inspect the complete tree-canopy metadata
# -------------------------------------------------------------------

canopy_definition <- dashboard_metadata |>
  dplyr::filter(
    indicator == "Boomkroonoppervlakte"
  )

print(
  t(as.data.frame(canopy_definition)),
  quote = FALSE
)
# -------------------------------------------------------------------
# 17. Import the complete Buurtdashboard data
# -------------------------------------------------------------------

dashboard_data <- readxl::read_excel(
  dashboard_path,
  sheet = "Data",
  na = c("", "NA", "NVT", "nvt"),
  guess_max = 10000
) |>
  janitor::clean_names()

print(dim(dashboard_data))
print(object.size(dashboard_data), units = "MB")
# -------------------------------------------------------------------
# 18. Create the neighbourhood-level environmental working dataset
# -------------------------------------------------------------------

identifier_columns <- c(
  "objectid",
  "buurtcode2024",
  "buurtnaam",
  "gemeentecode",
  "gemeentenaam",
  "aant_inw"
)

missing_identifier_columns <- setdiff(
  identifier_columns,
  names(dashboard_data)
)

print(missing_identifier_columns)

stopifnot(length(missing_identifier_columns) == 0)

environmental_neighbourhood <- dashboard_data |>
  dplyr::select(
    dplyr::all_of(identifier_columns),
    dplyr::all_of(selected_environmental_columns)
  )

environmental_structure <- environmental_neighbourhood |>
  dplyr::summarise(
    rows = dplyr::n(),
    neighbourhoods = dplyr::n_distinct(
      buurtcode2024,
      na.rm = TRUE
    ),
    municipalities = dplyr::n_distinct(
      gemeentecode,
      na.rm = TRUE
    ),
    missing_neighbourhood_codes = sum(is.na(buurtcode2024)),
    missing_municipality_codes = sum(is.na(gemeentecode))
  )

print(environmental_structure)
dplyr::glimpse(environmental_neighbourhood)
# -------------------------------------------------------------------
# 19. Diagnose negative missing-value codes
# -------------------------------------------------------------------

print(environmental_structure)

negative_value_summary <- environmental_neighbourhood |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(selected_environmental_columns),
      ~ sum(.x < 0, na.rm = TRUE)
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variable",
    values_to = "negative_values"
  )

print(
  negative_value_summary,
  n = Inf
)
# -------------------------------------------------------------------
# 20. Compare tree-canopy coverage across AHN versions
# -------------------------------------------------------------------

canopy_version_columns <- c(
  "bk_bgraad_tot_perc_land_ahn3",
  "bk_bgraad_tot_perc_land_ahn4",
  "bk_bgraad_tot_perc_land_ahn5"
)

canopy_version_availability <- dashboard_data |>
  dplyr::select(
    dplyr::all_of(canopy_version_columns)
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "canopy_version",
    values_to = "value"
  ) |>
  dplyr::group_by(canopy_version) |>
  dplyr::summarise(
    total_rows = dplyr::n(),
    valid_values = sum(!is.na(value) & value >= 0),
    negative_codes = sum(value < 0, na.rm = TRUE),
    ordinary_missing = sum(is.na(value)),
    coverage_percent = round(
      mean(!is.na(value) & value >= 0) * 100,
      1
    ),
    .groups = "drop"
  )

print(canopy_version_availability)
# -------------------------------------------------------------------
# 21. Replace negative missing-value codes with NA
# -------------------------------------------------------------------

environmental_neighbourhood_clean <- environmental_neighbourhood |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(selected_environmental_columns),
      ~ dplyr::if_else(.x < 0, NA_real_, .x)
    )
  )

remaining_negative_values <- environmental_neighbourhood_clean |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(selected_environmental_columns),
      ~ sum(.x < 0, na.rm = TRUE)
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variable",
    values_to = "negative_values"
  )

print(
  remaining_negative_values,
  n = Inf
)
# -------------------------------------------------------------------
# 22. Clean and inspect the neighbourhood population variable
# -------------------------------------------------------------------

environmental_neighbourhood_clean <-
  environmental_neighbourhood_clean |>
  dplyr::mutate(
    population = as.numeric(aant_inw)
  )

population_check <- environmental_neighbourhood_clean |>
  dplyr::summarise(
    total_rows = dplyr::n(),
    original_missing = sum(is.na(aant_inw)),
    numeric_missing = sum(is.na(population)),
    zero_population = sum(population == 0, na.rm = TRUE),
    negative_population = sum(population < 0, na.rm = TRUE),
    minimum_population = min(population, na.rm = TRUE),
    maximum_population = max(population, na.rm = TRUE)
  )

print(population_check)
print(population_check, width = Inf)
# -------------------------------------------------------------------
# 23. Create the five neighbourhood environmental indicators
# -------------------------------------------------------------------

environmental_indicators <- environmental_neighbourhood_clean |>
  dplyr::mutate(
    canopy_percent = bk_bgraad_tot_perc_land_ahn4,
    
    drought_depth_m = lg3max_huidig,
    
    heat_exposure_41plus = dplyr::if_else(
      dplyr::if_all(
        dplyr::all_of(c(
          "pet41tm45p_ahn4",
          "pet46tm50p_ahn4",
          "pet51plusp_ahn4"
        )),
        is.na
      ),
      NA_real_,
      rowSums(
        dplyr::pick(
          dplyr::all_of(c(
            "pet41tm45p_ahn4",
            "pet46tm50p_ahn4",
            "pet51plusp_ahn4"
          ))
        ),
        na.rm = TRUE
      )
    ),
    
    flood_exposure_over50cm = dplyr::if_else(
      dplyr::if_all(
        dplyr::all_of(c(
          "p_50tot200cm",
          "p_200pluscm"
        )),
        is.na
      ),
      NA_real_,
      rowSums(
        dplyr::pick(
          dplyr::all_of(c(
            "p_50tot200cm",
            "p_200pluscm"
          ))
        ),
        na.rm = TRUE
      )
    ),
    
    heavy_rain_exposure_20plus = dplyr::if_else(
      dplyr::if_all(
        dplyr::all_of(c(
          "perc20_30mm",
          "perc30mm_me"
        )),
        is.na
      ),
      NA_real_,
      rowSums(
        dplyr::pick(
          dplyr::all_of(c(
            "perc20_30mm",
            "perc30mm_me"
          ))
        ),
        na.rm = TRUE
      )
    )
  )

environmental_indicator_names <- c(
  "canopy_percent",
  "drought_depth_m",
  "heat_exposure_41plus",
  "flood_exposure_over50cm",
  "heavy_rain_exposure_20plus"
)

indicator_summary <- environmental_indicators |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(environmental_indicator_names),
      list(
        missing = ~ sum(is.na(.x)),
        minimum = ~ min(.x, na.rm = TRUE),
        maximum = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE)
      )
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c("indicator", ".value"),
    names_pattern = "^(.*)_(missing|minimum|maximum|mean)$"
  )

print(indicator_summary, n = Inf)
# -------------------------------------------------------------------
# 24. Inspect indicator distributions and possible scale differences
# -------------------------------------------------------------------

indicator_quantiles <- environmental_indicators |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(environmental_indicator_names),
      list(
        p00 = ~ quantile(.x, 0.00, na.rm = TRUE),
        p25 = ~ quantile(.x, 0.25, na.rm = TRUE),
        p50 = ~ quantile(.x, 0.50, na.rm = TRUE),
        p75 = ~ quantile(.x, 0.75, na.rm = TRUE),
        p95 = ~ quantile(.x, 0.95, na.rm = TRUE),
        p99 = ~ quantile(.x, 0.99, na.rm = TRUE),
        p100 = ~ quantile(.x, 1.00, na.rm = TRUE)
      )
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c("indicator", ".value"),
    names_pattern = "^(.*)_(p00|p25|p50|p75|p95|p99|p100)$"
  )

print(indicator_quantiles, n = Inf, width = Inf)

highest_drought_values <- environmental_indicators |>
  dplyr::select(
    buurtcode2024,
    buurtnaam,
    gemeentenaam,
    drought_depth_m
  ) |>
  dplyr::arrange(dplyr::desc(drought_depth_m)) |>
  dplyr::slice_head(n = 15)

print(highest_drought_values, n = 15, width = Inf)
print(indicator_quantiles, n = Inf, width = Inf)
# -------------------------------------------------------------------
# 25. Harmonise indicator measurement scales
# -------------------------------------------------------------------

environmental_indicators_scaled <- environmental_indicators |>
  dplyr::mutate(
    heat_exposure_41plus_pct =
      heat_exposure_41plus * 100,
    
    heavy_rain_exposure_20plus_pct =
      heavy_rain_exposure_20plus * 100,
    
    flood_exposure_over50cm_pct =
      flood_exposure_over50cm
  )

scaled_indicator_names <- c(
  "canopy_percent",
  "drought_depth_m",
  "heat_exposure_41plus_pct",
  "flood_exposure_over50cm_pct",
  "heavy_rain_exposure_20plus_pct"
)

scaled_indicator_summary <- environmental_indicators_scaled |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(scaled_indicator_names),
      list(
        missing = ~ sum(is.na(.x)),
        minimum = ~ min(.x, na.rm = TRUE),
        maximum = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE)
      )
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c("indicator", ".value"),
    names_pattern = "^(.*)_(missing|minimum|maximum|mean)$"
  )

print(
  scaled_indicator_summary,
  n = Inf,
  width = Inf
)
# -------------------------------------------------------------------
# 26. Aggregate environmental indicators to municipalities
# -------------------------------------------------------------------

safe_weighted_mean <- function(x, weights) {
  valid <- !is.na(x) & !is.na(weights) & weights > 0
  
  if (!any(valid)) {
    return(NA_real_)
  }
  
  stats::weighted.mean(
    x = x[valid],
    w = weights[valid]
  )
}

population_coverage <- function(x, weights) {
  valid_population <- !is.na(weights) & weights > 0
  available_population <- valid_population & !is.na(x)
  
  total_population <- sum(weights[valid_population])
  
  if (total_population == 0) {
    return(NA_real_)
  }
  
  100 * sum(weights[available_population]) / total_population
}

municipality_environmental <- environmental_indicators_scaled |>
  dplyr::group_by(
    gemeentecode,
    gemeentenaam
  ) |>
  dplyr::summarise(
    neighbourhoods = dplyr::n(),
    municipality_population = sum(population, na.rm = TRUE),
    
    dplyr::across(
      dplyr::all_of(scaled_indicator_names),
      list(
        weighted_mean = ~ safe_weighted_mean(.x, population),
        population_coverage_pct = ~ population_coverage(.x, population)
      )
    ),
    
    .groups = "drop"
  )

municipality_environmental_check <- municipality_environmental |>
  dplyr::summarise(
    municipalities = dplyr::n(),
    minimum_neighbourhoods = min(neighbourhoods),
    maximum_neighbourhoods = max(neighbourhoods),
    zero_population_municipalities =
      sum(municipality_population == 0)
  )

print(
  municipality_environmental_check,
  width = Inf
)

dplyr::glimpse(municipality_environmental)
print(municipality_environmental_check, width = Inf)
# -------------------------------------------------------------------
# 27. Assess municipal indicator completeness and coverage
# -------------------------------------------------------------------

municipal_missing_summary <- municipality_environmental |>
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_weighted_mean"),
      ~ sum(is.na(.x))
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "indicator",
    values_to = "municipalities_missing"
  )

municipal_coverage_summary <- municipality_environmental |>
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_population_coverage_pct"),
      list(
        minimum = ~ min(.x, na.rm = TRUE),
        p25 = ~ quantile(.x, 0.25, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        p75 = ~ quantile(.x, 0.75, na.rm = TRUE),
        maximum = ~ max(.x, na.rm = TRUE),
        below_80 = ~ sum(.x < 80, na.rm = TRUE)
      )
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c("indicator", ".value"),
    names_pattern =
      "^(.*)_(minimum|p25|median|p75|maximum|below_80)$"
  )

print(
  municipal_missing_summary,
  n = Inf,
  width = Inf
)

print(
  municipal_coverage_summary,
  n = Inf,
  width = Inf
)
print(municipal_missing_summary, n = Inf, width = Inf)
# -------------------------------------------------------------------
# 28. Investigate municipal flood-data availability
# -------------------------------------------------------------------

flood_coverage_review <- municipality_environmental |>
  dplyr::transmute(
    gemeentecode,
    gemeentenaam,
    municipality_population,
    flood_exposure =
      flood_exposure_over50cm_pct_weighted_mean,
    flood_population_coverage =
      flood_exposure_over50cm_pct_population_coverage_pct,
    
    coverage_category = dplyr::case_when(
      is.na(flood_exposure) |
        flood_population_coverage == 0 ~ "No flood data",
      
      flood_population_coverage < 80 ~ "Below 80% coverage",
      
      TRUE ~ "At least 80% coverage"
    )
  )

flood_coverage_counts <- flood_coverage_review |>
  dplyr::count(
    coverage_category,
    name = "municipalities"
  )

municipalities_without_flood_data <- flood_coverage_review |>
  dplyr::filter(coverage_category == "No flood data") |>
  dplyr::arrange(gemeentenaam)

lowest_flood_coverage <- flood_coverage_review |>
  dplyr::filter(coverage_category != "No flood data") |>
  dplyr::arrange(flood_population_coverage) |>
  dplyr::slice_head(n = 20)

print(
  flood_coverage_counts,
  n = Inf,
  width = Inf
)

print(
  municipalities_without_flood_data,
  n = Inf,
  width = Inf
)

print(
  lowest_flood_coverage,
  n = 20,
  width = Inf
)
print(flood_coverage_counts, n = Inf)
municipalities_without_flood_data |>
  dplyr::select(
    gemeentecode,
    gemeentenaam,
    municipality_population
  ) |>
  print(n = Inf)
# -------------------------------------------------------------------
# 29. Investigate the meaning of missing flood values
# -------------------------------------------------------------------

flood_definition <- dashboard_metadata |>
  dplyr::filter(
    indicator == "Maximale overstromingsdiepte"
  ) |>
  dplyr::select(
    indicator,
    categorie,
    attribuutnaam,
    eenheid,
    bron,
    omschrijving_kort,
    omschrijving_lang
  ) |>
  dplyr::distinct()

print(
  t(as.data.frame(flood_definition)),
  quote = FALSE
)

all_flood_columns <- c(
  "p_tot20cm",
  "p_20tot50cm",
  "p_50tot200cm",
  "p_200pluscm"
)

flood_missing_patterns <- dashboard_data |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(all_flood_columns),
      ~ sum(is.na(.x))
    ),
    all_four_missing = sum(
      dplyr::if_all(
        dplyr::all_of(all_flood_columns),
        is.na
      )
    )
  )

print(
  flood_missing_patterns,
  width = Inf
)

no_flood_data_examples <- dashboard_data |>
  dplyr::filter(
    dplyr::if_all(
      dplyr::all_of(all_flood_columns),
      is.na
    )
  ) |>
  dplyr::select(
    buurtcode2024,
    buurtnaam,
    gemeentenaam,
    aant_inw,
    dplyr::all_of(all_flood_columns)
  ) |>
  dplyr::slice_head(n = 20)

print(
  no_flood_data_examples,
  n = 20,
  width = Inf
)
print(
  t(as.data.frame(flood_definition)),
  quote = FALSE
)
print(
  flood_missing_patterns,
  width = Inf
)
# -------------------------------------------------------------------
# 30. Check whether flood-depth categories form a complete distribution
# -------------------------------------------------------------------

flood_category_check <- dashboard_data |>
  dplyr::mutate(
    flood_category_total = rowSums(
      dplyr::pick(
        dplyr::all_of(all_flood_columns)
      ),
      na.rm = TRUE
    )
  )

flood_total_summary <- flood_category_check |>
  dplyr::summarise(
    rows = dplyr::n(),
    minimum = min(flood_category_total),
    p25 = quantile(flood_category_total, 0.25),
    median = median(flood_category_total),
    p75 = quantile(flood_category_total, 0.75),
    maximum = max(flood_category_total),
    approximately_100 = sum(
      dplyr::between(flood_category_total, 99.99, 100.01)
    ),
    below_99_99 = sum(flood_category_total < 99.99),
    above_100_01 = sum(flood_category_total > 100.01)
  )

print(
  flood_total_summary,
  width = Inf
)

flood_total_examples <- flood_category_check |>
  dplyr::select(
    buurtcode2024,
    buurtnaam,
    gemeentenaam,
    dplyr::all_of(all_flood_columns),
    flood_category_total
  ) |>
  dplyr::arrange(flood_category_total) |>
  dplyr::slice_head(n = 20)

print(
  flood_total_examples,
  n = 20,
  width = Inf
)
print(flood_total_summary, width = Inf)
# -------------------------------------------------------------------
# 31. Correct flood-category structural missing values
# -------------------------------------------------------------------

environmental_indicators_final <-
  environmental_indicators_scaled |>
  dplyr::mutate(
    flood_exposure_over50cm_pct =
      dplyr::coalesce(p_50tot200cm, 0) +
      dplyr::coalesce(p_200pluscm, 0)
  )

municipality_environmental_final <-
  environmental_indicators_final |>
  dplyr::group_by(
    gemeentecode,
    gemeentenaam
  ) |>
  dplyr::summarise(
    neighbourhoods = dplyr::n(),
    
    municipality_population =
      sum(population, na.rm = TRUE),
    
    dplyr::across(
      dplyr::all_of(scaled_indicator_names),
      list(
        weighted_mean =
          ~ safe_weighted_mean(.x, population),
        
        population_coverage_pct =
          ~ population_coverage(.x, population)
      )
    ),
    
    .groups = "drop"
  )

final_municipal_check <- municipality_environmental_final |>
  dplyr::summarise(
    municipalities = dplyr::n(),
    
    missing_canopy = sum(
      is.na(canopy_percent_weighted_mean)
    ),
    
    missing_drought = sum(
      is.na(drought_depth_m_weighted_mean)
    ),
    
    missing_heat = sum(
      is.na(heat_exposure_41plus_pct_weighted_mean)
    ),
    
    missing_flood = sum(
      is.na(flood_exposure_over50cm_pct_weighted_mean)
    ),
    
    missing_heavy_rain = sum(
      is.na(heavy_rain_exposure_20plus_pct_weighted_mean)
    ),
    
    minimum_flood_coverage = min(
      flood_exposure_over50cm_pct_population_coverage_pct,
      na.rm = TRUE
    )
  )

print(
  final_municipal_check,
  width = Inf
)
# -------------------------------------------------------------------
# 32. Save the final municipality environmental dataset
# -------------------------------------------------------------------

municipality_environmental_analysis <-
  municipality_environmental_final |>
  dplyr::transmute(
    municipality_code = gemeentecode,
    municipality_name = gemeentenaam,
    neighbourhoods,
    municipality_population,
    
    canopy_percent =
      canopy_percent_weighted_mean,
    
    drought_depth_m =
      drought_depth_m_weighted_mean,
    
    heat_exposure_41plus_pct =
      heat_exposure_41plus_pct_weighted_mean,
    
    flood_exposure_over50cm_pct =
      flood_exposure_over50cm_pct_weighted_mean,
    
    heavy_rain_exposure_20plus_pct =
      heavy_rain_exposure_20plus_pct_weighted_mean
  )

readr::write_csv(
  municipality_environmental_analysis,
  "tables/municipality_environmental_indicators.csv",
  na = ""
)

print(dim(municipality_environmental_analysis))
print(names(municipality_environmental_analysis))

# Identify municipality-related CBS objects currently available.
print(
  ls(
    pattern = "municip|gemeente",
    envir = .GlobalEnv
  )
)
