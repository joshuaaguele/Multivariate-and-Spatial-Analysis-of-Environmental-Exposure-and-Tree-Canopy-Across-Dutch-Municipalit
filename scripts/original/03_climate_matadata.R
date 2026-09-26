climate_metadata <- read_delim(
  "data_raw/climate_impact_metadata.csv",
  delim = ";",
  quote = "\"",
  trim_ws = TRUE,
  show_col_types = FALSE
) |>
  clean_names()

glimpse(climate_metadata)
names(climate_metadata)
View(climate_metadata)
# Count datasets available under each environmental theme

climate_metadata |>
  filter(!is.na(thema)) |>
  count(thema, sort = TRUE)
# Select themes relevant to regional vulnerability

environmental_catalogue <- climate_metadata |>
  filter(
    thema %in% c(
      "Wateroverlast",
      "Droogte",
      "Hitte",
      "Overstroming",
      "Basiskaarten"
    )
  ) |>
  select(
    thema,
    titel_dataset_in_de_kaartviewer,
    datasetnaam_in_downloadfolder,
    varianten_van_dataset
  ) |>
  arrange(thema, titel_dataset_in_de_kaartviewer)

View(environmental_catalogue)

write_csv(
  environmental_catalogue,
  "tables/environmental_dataset_catalogue.csv"
)
# Shortlist potentially useful environmental datasets

environmental_shortlist <- environmental_catalogue |>
  filter(
    str_detect(
      str_to_lower(titel_dataset_in_de_kaartviewer),
      "boomkroon|hitte|waterdiepte|overstrom|bodemdaling|grondwaterstand"
    )
  )

View(environmental_shortlist)

write_csv(
  environmental_shortlist,
  "tables/environmental_dataset_shortlist.csv"
)
# Select one dataset for each main environmental dimension

selected_environmental_datasets <- environmental_shortlist |>
  filter(
    str_detect(
      str_to_lower(titel_dataset_in_de_kaartviewer),
      paste(
        c(
          "boomkroonbedekking",
          "hittestress door warme nachten",
          "laagste grondwaterstand.*extreem droge zomer",
          "waterdiepte bij hevige bui",
          "overstromingsdiepte"
        ),
        collapse = "|"
      )
    )
  ) |>
  mutate(
    proposed_indicator = case_when(
      str_detect(str_to_lower(titel_dataset_in_de_kaartviewer),
                 "boomkroonbedekking") ~ "Tree-canopy coverage",
      str_detect(str_to_lower(titel_dataset_in_de_kaartviewer),
                 "hittestress") ~ "Heat stress",
      str_detect(str_to_lower(titel_dataset_in_de_kaartviewer),
                 "laagste grondwaterstand") ~ "Drought exposure",
      str_detect(str_to_lower(titel_dataset_in_de_kaartviewer),
                 "waterdiepte bij hevige bui") ~ "Heavy-rain exposure",
      str_detect(str_to_lower(titel_dataset_in_de_kaartviewer),
                 "overstromingsdiepte") ~ "Flood exposure"
    )
  ) |>
  select(
    proposed_indicator,
    thema,
    titel_dataset_in_de_kaartviewer,
    datasetnaam_in_downloadfolder,
    varianten_van_dataset
  )

View(selected_environmental_datasets)

write_csv(
  selected_environmental_datasets,
  "tables/selected_environmental_datasets.csv"
)
# Select the five baseline environmental files for the analysis

final_climate_files <- download_file_options |>
  filter(
    (
      proposed_indicator == "Tree-canopy coverage" &
        dataset_file == "bkb_2024.tif"
    ) |
      (
        proposed_indicator == "Drought exposure" &
          str_detect(
            dataset_file,
            regex("huidig", ignore_case = TRUE)
          )
      ) |
      (
        proposed_indicator == "Heat stress" &
          str_detect(
            dataset_file,
            regex("huidig", ignore_case = TRUE)
          )
      ) |
      (
        proposed_indicator == "Flood exposure" &
          str_detect(
            dataset_file,
            regex("grote_kans", ignore_case = TRUE)
          ) &
          !str_detect(
            dataset_file,
            regex(
              "middelgrote_kans|zeer_grote_kans",
              ignore_case = TRUE
            )
          )
      ) |
      (
        proposed_indicator == "Heavy-rain exposure" &
          str_detect(
            dataset_file,
            regex("1-100 jaar\\.tif$", ignore_case = TRUE)
          )
      )
  ) |>
  select(
    proposed_indicator,
    thema,
    dataset_file
  )

# Open the selected files as a table

View(final_climate_files)

# Save the selection permanently

write_csv(
  final_climate_files,
  "tables/final_climate_files.csv"
)

# Confirm that exactly five files were selected

nrow(final_climate_files)