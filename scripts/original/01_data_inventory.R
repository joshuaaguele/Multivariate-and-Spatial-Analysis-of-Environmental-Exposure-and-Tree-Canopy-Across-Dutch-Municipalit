# Candidate indicators for the study

data_inventory <- tribble(
  ~category, ~indicator, ~role, ~proposed_source,
  "Exposure", "Flood exposure", "Environmental predictor", "Climate Impact Atlas",
  "Exposure", "Heat stress", "Environmental predictor", "Climate Impact Atlas",
  "Exposure", "Heavy-rain vulnerability", "Environmental predictor", "Climate Impact Atlas",
  "Sensitivity", "Population aged 65 or older", "PCA variable", "CBS",
  "Sensitivity", "Low-income households", "PCA variable", "CBS",
  "Sensitivity", "Social-assistance recipients", "PCA variable", "CBS",
  "Adaptive capacity", "Average or median income", "PCA variable", "CBS",
  "Adaptive capacity", "Highly educated population", "PCA variable", "CBS",
  "Adaptive capacity", "Green-space percentage", "PCA variable", "CBS/RIVM",
  "Control", "Population density", "Regression control", "CBS"
)

View(data_inventory)

write_csv(
  data_inventory,
  "tables/data_inventory.csv"
)