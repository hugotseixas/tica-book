# Download data
tica::loop_function(
  function_name = "download_external_data",
  arguments_subset = 1
)

# Set Project CRS
project_crs <-
  sf::st_crs(
    readr::read_file(
      glue::glue("./data/project_crs.txt")
    )
  )

# Create analysis grid
aoi <-
  sf::read_sf("./data/raw/biomes/biomes_2019.fgb") |>
  dplyr::filter(code_biome %in% c(1, 3))

grid <-
  tica::create_grid(
    aoi = aoi,
    crs = project_crs,
    resolution = 40000
  )

# Process data
tica::loop_function(
  function_name = "process_external_data",
  arguments_subset = c(1:8)
)

# Merge gridded data
merged_data <- tica::merge_data()

# Fill merged data
filled_data <- tica::fill_data(merged_data)

# Create Methods visualizations
tica::loop_function(
  function_name = "create_visualization",
  arguments_subset = c(3)
)

# Create EDA visualizations
tica::loop_function(
  function_name = "create_visualization",
  arguments_subset = c(4:20)
)

# Sample data
sampled_data <-
  tica::sample_data(
    filled_data = sf::read_sf("./data/merged/filled_data.fgb"),
    year_min = 1995, # Set start year of the timeseries
    year_max = 2021, # Set end year of the timeseries
    seed = 1 # For reproducibility
  )

# Run model
model_predictions <-
  tica::run_model_ensemble(
    filled_data = sf::read_sf("./data/merged/filled_data.fgb"),
    sampled_data = arrow::read_parquet("./data/sampled_data.parquet")
  )

# Create results visualizations
tica::loop_function(
  function_name = "create_visualization",
  arguments_subset = c(33:34)
)
