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
  sf::read_sf("./data/external_raw/biomes/biomes_2019.fgb") |>
  dplyr::filter(code_biome %in% c(1, 3))

grid <-
  tica::create_grid(
    aoi = aoi,
    crs = project_crs,
    resolution = 60000,
    full_cells = FALSE,
    shape = "hex"
  )

# Process data
tica::loop_function(
  function_name = "process_external_data",
  arguments_subset = c(2)
)

# Merge gridded data
merged_data <- tica::merge_data()

# Fill merged data
filled_data <- tica::fill_data(merged_data)

data_split <- rsample::initial_split(filled_data, prop = 3 / 4)

# Create data frames for the two sets:
train_data <- rsample::training(data_split)
test_data  <- rsample::testing(data_split)

veg_suppression_rec <-
  recipes::recipe(area ~ ., data = train_data) |>
  recipes::step_naomit(recipes::all_outcomes()) |>
  recipes::step_naomit(recipes::all_predictors()) |>
  recipes::update_role(cell_id, year, new_role = "ID") |>
  recipes::step_zv(recipes::all_predictors()) |>
  recipes::step_dummy(recipes::all_nominal_predictors())

rf_mod <-
  parsnip::rand_forest(trees = 1000) |>
  parsnip::set_engine("ranger") |>
  parsnip::set_mode("regression")

veg_suppression_wflow <-
  workflows::workflow() |>
  workflows::add_model(rf_mod) |>
  workflows::add_recipe(veg_suppression_rec)

veg_suppression_fit <- veg_suppression_wflow |>
  parsnip::fit(data = train_data)

predict(veg_suppression_fit, test_data)
