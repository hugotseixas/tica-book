
library(conflicted)
library(arrow)
library(tica)
library(sf)
library(tidyverse)

conflicts_prefer(dplyr::filter)

tica::download_federal_roads()

federal_roads_grid <- tica::process_federal_roads()

write_parquet(
  x = federal_roads_grid,
  sink = "data/federal_roads.parquet",
  version = "latest"
)


aoi <- read_sf("data/external/aoi/aoi.fgb")

base_grid <- read_sf("data/base_grid.fgb")

fr <-
  read_parquet(file = "data/federal_roads.parquet") |>
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) |>
  select(!c(geometry, cell_area)) |>
  arrange(cell_id, year) |>
  summarise(
    length = sum(length, na.rm = TRUE),
    .by = c("cell_id", "region_name", "year")
  ) |>
  mutate(
    cumulative_length = cumsum(length),
    .by = "cell_id"
  ) |>
  filter(year > 1986, year < 2021)

viz_data <- fr |>
  summarise(
    length = sum(length),
    .by = c("cell_id")
  ) |>
  filter(length != 0) |>
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) |>
  st_as_sf() |>
  select(length)

create_visualizations(
  f = eda_spatial_distribution,
  data = viz_data,
  variable = length,
  variable_label = "Road Length (m)",
  group_variable = NULL,
  viz_title = "Spatial Distribution of Federal Roads",
  x_title = "",
  y_title = "",
  base_map = aoi,
  out_filename = "fr_spatial",
  out_path = "figs/eda/",
  out_width = 15,
  out_height = 15
)
