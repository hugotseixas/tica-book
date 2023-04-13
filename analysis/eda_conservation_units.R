# HEADER ----------------------------------------------------------------------
#
# Title:          Exploratory Analysis for Conservation Units Data
# Description:
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(conflicted)
library(sf)
library(arrow)
library(scales)
library(scico)
library(tidyverse)
library(tica)
#
# CONFLICTS -------------------------------------------------------------------
#

conflicts_prefer(dplyr::filter)

#
# OPTIONS ---------------------------------------------------------------------
#

#
# LOAD DATA -------------------------------------------------------------------

aoi <- read_sf("data/external/aoi/aoi.fgb")

aoi <-
  smoothr::smooth(
    st_simplify(aoi, dTolerance = 5000),
    method = "ksmooth",
    smoothness = 3
  )

base_grid <- read_sf("data/base_grid.fgb")

uc <-
  read_parquet(file = "data/conservation_units.parquet") |>
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) |>
  select(!c(geometry, cell_area)) |>
  mutate(uc_area = uc_area * 0.0001) |>
  arrange(cell_id, year) |>
  summarise(
    uc_area = sum(uc_area),
    .by = c("cell_id", "region_name", "year")
  ) |>
  mutate(
    cumulative_uc_area = cumsum(uc_area),
    .by = "cell_id"
  ) |>
  filter(year > 1986, year < 2021)

# EXPLORE DATA ----------------------------------------------------------------

create_visualizations(
  f = eda_histogram,
  data = filter(uc, uc_area != 0),
  variable = uc_area,
  group_variable = region_name,
  group_facet = TRUE,
  viz_title = "Conservation Unit Histogram",
  x_title = "Conservation Unit Area (ha)",
  y_title = "",
  scale_transform = "identity",
  n_bins = 50,
  x_lim = NULL,
  out_filename = "uc_histogram",
  out_path = "figs/eda/"
)

create_visualizations(
  f = eda_cumulative_distribution,
  data = filter(uc, uc_area != 0),
  variable = uc_area,
  group_variable = region_name,
  group_facet = TRUE,
  viz_title = "Cumulative Conservation Unit Area",
  x_title = "Conservation unit Area (ha)",
  y_title = "Cumulative Percentage (%)",
  quantiles_list = c(0.05, 0.5, 0.75, 1),
  scale_transform = "identity",
  out_filename = "uc_cumulative",
  out_path = "figs/eda/"
)

viz_data <- uc |>
  summarise(
    uc_area = sum(uc_area),
    .by = c("cell_id")
  ) |>
  filter(uc_area != 0) |>
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) |>
  st_as_sf() |>
  select(uc_area)

create_visualizations(
  f = eda_spatial_distribution,
  data = viz_data,
  variable = uc_area,
  variable_label = "Conservation Unit Area (ha)",
  group_variable = NULL,
  viz_title = "Spatial Distribution of Conservation Units",
  x_title = "",
  y_title = "",
  base_map = aoi,
  out_filename = "uc_spatial",
  out_path = "figs/eda/",
  out_width = 15,
  out_height = 15
)

viz_data <- read_parquet(file = "data/conservation_units.parquet") |>
  filter(year > 1986, year < 2021) |>
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) |>
  select(!c(geometry, cell_area)) |>
  drop_na() |>
  mutate(uc_area = uc_area * 0.0001) |>
  summarise(
    uc_area = sum(uc_area),
    .by = c("region_name", "government_level")
  )

create_visualizations(
  f = eda_colsum,
  data = viz_data,
  variable = uc_area,
  group_facet = TRUE,
  group_variable = region_name,
  cat_variable = government_level,
  scale_transform = "log",
  viz_title = "Deforested Areas Classes",
  x_title = "LULC",
  y_title = "Deforestation Area (ha)",
  out_filename = "uc_colsum",
  out_path = "figs/eda/"
)

viz_data <- uc |>
  summarise(
    cumulative_uc_area = sum(cumulative_uc_area),
    .by = c("region_name", "year")
  ) |>
  rename(date = year)

create_visualizations(
  f = eda_time_series,
  data = viz_data,
  variable = cumulative_uc_area,
  group_facet = TRUE,
  group_variable = region_name,
  viz_title = "Deforestation Time Series",
  x_title = "Year",
  y_title = "Deforestation Area (ha)",
  ts_type = "step",
  out_filename = "uc_timeseries",
  out_path = "figs/eda/"
)

viz_data <- read_parquet(file = "data/conservation_units.parquet") |>
  filter(year > 1986, year < 2021) |>
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) |>
  select(!c(geometry, cell_area)) |>
  drop_na() |>
  mutate(uc_area = uc_area * 0.0001) |>
  summarise(
    uc_area = sum(uc_area),
    .by = c("region_name", "group")
  ) |>
  rename(uc_type = group)

create_visualizations(
  f = eda_colsum,
  data = viz_data,
  variable = uc_area,
  group_facet = TRUE,
  group_variable = region_name,
  cat_variable = uc_type,
  scale_transform = "identity",
  viz_title = "Conservation Units Classes",
  x_title = "Conservation Unit Class",
  y_title = "Conservation Unit Area (ha)",
  out_filename = "uc_colsum",
  out_path = "figs/eda/"
)


