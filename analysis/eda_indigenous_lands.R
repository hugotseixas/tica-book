# HEADER ----------------------------------------------------------------------
#
# Title:          Exploratory Analysis for Indigenous Lands Data
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

base_grid <- read_sf("data/base_grid.fgb")

il <-
  read_parquet(file = "data/indigenous_lands.parquet") |>
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) |>
  select(!c(geometry, cell_area)) |>
  mutate(il_area = il_area * 0.0001) |>
  arrange(cell_id, year) |>
  summarise(
    il_area = sum(il_area, na.rm = TRUE),
    .by = c("cell_id", "region_name", "year")
  ) |>
  mutate(
    cumulative_il_area = cumsum(il_area),
    .by = "cell_id"
  ) |>
  filter(year > 1986, year < 2021)

viz_data <- il |>
  summarise(
    il_area = sum(il_area),
    .by = c("cell_id")
  ) |>
  filter(il_area != 0) |>
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) |>
  st_as_sf() |>
  select(il_area)

create_visualizations(
  f = eda_spatial_distribution,
  data = viz_data,
  variable = il_area,
  variable_label = "Indigenous Land Area (ha)",
  group_variable = NULL,
  viz_title = "Spatial Distribution of Indigenous Land",
  x_title = "",
  y_title = "",
  base_map = aoi,
  out_filename = "il_spatial",
  out_path = "figs/eda/",
  out_width = 15,
  out_height = 15
)

create_visualizations(
  f = eda_histogram,
  data = filter(il, il_area != 0),
  variable = il_area,
  group_variable = region_name,
  group_facet = TRUE,
  viz_title = "Indigenous Lands Histogram",
  x_title = "Indigenous Lands  Area (ha)",
  y_title = "",
  scale_transform = "identity",
  n_bins = 50,
  x_lim = NULL,
  out_filename = "il_histogram",
  out_path = "figs/eda/"
)
