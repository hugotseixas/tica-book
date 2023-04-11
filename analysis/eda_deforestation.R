# HEADER ----------------------------------------------------------------------
#
# Title:          EDA for deforestation variable
# Description:    This script explores deforestation data from Mapbiomas
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(conflicted)
library(sf)
library(arrow)
library(glue)
library(scales)
library(scico)
library(cowplot)
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

defo <- read_parquet("data/deforestation.parquet")

base_grid <- read_sf("data/base_grid.fgb")

# PROCESS DATA ---------------------------------------------------------------

defo <- defo |>
  filter(year > 1986, year < 2021) |>
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) |>
  select(!c(geometry, cell_area)) |>
  mutate(deforestation_area = deforestation_area * 0.0001) |>
  summarise(
    deforestation_area = sum(deforestation_area),
    .by = c("cell_id", "region_name", "year")
  )

# EXPLORE DATA ----------------------------------------------------------------
create_visualizations(
  f = eda_histogram,
  data = defo,
  variable = deforestation_area,
  group_variable = region_name,
  group_facet = TRUE,
  viz_title = "Deforestation Histogram",
  x_title = "Deforestation Area (ha)",
  y_title = "",
  scale_transform = "log",
  n_bins = 50,
  x_lim = NULL,
  out_filename = "deforestation_histogram",
  out_path = "figs/eda/"
)

## Create Cumulative Visualization ----
create_visualizations(
  f = eda_cumulative_distribution,
  data = select(defo, c(region_name, deforestation_area)),
  variable = deforestation_area,
  group_variable = region_name,
  group_facet = TRUE,
  viz_title = "Cumulative Deforestation Area",
  x_title = "Deforestation Area (ha)",
  y_title = "Cumulative Percentage (%)",
  quantiles_list = c(0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1),
  scale_transform = "log",
  out_filename = "deforestation_cumulative",
  out_path = "figs/eda/"
)

## Spatial distribution ----
viz_data <- defo |>
  summarise(
    deforestation_area = sum(deforestation_area),
    .by = c("cell_id")
  ) |>
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) |>
  st_as_sf() |>
  select(deforestation_area)

create_visualizations(
  f = eda_spatial_distribution,
  data = viz_data,
  variable = deforestation_area,
  variable_label = "Deforestation Area (ha)",
  group_variable = NULL,
  viz_title = "Spatial Distribution of Deforestation",
  x_title = "",
  y_title = "",
  base_map = aoi,
  out_filename = "deforestation_spatial",
  out_path = "figs/eda/",
  out_width = 15,
  out_height = 15
)

## Create Time Series Plot ----
viz_data <- defo |>
  summarise(
    deforestation_area = sum(deforestation_area),
    .by = c("region_name", "year")
  ) |>
  rename(date = year)

create_visualizations(
  f = eda_time_series,
  data = viz_data,
  variable = deforestation_area,
  group_facet = TRUE,
  group_variable = region_name,
  viz_title = "Deforestation Time Series",
  x_title = "Year",
  y_title = "Deforestation Area (ha)",
  ts_type = "line",
  out_filename = "deforestation_timeseries",
  out_path = "figs/eda/"
)

## Create Sum plot ----
viz_data <- read_parquet("data/deforestation.parquet") |>
  filter(year > 1986, year < 2021) |>
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) |>
  mutate(deforestation_area = deforestation_area * 0.0001) |>
  summarise(
    deforestation_area = sum(deforestation_area),
    .by = c("region_name", "natural_class")
  )

create_visualizations(
  f = eda_colsum,
  data = viz_data,
  variable = deforestation_area,
  group_facet = TRUE,
  group_variable = region_name,
  cat_variable = natural_class,
  scale_transform = "log",
  viz_title = "Deforested Areas Classes",
  x_title = "LULC",
  y_title = "Deforestation Area (ha)",
  out_filename = "deforestation_colsum_natural",
  out_path = "figs/eda/"
)

viz_data <- read_parquet("data/deforestation.parquet") |>
  filter(year > 1986, year < 2021) |>
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) |>
  mutate(deforestation_area = deforestation_area * 0.0001) |>
  summarise(
    deforestation_area = sum(deforestation_area),
    .by = c("region_name", "human_class")
  )

create_visualizations(
  f = eda_colsum,
  data = viz_data,
  variable = deforestation_area,
  group_facet = TRUE,
  group_variable = region_name,
  cat_variable = human_class,
  scale_transform = "log",
  viz_title = "Deforested Areas Classes",
  x_title = "LULC",
  y_title = "Deforestation Area (ha)",
  out_filename = "deforestation_colsum_human",
  out_path = "figs/eda/"
)

## Create summary table ----
eda_summary_table(
  data = defo,
  variable = deforestation_area,
  rowname_variable = region_name,
  viz_title = "Deforestation Area (ha)",
  out_path = "./figs/eda/",
  out_filename = "deforestation_table"
)
