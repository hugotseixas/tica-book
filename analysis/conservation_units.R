# HEADER ----------------------------------------------------------------------
#
# Title:          Process Conservation Units Data
# Description:    This process Conservation Units data to fit the base grid
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(conflicted)
library(arrow)
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
# PROCESS AND SAVE UC DATA ----------------------------------------------------

conservation_units_grid <-
  process_conservation_units(
    base_grid_path = "data/base_grid.fgb",
    timespan = 1985:2021
  )

# Save grid polygons as FlatGeobuf file
write_parquet(
  x = conservation_units_grid,
  sink = "data/conservation_units.parquet",
  version = "latest"
)

