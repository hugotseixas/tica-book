# HEADER ----------------------------------------------------------------------
#
# Title:          Process Deforestation Data
# Description:    This process Deforestation data to fit the base grid
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(conflicted)
library(arrow)
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

deforestation_grid <-
  process_land_use()

# Save grid polygons as FlatGeobuf file
write_parquet(
  x = deforestation_grid,
  sink = "data/deforestation.parquet",
  version = "latest"
)
