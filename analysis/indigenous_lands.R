# HEADER ----------------------------------------------------------------------
#
# Title:          Process Indigenous Lands Data
# Description:    This process Indigenous Lands data to fit the base grid
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

download_indigenous_lands()

indigenous_lands_grid <-
  process_indigenous_lands()

# Save grid polygons as FlatGeobuf file
write_parquet(
  x = indigenous_lands_grid,
  sink = "data/indigenous_lands.parquet",
  version = "latest"
)
