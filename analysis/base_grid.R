# HEADER ----------------------------------------------------------------------
#
# Title:          Create project grid
# Description:    This script creates a grid that will be used for all
#                 data in this project
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(conflicted)
library(sf)
library(geobr)
library(tidyverse)
library(tica)
#
# CONFLICTS -------------------------------------------------------------------
#

conflicts_prefer(dplyr::filter)

#
# OPTIONS ---------------------------------------------------------------------
#

# Size of the grid cell in degrees
resolution <- 0.2

# Create grid with cells that are fully within the biomes?
full_cells <- TRUE
# If TRUE, st_within is called as predicate for spatial filter
# If FALSE, st_intersects is called as predicate for spatial filter

#
# LOAD BIOME DATA -------------------------------------------------------------

# Load biomes limit
biomes <-
  read_biomes(
    year = 2019,
    simplified = TRUE, # Biome limits are simplified (topology preserved)
    showProgress = FALSE
  ) %>%
  filter(name_biome %in% c("Amazônia", "Cerrado"))

# CREATE  AND SAVE GRID -------------------------------------------------------

base_grid <-
  create_grid(
    resolution = 0.2,
    full_cells = TRUE
  )

# Save grid polygons as FlatGeobuf file
write_sf(
  obj = base_grid,
  dsn = "./data/base_grid.fgb",
  driver = "FlatGeobuf",
  append = FALSE,
  delete_dsn = TRUE
)

# CREATE AND SAVE PLOT --------------------------------------------------------

# Plot grid and biome limits
grid_plot <- ggplot() +
  geom_sf(
    data = biomes,
    fill = "transparent"
  ) +
  geom_sf(
    data = base_grid,
    fill = "transparent"
  ) +
  theme_void()

# Save plot
ggsave(
  filename = ".//figs/grid_cells.png",
  plot = grid_plot,
  device = ragg::agg_png,
  width = 15,
  height = 15,
  units = "cm",
  dpi = 300
)

