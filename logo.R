# HEADER ----------------------------------------------------------------------
#
# Title:          Create project logo
# Description:    This script creates a logo
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(conflicted)
library(sf)
library(geobr)
library(ggfx)
library(tidyverse)
#
# CONFLICTS -------------------------------------------------------------------
#

conflicts_prefer(dplyr::filter)

#
# OPTIONS ---------------------------------------------------------------------
#

# Size of the grid cell in degrees
resolution <- 3

# Create grid with cells that are fully within the biomes?
full_cells <- FALSE
# If TRUE, st_within is called as predicate for spatial filter
# If FALSE, st_intersects is called as predicate for spatial filter

#
# LOAD DATA -------------------------------------------------------------------

# Load biomes limit
biomes <-
  read_biomes(
    year = 2019,
    simplified = TRUE, # Biome limits are simplified (topology preserved)
    showProgress = FALSE
  ) %>%
  filter(name_biome %in% c("Amazônia", "Cerrado"))

# CREATE AND SAVE GRID --------------------------------------------------------

# Create grid based on biomes polygons
grid <-
  st_make_grid(
    x = biomes,
    square = FALSE,
    cellsize = resolution # Resolution of the grid cell (check options above)
  ) %>%
  st_as_sf() %>%
  st_join(
    y = biomes,
    left = FALSE,
    join = if (full_cells) {st_within} else {st_intersects}
  ) %>%
  mutate(
    value = row_number(),
    .by = "code_biome"
  ) %>%
  mutate(
    value = log(value),
    value = if_else(code_biome == 1, value * -1, value)
  )

# CREATE AND SAVE PLOT --------------------------------------------------------

# Plot grid and biome limits
(
  grid_plot <- ggplot() +
    with_outer_glow(
      geom_sf(
        data = grid,
        aes(fill = value),
        color = "#4c4038",
        lwd = 1
      ),
      colour = "#4c4038",
      sigma = 10,
      expand = 15
    ) +
    scale_fill_gradient2(
      low = "#c78e17",
      mid = "#ffffff",
      high = "#bf4300"
    ) +
    theme_void() +
    theme(
      legend.position = ""
    )
)

# Save plot
ggsave(
  filename = "logo.png",
  plot = grid_plot,
  device = ragg::agg_png,
  width = 15,
  height = 15,
  units = "cm",
  dpi = 300
)
