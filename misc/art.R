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
library(ggpattern)
library(tidyverse)
#
# CONFLICTS -------------------------------------------------------------------
#

conflicts_prefer(dplyr::filter)

#
# OPTIONS ---------------------------------------------------------------------
#

# Size of the grid cell in degrees
resolution <- 4

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

img <-
  read_table("figs/art/amazon_url.txt", col_names = FALSE) %>%
  pull(X1)

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
  filter(code_biome == 1)

# CREATE AND SAVE PLOT --------------------------------------------------------

# Plot grid and biome limits
grid_plot <- ggplot() +
  with_outer_glow(
    geom_sf_pattern(
      data = grid %>% slice_head(n = length(img)),
      pattern_filename = img,
      pattern = 'image',
      pattern_type = "expand"
    ),
    sigma = 10,
    expand = 15
  ) +
  geom_sf(
    data = grid,
    color = "#ffffff",
    fill = "transparent",
    lwd = 1
  ) +
  theme_void() +
  theme(
    legend.position = ""
  )

# Save plot
ggsave(
  filename = "figs/art_amazon.png",
  plot = grid_plot,
  device = ragg::agg_png,
  width = 15,
  height = 10,
  units = "cm",
  dpi = 300
)
