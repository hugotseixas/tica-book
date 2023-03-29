# HEADER ----------------------------------------------------------------------
#
# Title:          Create art designs
# Description:    This script creates two art designs to be used to represent
#                 the Cerrado and Amazon biomes.
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(conflicted)
library(sf)
library(geobr)
library(glue)
library(fs)
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

# CREATE AND SAVE GRID --------------------------------------------------------

# Create grid based on biomes polygons
full_grid <-
  st_make_grid(
    x = biomes,
    square = FALSE,
    cellsize = resolution # Resolution of the grid cell (check options above)
  ) %>%
  st_as_sf()

# CREATE AND SAVE PLOT --------------------------------------------------------

walk2(
  .x = biomes$code_biome,
  .y = c("amazon", "cerrado"),
  .f = ~ {

    biome <- biomes %>%
      filter(code_biome == .x)

    grid <- full_grid %>%
      st_filter(
        y = biome,
        .predicate = if (full_cells) {st_within} else {st_intersects}
      )

    img <-
      read_table(
        file = glue("./figs/art/url_{.y}.txt"),
        col_names = FALSE,
        col_types = "c"
      ) %>%
      slice_sample(
        n = nrow(grid)
      ) %>%
      pull(X1)

    # Plot grid and biome limits
    grid_plot <- ggplot() +
      with_outer_glow(
        geom_sf_pattern(
          data = grid,
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
      filename = glue("./figs/art/art_{.y}.png"),
      plot = grid_plot,
      device = ragg::agg_png,
      width = 15,
      height = 10,
      units = "cm",
      dpi = 300
    )

  }
)
