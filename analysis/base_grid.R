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
# LOAD BIOME DATA -------------------------------------------------------------

# Load biomes limit
biomes <-
  read_biomes(
    year = 2019,
    simplified = TRUE, # Biome limits are simplified (topology preserved)
    showProgress = FALSE
  ) |>
  filter(name_biome %in% c("Amazônia", "Cerrado"))

biomes <-
  smoothr::smooth(
    st_simplify(biomes, dTolerance = 5000),
    method = "ksmooth",
    smoothness = 3
  )

# CREATE AND SAVE GRID --------------------------------------------------------

base_grid <- create_grid(resolution = 0.3)

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
viz_grid <- ggplot() +
  geom_sf(
    data = biomes,
  ) +
  geom_sf(
    data = base_grid,
    fill = "transparent"
  ) +
  theme_void()

cell_area <- base_grid |>
  slice_min(order_by = abs(cell_area - mean(cell_area))) |>
  select(cell_area) |>
  mutate(
    key = 1,
    cell_area = cell_area * 0.0001
  ) |>
  left_join(
    tibble(
      key = rep(1, 6),
      percentage = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.9)
    ),
    by = join_by(key)
  ) |>
  mutate(percentage_area = cell_area * percentage)

sub_cell_area <-
  map_df(
    cell_area$percentage,
    \(p) {

      sub_cell_area <-
        st_make_grid(
          st_geometry(first(cell_area)), cellsize = 0.015, square = FALSE
        ) %>%
        st_intersection(
          st_geometry(first(cell_area))
        ) %>%
        st_as_sf() %>%
        filter(st_is(., c("POLYGON", "GEOMETRYCOLLECTION"))) %>%
        slice_sample(prop = p) %>%
        mutate(percentage = p)

    }
  )

viz_cell_area <- cell_area |>
  ggplot() +
  facet_wrap(
    facets = vars(percentage),
    nrow = 1,
    labeller = label_bquote(.(percent(percentage)))
  ) +
  geom_sf() +
  geom_sf(
    data = sub_cell_area,
    fill = "#000000",
    color = "#000000"
  ) +
  geom_sf_text(
    aes(
      label = scales::number(
        percentage_area,
        suffix = " ha",
        accuracy = 1,
        scale_cut = cut_short_scale()
      )
    ),
    size = 4,
    nudge_y = -0.22
  ) +
  coord_sf(clip = "off") +
  theme_void() +
  theme(
    text = ggplot2::element_text(size = 13, face = "bold"),
    plot.margin = unit(c(-0.7, 0, 0, 0), "cm")
  )

viz_grid_area <-
  plot_grid(
    viz_grid,
    viz_cell_area,
    ncol = 1,
    scale = 0.9,
    rel_heights = c(1, 0.15)
  )

ggsave2(
  "./figs/base_grid_area.png",
  plot = viz_cell_area
)

save(viz_grid_area, file = "./figs/base_grid_area.rdata")
save(viz_grid, file = "./figs/base_grid.rdata")

