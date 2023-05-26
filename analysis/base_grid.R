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
library(scales)
library(gt)
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

aoi <- read_sf("data/external/aoi/aoi.fgb")

# CREATE AND SAVE GRID --------------------------------------------------------

base_grid <- create_grid(resolution = 30000)

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
    data = aoi,
  ) +
  geom_sf(
    data = base_grid,
    fill = "transparent"
  ) +
  theme_void()

ggsave2(
  "./figs/viz_grid.png",
  plot = viz_grid
)

save(viz_grid, file = "./figs/base_grid.rdata")

cell_area <- base_grid |>
  slice_min(
    order_by = abs(cell_area - mean(cell_area)),
    with_ties = FALSE
  ) |>
  select(cell_area) |>
  mutate(
    key = 1
  ) |>
  left_join(
    tibble(
      key = rep(1, 7),
      percentage = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.9, 1)
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
          st_geometry(first(cell_area)), cellsize = 1000, square = FALSE
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
  geom_sf(
    data = sub_cell_area,
    aes(fill = percentage, color = percentage)
  ) +
  geom_sf(
    fill = "transparent",
    color = "#000000",
    linewidth = 0.5
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
    nudge_y = -25000
  ) +
  scico::scale_fill_scico(begin = 0.3) +
  scico::scale_color_scico(begin = 0.3) +
  coord_sf(clip = "off") +
  theme_void() +
  theme(
    legend.position = "",
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
  plot = viz_grid_area
)

save(viz_grid_area, file = "./figs/base_grid_area.rdata")

color_scale <-
  tibble(x = rep(NA, 2), y = rep(NA, 2), fill = 0:1) |>
  ggplot(aes(x = x, y = y, fill = fill)) +
  geom_point(alpha = 0) +
  scico::scale_fill_scico(
    breaks = c(0, 1),
    labels = c("Minimum","Maximum"),
    guide = guide_colorbar(
      frame.colour = "#000000",
      frame.linewidth = 1/.pt,
      ticks.colour = "transparent"
    )
  ) +
  theme_nothing() +
  theme(
    legend.position = c(0.5, 0.5), # move the legend to the center
    legend.title = element_blank(),
    legend.text = element_text(size = 13, face = "bold"),
    legend.key = element_rect(fill = 'NA'),
    legend.key.height = ggplot2::unit(6, 'mm'),
    legend.key.width = ggplot2::unit(30, 'mm'),
    legend.text.align = c(0, 1),
    legend.direction = "horizontal"
  )

ggsave2(
  "./figs/color_scale.png",
  plot = color_scale
)

save(color_scale, file = "./figs/color_scale.rdata")

read_csv("data/variables_table.csv") |>
  gt() |>
  sub_missing() |>
  cols_label(
    variable = gt::md("**Variable**"),
    source = gt::md("**Source**"),
    creator = gt::md("**Creator**"),
    license = gt::md("**License**")
  ) |>
  tab_options(
    table.font.size = 14
  ) |>
  opt_stylize(style = 1, color = "gray") |>
  gtsave("figs/variables_table.html")
