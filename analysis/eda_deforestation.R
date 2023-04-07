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

conflicts_prefer(dplyr::filter)

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
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) |>
  mutate(deforestation_area = deforestation_area * 0.0001)

# EXPLORE DATA ----------------------------------------------------------------

## Create Histogram Visualization ----
viz_list <-
  map(
    c(3, 1),
    \(region) {

      viz <- defo |>
        filter(region_code == region) |>
        summarise(
          deforestation_area = sum(deforestation_area),
          .by = c("cell_id", "region_name", "year")
        ) |>
        eda_histogram(
          variable = deforestation_area,
          n_bins = 50,
          scale_transform = "log",
          xlim = c(70, 600000)
        ) +
        facet_wrap(facets = vars(region_name)) +
        theme(
          strip.text = element_text()
        )

      if (region == 1) {

        viz <- viz +
          labs(x = "Deforestation Area (ha)") +
          theme(
            axis.title.x = element_text(),
            axis.text.x = element_text(size = 10)
          )

      }

      return(viz)

    }
  )

title <- ggdraw() +
  draw_label(
    "Deforestation Histogram",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

viz_hist <-
  plot_grid(
    title,
    plotlist =  viz_list,
    ncol = 1,
    scale = 0.9,
    rel_heights = c(0.1, 1, 1)
  )

save(viz_hist, file = "./figs/deforestation_hist.rdata")

## Create Cumulative Visualization ----
viz_list <-
  map(
    c(3, 1),
    \(region) {

      viz <- defo |>
        filter(region_code == region) |>
        summarise(
          deforestation_area = sum(deforestation_area),
          .by = c("cell_id", "region_name", "year")
        ) |>
        eda_cumulative_distribution(
          variable = deforestation_area,
          quantiles_list = c(0.005, 0.25, 0.5, 0.75, 0.95, 0.99, 1),
          scale_transform = "log"
        ) +
        facet_wrap(facets = vars(region_name)) +
        theme(
          strip.text = element_text(),
          axis.text = element_text(size = 10),
          panel.grid.major.y = element_line(
            color = "#999999",
            linewidth = 0.1
          ),
          plot.margin = unit(c(-0.3, 0, 0, 0), "cm")
        )

      return(viz)

    }
  )

title <- ggdraw() +
  draw_label(
    "Cumulative Percentage of Deforestation",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 8, 0)
  )

viz_cumulative <-
  plot_grid(
    title,
    plotlist =  viz_list,
    ncol = 1,
    rel_heights = c(0.1, 1, 1),
    scale = 0.9
  ) +
  draw_label(
    "Cumulative Deforestation Percentage (%)",
    x = 0, y = 0.5,
    vjust = 1.5,
    angle = 90,
    size = 13
  ) +
  draw_label(
    "Deforestation Area (ha)",
    x = 0.5, y = 0,
    vjust = -0.2,
    angle = 0,
    size = 13
  )

save(viz_cumulative, file = "./figs/deforestation_cumulative.rdata")

## Spatial distribution ----
viz <- defo |>
  filter(year %in% c(2020)) |>
  st_as_sf() |>
  summarise(
    deforestation_area = sum(deforestation_area),
    .by = c("cell_id", "year", "geometry")
  ) |>
  filter(deforestation_area > 800) |>
  eda_spatial_distribution(
    variable = deforestation_area,
    base_map = aoi
  ) +
  labs(fill = "Deforestation Area (ha)") +
  facet_wrap(facets = vars(year)) +
  theme(
    strip.text = element_text(),
    legend.position = "bottom",
    legend.key.height = unit(2, 'mm'),
    legend.key.width = unit(25, 'mm'),
    legend.justification = c(0.5, 0.5),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

title <- ggdraw() +
  draw_label(
    "Spatial Distribution of Deforestation",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

viz_spatial_distribution <-
  plot_grid(
    title,
    viz,
    ncol = 1,
    rel_heights = c(0.1, 1),
    scale = 0.9
  )

save(
  viz_spatial_distribution,
  file = "./figs/deforestation_spatial_distribution.rdata"
)

## Create Time Series Plot ----



## Create sum plot ----



# SAVE PLOTS ------------------------------------------------------------------

# Save histogram plot
ggsave(
  filename = "./figs/deforestation_hist.png",
  plot = viz_hist,
  device = ragg::agg_png,
  width = 15,
  height = 14,
  units = "cm",
  dpi = 300
)

# Save cumulative plot
ggsave(
  filename = "./figs/deforestation_cumsum.png",
  plot = viz_cumulative,
  device = ragg::agg_png,
  width = 15,
  height = 14,
  units = "cm",
  dpi = 300
)

# Save map plot
ggsave(
  filename = "./figs/deforestation_map.png",
  plot = viz_spatial_distribution,
  device = ragg::agg_png,
  width = 15,
  height = 15,
  units = "cm",
  dpi = 300
)
