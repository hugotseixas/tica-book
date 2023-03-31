# HEADER ----------------------------------------------------------------------
#
# Title:          Exploratory Analysis for Conservation Units Data
# Description:
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(conflicted)
library(sf)
library(arrow)
library(scales)
library(scico)
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
# LOAD DATA -------------------------------------------------------------------

aoi <- read_sf("data/external/aoi/aoi.fgb")

base_grid <- read_sf("data/base_grid.fgb")

uc <- read_parquet(file = "data/conservation_units.parquet")

# EXPLORE DATA ----------------------------------------------------------------

## Time Series ----
timeseries_plot <- uc %>%
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) %>%
  select(!geometry) %>%
  summarise(
    uc_area = as.numeric(sum(uc_area)),
    .by = c("year", "region_name")
  ) %>%
  arrange(year) %>%
  mutate(
    uc_area = cumsum(uc_area),
    uc_area = uc_area * 0.0001,
    .by = "region_name"
  ) %>%
  ggplot(
    aes(
      x = year,
      y = uc_area,
      shape = region_name,
      color = uc_area
    )
  ) +
  facet_wrap(
    facets = vars(region_name),
    ncol = 1,
    scales = "free_y"
  ) +
  geom_step() +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  scale_x_continuous(
    labels = label_date(format = "%Y")
  ) +
  scale_color_scico(
    palette = "bilbao",
    begin = 0.6,
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  labs(
    title = "Conservation Units area time series",
    y = "Area (ha)",
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = ""
  )

## Spatial distribution ----
map_plot <- uc %>%
  summarise(
    uc_area = as.numeric(sum(uc_area)),
    .by = c("cell_id", "year")
  ) %>%
  arrange(cell_id, year) %>%
  mutate(
    uc_area = cumsum(uc_area),
    uc_area = uc_area * 0.0001,
    .by = "cell_id"
  ) %>%
  filter(year %in% c(1985, 2021)) %>%
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) %>%
  ggplot() +
  facet_wrap(facets = vars(year)) +
  geom_sf(
    data = aoi,
    fill = "transparent"
  ) +
  geom_sf(
    aes(
      geometry = geometry,
      fill = uc_area
    )
  ) +
  scale_fill_scico(
    palette = "bilbao",
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  labs(
    title = "Spatial distribution of Conservation Units",
    fill = "Area (ha)"
  ) +
  theme_void() +
  theme(text = element_text(size = 12))

## Histogram ----
(
  hist_plot <- uc %>%
    summarise(
      uc_area = as.numeric(sum(uc_area)),
      .by = c("cell_id", "year")
    ) %>%
    arrange(cell_id, year) %>%
    mutate(
      uc_area = cumsum(uc_area),
      uc_area = uc_area * 0.0001,
      .by = "cell_id"
    ) %>%
    left_join(
      base_grid,
      by = join_by(cell_id)
    ) %>%
    select(!geometry) %>%
    mutate(uc_area = as.numeric(uc_area)) %>%
    filter(uc_area > 0) %>%
    ggplot() +
    facet_wrap(
      facets = vars(region_name),
      ncol = 1
    ) +
    geom_histogram(
      aes(x = uc_area, y = after_stat(count)),
      color = "#000000",
      fill = "#e8e8e8",
      bins = 50
    ) +
    scale_x_log10(
      breaks = log_breaks(6),
      labels = label_number(accuracy = 1),
      expand = c(0, 0)
    ) +
    labs(
      title = "Conservation Units delimitation histogram",
      x = "Area (ha)"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 12),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
)


## Columns sum ----
(
  colsum_plot <- uc %>%
    left_join(
      base_grid,
      by = join_by(cell_id)
    ) %>%
    select(!geometry) %>%
    drop_na(group) %>%
    summarise(
      uc_area = sum(uc_area),
      .by = c("group", "region_name")
    ) %>%
    mutate(uc_area = as.numeric(uc_area) * 0.0001) %>%
    ggplot() +
    facet_wrap(facets = "region_name") +
    geom_col(
      aes(x = group, y = uc_area, group = group),
      color = "#000000",
      fill = "#e8e8e8"
    ) +
    scale_y_continuous(
      labels = label_number(scale_cut = cut_short_scale())
    ) +
    labs(
      title = "Conservation Units area by type",
      x = "Conservation Unit Type",
      y = "Area (ha)"
    ) +
    theme_minimal() +
    theme(text = element_text(size = 12))
)

# SAVE PLOTS ------------------------------------------------------------------

# Save time series plot
ggsave(
  filename = "./figs/conservation_units_timeseries.png",
  plot = timeseries_plot,
  device = ragg::agg_png,
  width = 15,
  height = 8,
  units = "cm",
  dpi = 300
)

# Save map plot
ggsave(
  filename = "./figs/conservation_units_map.png",
  plot = map_plot,
  device = ragg::agg_png,
  width = 15,
  height = 8,
  units = "cm",
  dpi = 300
)

# Save colsum plot
ggsave(
  filename = "./figs/conservation_units_colsum.png",
  plot = colsum_plot,
  device = ragg::agg_png,
  width = 15,
  height = 8,
  units = "cm",
  dpi = 300
)

# Save histogram plot
ggsave(
  filename = "./figs/conservation_units_hist.png",
  plot = hist_plot,
  device = ragg::agg_png,
  width = 15,
  height = 8,
  units = "cm",
  dpi = 300
)
