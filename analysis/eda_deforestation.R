# HEADER ----------------------------------------------------------------------
#
# Title:          EDA for deforestation variable
# Description:    This script explores deforestation data from PRODES
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
library(tidyverse)
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

defo <-
  read_parquet("data/deforestation.parquet") %>%
  summarise(
    deforestation_area = sum(deforestation_area),
    .by = c("year", "cell_id")
  )

base_grid <- read_sf("data/base_grid.fgb")

# EXPLORE DATA ----------------------------------------------------------------

## Create Histogram Plot ----

# Create plot
(
  hist_plot <- defo %>%
    drop_na() %>%
    mutate(deforestation_area = deforestation_area * 0.0001) %>%
    ggplot() +
    geom_histogram(
      aes(x = deforestation_area, y = after_stat(count)),
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
      title = "Deforestation Histogram",
      x = "Area (ha)"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 18),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
)

## Create Cumulative Plot ----

# Calculate some percentiles
quantile_table <- defo %>%
  drop_na() %>%
  #mutate(deforestation_area = deforestation_area * 0.0001) %>%
  summarise(
    #q01 = quantile(deforestation_area, 0.01),
    q05 = quantile(deforestation_area, 0.05),
    q25 = quantile(deforestation_area, 0.25),
    q50 = quantile(deforestation_area, 0.5),
    q75 = quantile(deforestation_area, 0.75),
    q95 = quantile(deforestation_area, 0.95),
    q99 = quantile(deforestation_area, 0.99),
    q100 = quantile(deforestation_area, 1)
  )

# Get cumulative sum for each percentile
cumsum_values <- defo %>%
  drop_na() %>%
  #mutate(deforestation_area = deforestation_area * 0.0001) %>%
  arrange(deforestation_area) %>%
  mutate(
    cumulative = cumsum(
      deforestation_area/sum(deforestation_area)
    )
  ) %>%
  inner_join(
    quantile_table %>%
      pivot_longer(everything()),
    by = join_by(closest(deforestation_area >= value))
  ) %>%
  mutate(dif = deforestation_area - value) %>%
  slice_min(order_by = dif, by = name) %>%
  mutate(
    cumulative = round(cumulative, digits = 2),
    deforestation_area = round(deforestation_area),
    quant = as.numeric(str_remove(name, "q"))
  ) %>%
  distinct(cumulative, deforestation_area, quant)

# Create plot
(
  cumsum_plot <- defo %>%
    drop_na() %>%
    #mutate(deforestation_area = deforestation_area * 0.0001) %>%
    arrange(deforestation_area) %>%
    mutate(
      cumulative = cumsum(deforestation_area/sum(deforestation_area)),
      quant = case_when(
        deforestation_area <= quantile_table[[1]] ~ "a",
        deforestation_area <= quantile_table[[2]] ~ "b",
        deforestation_area <= quantile_table[[3]] ~ "c",
        deforestation_area <= quantile_table[[4]] ~ "d",
        deforestation_area <= quantile_table[[5]] ~ "e",
        deforestation_area <= quantile_table[[6]] ~ "f",
        deforestation_area <= quantile_table[[7]] ~ "g"
      )
    ) %>%
    ggplot(aes(x = deforestation_area, y = cumulative)) +
    geom_ribbon(
      aes(
        ymin = 0,
        ymax = cumulative,
        fill = quant
      ),
      alpha = 0.7
    ) +
    geom_step(
      linewidth = 0.5
    ) +
    annotate(
      geom = "segment",
      x = cumsum_values$deforestation_area,
      xend = cumsum_values$deforestation_area,
      y = 0,
      yend = cumsum_values$cumulative,
      linetype = 2
    ) +
    geom_label(
      data = cumsum_values,
      aes(label = glue("{quant}th")),
      alpha = 0.9,
      size = 3
    ) +
    scale_x_log10(
      breaks = c(cumsum_values$deforestation_area),
      labels = label_number(accuracy = 1, scale = 0.0001),
      guide = guide_axis(angle = 55)
    ) +
    scale_y_continuous(
      labels = label_percent(),
      breaks = c(cumsum_values$cumulative, 1),
      guide = guide_axis(check.overlap = TRUE)
    ) +
    scale_fill_scico_d(palette = "bilbao") +
    labs(
      title = "Cumulative Deforestation Area",
      x = "Area (ha)",
      y = "Cumulative Deforestation Percentage"
    ) +
    theme_minimal() +
    theme(
      legend.position = "",
      text = element_text(size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
)

## Spatial distribution ----
(
  map_plot <- defo %>%
    drop_na() %>%
    summarise(
      deforestation_area = sum(deforestation_area),
      .by = c("cell_id", "year")
    ) %>%
    arrange(cell_id, year) %>%
    mutate(deforestation_area = deforestation_area * 0.0001) %>%
    filter(year %in% c(2020, 2021)) %>%
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
        fill = deforestation_area
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
)

## Create Time Series Plot ----

defo %>%
  left_join(
    base_grid,
    by = join_by(cell_id)
  ) %>%
  as_tibble() %>%
  mutate(
    date = ymd(year, truncated = 2),
    deforestation_area = deforestation_area * 0.0001
  ) %>%
  summarise(
    deforestation_area = sum(deforestation_area),
    .by = c("date", "region_name")
  ) %>%
  ggplot() +
  facet_wrap(facets = "region_name") +
  geom_col(
    aes(
      x = date,
      y = deforestation_area,
      fill = deforestation_area
    )
  ) +
  scale_fill_scico(
    palette = "bilbao",
    labels = label_number(),
    trans = "log10"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 15)
  )

# SAVE PLOTS ------------------------------------------------------------------

# Save histogram plot
ggsave(
  filename = "./figs/deforestation_hist.png",
  plot = hist_plot,
  device = ragg::agg_png,
  width = 15,
  height = 7,
  units = "cm",
  dpi = 300
)

# Save cumulative plot
ggsave(
  filename = "./figs/deforestation_cumsum.png",
  plot = cumsum_plot,
  device = ragg::agg_png,
  width = 15,
  height = 11,
  units = "cm",
  dpi = 300
)

# Save map plot
ggsave(
  filename = "./figs/deforestation_map.png",
  plot = map_plot,
  device = ragg::agg_png,
  width = 15,
  height = 15,
  units = "cm",
  dpi = 300
)
