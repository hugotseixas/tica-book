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
library(arrow)
library(scales)
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

defo <- read_parquet("data/deforestation.parquet")

# EXPLORE DATA ----------------------------------------------------------------

(
  hist_plot <- defo %>%
    drop_na() %>%
    mutate(area = area * 0.0001) %>%
    filter(
      # Remove deforestation smaller than 0.1 ha to improve visualization
      area > 0.1
    ) %>%
    ggplot() +
    geom_histogram(
      aes(x = area, y = after_stat(count)),
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
      text = element_text(size = 20),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
)

(
  box_plot <- defo %>%
    drop_na() %>%
    mutate(area = area * 0.0001) %>%
    filter(
      # Remove deforestation smaller than 0.1 ha to improve visualization
      area > 0.1
    ) %>%
    ggplot() +
    geom_boxplot(
      aes(x = area)
    ) +
    scale_x_log10(
      breaks = log_breaks(6),
      labels = label_number(),
      expand = c(0, 0)
    ) +
    labs(
      title = "Deforestation Histogram",
      x = "Area (ha)"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 20),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
)

defo %>%
  drop_na() %>%
  mutate(area = area * 0.0001) %>%
  summarise(
    q00 = quantile(area, 0.01),
    q25 = quantile(area, 0.25),
    q50 = quantile(area, 0.5),
    q75 = quantile(area, 0.75),
    q100 = quantile(area, 0.99),
    total = sum(area)
  )

# SAVE PLOTS ------------------------------------------------------------------

ggsave(
  filename = "./figs/deforestation_hist.png",
  plot = hist_plot,
  device = ragg::agg_png,
  width = 15,
  height = 7,
  units = "cm",
  dpi = 300
)

