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
library(arrow)
library(glue)
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

## Create Histogram Plot ----

# Create plot
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

# Create Cumulative Plot ----

# Calculate some percentiles
quantile_table <- defo %>%
  drop_na() %>%
  mutate(area = area * 0.0001) %>%
  summarise(
    q01 = quantile(area, 0.01),
    q05 = quantile(area, 0.05),
    q25 = quantile(area, 0.25),
    q50 = quantile(area, 0.5),
    q75 = quantile(area, 0.75),
    q95 = quantile(area, 0.95),
    q99 = quantile(area, 0.99),
    q100 = quantile(area, 1)
  )

# Get cumulative sum for each percentile
cumsum_values <- defo %>%
  drop_na() %>%
  mutate(area = area * 0.0001) %>%
  arrange(area) %>%
  mutate(cumulative = cumsum(area/sum(area))) %>%
  inner_join(
    quantile_table %>%
      pivot_longer(everything()),
    by = join_by(closest(area >= value))
  ) %>%
  mutate(dif = area - value) %>%
  slice_min(order_by = dif, by = name) %>%
  mutate(
    cumulative = round(cumulative, digits = 2),
    area = round(area),
    quant = as.numeric(str_remove(name, "q"))
  ) %>%
  distinct(cumulative, area, quant)

# Create plot
(
  cumsum_plot <- defo %>%
    drop_na() %>%
    mutate(area = area * 0.0001) %>%
    arrange(area) %>%
    mutate(cumulative = cumsum(area/sum(area))) %>%
    ggplot(aes(x = area, y = cumulative)) +
    geom_step() +
    labs(
      title = "Cumulative Deforestation Area",
      x = "Area (ha)",
      y = "Cumulative Percentage"
    ) +
    geom_vline(
      xintercept = flatten_dbl(select(quantile_table, q01:q100)),
      linetype = 2
    ) +
    geom_label(
      data = cumsum_values,
      aes(label = glue("{quant}th")),
      alpha = 0.9
    ) +
    scale_x_log10(
      breaks = c(cumsum_values$area),
      labels = label_number(accuracy = 10),
      guide = guide_axis(angle = 55)
    ) +
    scale_y_continuous(
      labels = label_percent(),
      breaks = c(cumsum_values$cumulative, 1),
      guide = guide_axis(check.overlap = TRUE)
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 20),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
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
  plot = hist_plot,
  device = ragg::agg_png,
  width = 15,
  height = 10,
  units = "cm",
  dpi = 300
)
