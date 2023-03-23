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

defo %>%
  drop_na() %>%
  mutate(area = area * 0.0001) %>%
  ggplot() +
  geom_histogram(
    aes(x = area, y = after_stat(count)),
    color = "#000000",
    fill = "#e8e8e8",
    bins = 50
  ) +
  scale_x_log10(
    breaks = log_breaks(),
    labels = label_number()
  ) +
  labs(x = "Area (ha)", y = "Count") +
  theme_minimal() +
  theme(
    text = element_text(size = 20)
  )
