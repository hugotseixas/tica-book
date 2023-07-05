# HEADER ----------------------------------------------------------------------
#
# Title:          Create interview schedule
# Description:
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(conflicted)
library(gt)
library(tidyverse)
#
# CONFLICTS -------------------------------------------------------------------
#

conflicts_prefer(dplyr::filter)

#
# OPTIONS ---------------------------------------------------------------------
#

#
# LOAD INTERVIEW SCHEDULE TABLE -----------------------------------------------

read_delim("data/interview_schedule.csv", delim = ";") |>
  gt(groupname_col = c("section")) |>
  cols_hide(columns = c(id)) |>
  tab_options(
    table.font.size = 14,
    column_labels.hidden = TRUE,
    container.height = px(500)
  ) |>
  tab_style(
    style = list(
      cell_text(indent = pct(5))
    ),
    locations = cells_body(
      columns = c(schedule),
      rows = rationale %in% c("Probe")
    )
  ) |>
  tab_style(
    style = list(
      cell_text(style = "italic"),
      cell_borders(sides = c("top", "bottom"), style = "dashed")
    ),
    locations = cells_body(
      columns = c(schedule, rationale),
      rows = rationale %in% c("Transition")
    )
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#cc9e9e")
    ),
    locations = cells_body(
      columns = c(schedule, rationale),
      rows = rationale %in% c("Important check")
    )
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |>
  opt_stylize(style = 1, color = "gray") |>
  gtsave("figs/interview_schedule.html")
