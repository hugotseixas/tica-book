
library(gt)
library(tidyverse)

read_delim(file = "./data/glossary.csv") |>
  mutate(
    letter = str_sub(term, start = 1, end = 1)
  ) |>
  arrange(letter) |>
  gt(groupname_col = "letter") |>
  sub_missing() |>
  tab_options(
    table.font.size = 14,
    column_labels.hidden = TRUE
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(24)),
      cell_borders(sides = "bottom", color = NULL)
    ),
    locations = cells_row_groups()
  ) |>
  opt_stylize(
    style = 1,
    color = "gray"
  ) |>
  gtsave("figs/glossary.html")
