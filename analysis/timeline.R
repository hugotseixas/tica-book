
library(cowplot)
library(gt)
library(tidyverse)

table <-
  read_csv("./data/external/policies/policies.csv") |>
  mutate(year = year(start_date)) |>
  nest(.by = year) |>
  mutate(
    position = rep(c(1.0, -1.0, 1.5, -1.5, 2.0, -2.0), length.out = n()),
    direction = rep(c(1, -1), length.out = n())
  ) |>
  unnest(cols = data) |>
  mutate(
    name = paste(name, collapse = "\n"),
    .by = "year"
  )

plot <- table |>
  ggplot() +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_segment(
    data = distinct(table, year, .keep_all = TRUE),
    aes(x = year, y = position, xend = year, yend = 0),
    linetype = "dashed"
  ) +
  geom_point(
    data = distinct(table, year, .keep_all = TRUE),
    aes(x = year, y = 0),
    size = 3
  ) +
  geom_text(
    data = distinct(table, year, .keep_all = TRUE),
    aes(
      x = year,
      y = 0.2 * direction,
      label = format(year, format = "%Y")
    ),
    vjust = -0.5
  ) +
  geom_label(
    data = distinct(table, year, .keep_all = TRUE),
    aes(x = year, y = position, label = name),
    label.padding = unit(0.45, "lines"),
    label.size = 0.7,
    size = 3,
    fill = "#CCCCCC"
  ) +
  coord_flip(ylim = c(-2.6, 2.6)) +
  scale_x_reverse() +
  theme_nothing()

obj_name <- "pp_timeline"

assign(obj_name, plot)

save(
  list = obj_name,
  file = glue::glue("figs/eda/pp_timeline.rdata")
)

read_csv("./data/external/policies/policies.csv") |>
  mutate(year = year(start_date)) |>
  gt(groupname_col = c("year")) |>
  sub_missing() |>
  cols_hide(columns = c("type", "identification", "url")) |>
  tab_options(
    table.font.size = 14,
    container.height = px(500)
  ) |>
  cols_label(
    name = md(""),
    start_date = md("**Start Date**"),
    end_date = md("**End Date**"),
    coverage = md("**Coverage**"),
    description = md("**Description**")
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#EEEEEE"),
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  ) |>
  opt_stylize(
    style = 1,
    color = "gray",
    add_row_striping = FALSE
  ) |>
  gtsave("figs/policies_table.html")
