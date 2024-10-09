# LOAD DATA -------------------------------------------------------------------

base_dir <- here::here()

# Load biomes limit
biomes <-
  sf::read_sf("./data/raw/biomes/biomes_2019.fgb") |>
  dplyr::filter(name_biome %in% c("Amazônia", "Cerrado")) |>
  dplyr::select(name_biome) |>
  sf::st_transform(
    sf::st_crs(
      readr::read_file(glue::glue("{base_dir}/data/project_crs.txt"))
    )
  ) |>
  sf::st_make_valid() |>
  sf::st_simplify(dTolerance = 10000) |>
  sf::st_set_agr("constant")

shifted_biomes <- biomes |>
  dplyr::filter(name_biome == "Cerrado") |>
  dplyr::mutate(geometry = geometry + c(600000, -600000)) |>
  sf::st_set_crs(
    sf::st_crs(
      readr::read_file(glue::glue("{base_dir}/data/project_crs.txt"))
    )
  ) |>
  dplyr::bind_rows(
    biomes |> dplyr::filter(name_biome == "Amazônia")
  ) |>
  sf::st_set_agr("constant")

# CREATE AND SAVE GRID --------------------------------------------------------

# Create grid based on biomes polygons
full_grid <-
  sf::st_make_grid(
    x = shifted_biomes,
    square = FALSE,
    cellsize = 375000 # Resolution of the grid cell
  ) |>
  sf::st_as_sf(agr = "constant") |>
  sf::st_set_geometry("geometry") |>
  #sf::st_filter(shifted_biomes, .predicate = sf::st_within) |>
  sf::st_join(shifted_biomes, left = FALSE, largest = TRUE)

shifted_grid <- full_grid |>
  dplyr::filter(name_biome == "Cerrado") |>
  dplyr::mutate(geometry = geometry + c(-450000, 100000)) |>
  sf::st_set_crs(
    sf::st_crs(
      readr::read_file(glue::glue("{base_dir}/data/project_crs.txt"))
    )
  ) |>
  dplyr::bind_rows(
    full_grid |> dplyr::filter(name_biome == "Amazônia")
  ) |>
  sf::st_difference() |>
  dplyr::mutate(area = as.numeric(sf::st_area(geometry))) |>
  dplyr::filter(area >= max(area) - 100) |>
  dplyr::select(!area) |>
  dplyr::mutate(
    id = dplyr::row_number(),
    .by = "name_biome"
  )

# CREATE AND SAVE PLOT --------------------------------------------------------

photos <-
  purrr::map2(
    .x = c("Amazônia", "Cerrado"),
    .y = c("amazon", "cerrado"),
    .f = ~ {

      img <-
        readr::read_table(
          file = glue::glue("./figs/art/url_{.y}.txt"),
          col_names = "url",
          col_types = "c"
        ) |>
        dplyr::mutate(
          name_biome = .x
        ) |>
        dplyr::mutate(
          id = dplyr::row_number(),
          .by = "name_biome"
        )

    }
  ) |>
  purrr::list_rbind()

fig_data <- shifted_grid |>
  dplyr::inner_join(photos, by = dplyr::join_by(name_biome, id))

# Plot grid and biome limits
grid_plot <- ggplot2::ggplot() +
  ggfx::with_outer_glow(
    ggpattern::geom_sf_pattern(
      data = fig_data,
      pattern_filename = fig_data$url,
      pattern = "image",
      pattern_type = "expand"
    ),
    sigma = 10,
    expand = 20
  ) +
  ggplot2::geom_sf(
    data = shifted_grid,
    color = "#FFFFFF",
    fill = "transparent",
    lwd = 0.7
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = ""
  )

# Save plot
ggplot2::ggsave(
  filename = "./figs/art/art.png",
  plot = grid_plot,
  device = ragg::agg_png,
  width = 10,
  height = 10,
  units = "cm",
  dpi = 600
)
