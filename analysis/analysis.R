# Download data ----
tica::loop_function(
  function_name = "download_external_data",
  arguments_subset = 5
)

# Create analysis grid ----
aoi <-
  sf::read_sf("./data/external_raw/biomes/biomes_2019.fgb") |>
  dplyr::filter(cd_bioma %in% c(1, 3))

grid <-
  tica::create_grid(
    aoi = aoi,
    crs = glue::glue(
      'PROJCS["unnamed",GEOGCS["GCS_GRS_1980_IUGG_1980",DATUM["D_unknown",',
      'SPHEROID["GRS80",6378137,298.257222101]],PRIMEM["Greenwich",0],',
      'UNIT["Degree",0.0174532925199433]], PROJECTION["',
      'Albers_Conic_Equal_Area"],PARAMETER["latitude_of_center",-12], ',
      'PARAMETER["longitude_of_center",-54],',
      'PARAMETER["standard_parallel_1",-2],',
      'PARAMETER["standard_parallel_2",-22],',
      'PARAMETER["false_easting",5000000],',
      'PARAMETER["false_northing",10000000],',
      'UNIT["metre",1,AUTHORITY["EPSG","9001"]], AXIS["Easting",EAST],',
      'AXIS["Northing",NORTH]]'
    ),
    resolution = 40000,
    full_cells = TRUE,
    shape = "hex"
  )

# Process data ----
tica::loop_function(
  function_name = "preprocess_external_data",
  arguments_subset = 1:2
)
