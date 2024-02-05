# HEADER ----------------------------------------------------------------------
#
# Title:          Process Data
# Description:    This process data to fit the base grid
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(tica)
library(tidyverse)
#
# CONFLICTS -------------------------------------------------------------------
#

#
# OPTIONS ---------------------------------------------------------------------
#

#
# PROCESS DATA ----------------------------------------------------------------

arguments <- read_delim("./analysis/process_data_arguments.txt", delim = ",")

pwalk(
  .l = arguments[4, ],
  .f = \(
    f,
    base_grid_path,
    external_data_path,
    dest_dir,
    start_year,
    end_year
  ) {

    process_external_data(
      f = f,
      base_grid_path = base_grid_path,
      external_data_path = external_data_path,
      dest_dir = dest_dir,
      timespan = start_year:end_year
    )

  }
)
