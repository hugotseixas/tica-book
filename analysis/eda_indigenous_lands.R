# HEADER ----------------------------------------------------------------------
#
# Title:          Exploratory Analysis for Indigenous Lands Data
# Description:
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(conflicted)
library(sf)
library(arrow)
library(scales)
library(scico)
library(tidyverse)
library(tica)
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

aoi <-
  smoothr::smooth(
    st_simplify(aoi, dTolerance = 5000),
    method = "ksmooth",
    smoothness = 3
  )

base_grid <- read_sf("data/base_grid.fgb")

il <- read_parquet(file = "data/indigenous_lands.parquet")

