if(system.file(package = "pacman") == ""){
  install.packages("pacman")
}

pacman::p_load(
  conflicted,
  tidyverse,
  targets,
  tarchetypes,
  here,
  readxl,
  janitor,
  googledrive
)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(spatstat.geom::area)
conflicts_prefer(dplyr::lag)
