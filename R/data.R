#' A raster file
#'
#' A raster file covering an area of 60x60 cells. The raster file contains 5 layers - a high resolution (30m) race-specific grids with values of subpopulation densities for Asian, Black, Hispanic, other and Whites.
#' `system.file("extdata/race_raster.tif", package = "raceland")`
#'
#' @format A raster file
#' @name race_raster.tif
NULL

#' An sf object
#'
#' It is an sf POLYGON object with census block-level data. It consists of 7 variables: GISJOIN - block ID, ASIAN, BLACK, HISPANIC, OTHER, WHITE - number of people of given race/ethnicity in each block
#'
#' @format An sf object
"pop_vector"
