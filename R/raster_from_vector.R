#' @title Raster from Vector
#' @description This function maps a vector to an `sf` grid and then to a raster
#' @param vec A vector of data
#' @param output An `sf` grid to be resampled to
#' @return a single layer `raster`
#' @importFrom raster raster extent crs
#' @importFrom sf st_crs
#' @export

raster.from.vector = function(vec, output){

  m = matrix(vec, ncol = max(output$col_id), byrow = TRUE)

  dist = apply(m, 2, rev)

  r = raster(dist)
  raster::extent(r) = raster::extent(output)
  raster::crs(r) = sf::st_crs(output)[2]$proj4string

  return(r)
}












