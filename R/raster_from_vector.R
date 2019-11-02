#' @title Raster from Vector
#' @description This function maps a vector to an `sf` grid and then to a raster
#'
#' @param vec A vector of data
#' @param output.grid An `sf` object
#' @return a single layer `raster`
#' @export

#vec = cat
raster.from.vector = function(vec, output){
 
  x = max(output$col_id)
  m = matrix(vec, ncol = x, byrow = T )
  m = apply(m, 2, rev)
  dist = raster(m)

  extent(dist) = extent(output)
  crs(dist) = st_crs(output)[2]$proj4string
  return(dist)
}












