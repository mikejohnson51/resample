#' @title Raster from Vector
#' @description This function maps a vector to an `sf` grid and then to a raster
#' @param output An `sf` grid to be resampled to
#' @return a single layer `raster`
#' @importFrom raster raster extent values
#' @importFrom sf st_crs
#' @export

raster.from.vector = function(output){

  # Build Matrix from Cat vector
  v = matrix(output$cat, nrow = max(output$row_id), byrow = TRUE)

  # Define Raster from output object
  r = raster(ext = raster::extent(output), nrow = max(output$row_id),
             ncol = max(output$col_id), crs = st_crs(output)$proj4string)

  # Horizontially flip matrix
  r[] = apply(v, 2, rev)

  # Return
  return(r)
}












