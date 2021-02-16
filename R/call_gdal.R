#' @title Call GDAL
#' @param input input grid (raster)
#' @param method resampling method
#' @param cellsize output cell diminsion
#' @keywords internal
#' @return RasterLayer
#' @importFrom raster writeRaster raster
#' @importFrom sf gdal_utils

call_gdal = function(input, method, cellsize){

  tmp = tempfile(pattern = "input", fileext = '.tif')
  nn  = tempfile(pattern = "nn",    fileext = '.tif')

  writeRaster(input, filename = tmp, overwrite = TRUE)

  sf::gdal_utils("translate",
                 source      = tmp,
                 destination = nn,
                 options = c("-r", method, "-tr", c(cellsize,cellsize)))

  rr = raster(nn)

  file.remove(tmp)

  rr

}
